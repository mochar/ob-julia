;; -*- lexical-binding: t -*-

(require 'ob-julia)
(require 'julia-snail nil t)

(declare-function julia-snail "julia-snail")
(declare-function julia-snail--send-to-server "julia-snail")
(defvar julia-snail-repl-buffer)
(declare-function julia-snail--request-tracker-originating-buf "julia-snail")
(declare-function julia-snail--request-tracker-display-error-buffer-on-failure? "julia-snail")
(declare-function julia-snail--request-tracker-tmpfile "julia-snail")

(defvar org-babel-julia-snail-port-counter 10050
  "Counter for dynamically allocating Snail ports.")

(cl-defmethod org-babel-julia-prepare-format-call
    ((_ (eql 'julia-snail)) src-file out-file params &optional uuid)
  "Format a call to OrgBabelEval

OrgBabelEval is the entry point of the Julia code defined in
the startup script."
  (format
   ;; "Main.JuilaSnail.Extensions.ObJulia.OrgBabelEval(%S,%S,%S,%s;print_output=false);"
   "ObJulia.OrgBabelEval(%S,%S,%S,%s;print_output=false,catch_errors=false);"
   src-file out-file (org-babel-julia-params->named-tuple params)
   (or (when uuid (format "%S" uuid)) "nothing")))

(cl-defmethod org-babel-julia-prep-session ((_ (eql 'julia-snail)) session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (let ((dir (or (alist-get :dir params) default-directory))
        (repl-buffer (org-babel-julia-get-session-name params)))
    (save-window-excursion

      ;; Only spawn a new Snail process if the REPL buffer doesn't already exist
      (unless (get-buffer repl-buffer)
        (setq org-babel-julia-snail-port-counter (1+ org-babel-julia-snail-port-counter))

        ;; Julia-snail stores a buffer-local var in the repl buffer named
        ;; julia-snail--repl-go-back-target that points to the buffer where the
        ;; repl was created. Since it is assumed that this buffer is alive in
        ;; various places in julia-snail (namely when creating multimedia
        ;; buffer), we generate a hidden buffer for it, and kill it when the
        ;; repl is killed.
        (let ((origin-buf (generate-new-buffer (format " *ob-julia-origin-%s*" repl-buffer))))
          (with-current-buffer origin-buf
            (setq default-directory dir)
            ;; Prevent julia-snail from crashing when checking file-remote-p
            (setq buffer-file-name (or (buffer-file-name) (expand-file-name "dummy.org" dir)))
            (setq-local julia-snail-repl-buffer repl-buffer)
            (setq-local julia-snail-port org-babel-julia-snail-port-counter)
            (julia-snail))

          (with-current-buffer (get-buffer repl-buffer)
            (add-hook 'kill-buffer-hook
                      (lambda ()
                        (when (buffer-live-p origin-buf)
                          (kill-buffer origin-buf)))
                      nil t))))
      
      (message "Loading ObJulia...")
      (julia-snail--send-to-server
        '("Main")
        (format "include(\"%s\")" ob-julia-startup-script)
        :repl-buf (get-buffer repl-buffer)
        :async nil)
      (message "Loading ObJulia... done")
      repl-buffer)))

(cl-defmethod org-babel-julia--get-live-session
  (session &context (org-babel-julia-backend (eql 'julia-snail)))
  (and-let*
      ((repl-buffer (get-buffer session)) 
       ((buffer-live-p repl-buffer))
       ((buffer-local-value 'julia-snail-repl-mode repl-buffer)))
    repl-buffer))

(defun org-babel-julia-snail--ensure-module (session params)
  "Prompt to create the module specified in PARAMS if it does not exist."
  (let ((module (alist-get :module params)))
    (when (and module (not (string= module "Main")) (not (string= module "none")))
      (let* ((repl-buffer (get-buffer session))
             ;; Query Snail synchronously to see if the module exists.
             ;; Returns t if exists, otherwise returns :nothing 
             (exists (julia-snail--send-to-server
                       :Main
                       (format "isdefined(Main, :%s) && isa(getfield(Main, :%s), Module)" module module)
                       :repl-buf repl-buffer
                       :async nil)))
        (unless (eq exists t) ; Explicit because returns :nothing if not exists
          (if (yes-or-no-p (format "Module '%s' does not exist in Main. Create it? " module))
              ;; Create the module synchronously
              (julia-snail--send-to-server
                :Main
                (format "module %s end" module)
                :repl-buf repl-buffer
                :async nil)
            ;; Abort execution if the user says no
            (user-error "Evaluation aborted: Module '%s' does not exist." module)))))))

(cl-defmethod org-babel-julia-evaluate-in-session:sync
  ((_ (eql 'julia-snail)) session OrgBabelEval-call _ output-file params)
  "Run ORGBABELEVAL-CALL in session SESSION synchronously with julia-snail."
  (when-let ((mime-type
              (julia-snail--send-to-server
                '("Main")
                OrgBabelEval-call
                :repl-buf session
                :async nil
                :display-error-buffer-on-failure? t)))
    ;; Rename the output file heuristically by mime-type
    (setq output-file (org-babel-julia--maybe-rename-output output-file mime-type params))
    (if (file-exists-p output-file)
        output-file
      (error "No output produced."))))

(cl-defmethod org-babel-julia-evaluate-in-session:async
  ((_ (eql 'julia-snail)) session uuid OrgBabelEval-call _ output properties)
  "Run ORGBABELEVAL-CALL in session SESSION asynchronously with julia-snail."
  (org-babel-julia-snail--ensure-module session (car properties))
  (let ((reqid 
         (julia-snail--send-to-server
           '("Main") ;TODO: Use `julia-snail--module-at-point'
           OrgBabelEval-call
           :repl-buf session
           :async t
           :display-error-buffer-on-failure? t
           :callback-success #'org-babel-julia-snail-success-callback
           ;; Currently never called:
           :callback-failure #'org-babel-julia-snail-failure-callback)))
    (org-babel-julia--async-add uuid properties)
    (concat "julia-async:" uuid)))

(defun org-babel-julia-snail-success-callback (request-info result-data)
  "A function that is called when julia-snail response is available."
  (if (not result-data)
      (message "Code block produced no output.")
    (pcase-let ((`(,uuid-string . ,mime-type) (read result-data)))
      (if (string-match ".*ob_julia_async_\\([0-9a-z\\-]+\\).*" uuid-string)
          (let* ((uuid (match-string-no-properties 1 uuid-string))
                 (org-buffer (julia-snail--request-tracker-originating-buf request-info))
                 (display-errors (julia-snail--request-tracker-display-error-buffer-on-failure?
                                  request-info))
                 (properties (org-babel-julia--async-get-remove uuid))
                 (vals (cdr properties))
                 (params (elt vals 0))
                 (output-file (elt vals 1))
                 ;; (org-buffer (elt vals 2))
                 (src-file (elt vals 3)))
            (unwind-protect
                (progn
                  ;; Rename the output file heuristically by mime-type
                  (setq output-file
                        (org-babel-julia--maybe-rename-output output-file mime-type params))
                  (org-babel-julia--place-result output-file org-buffer uuid params))
              (when (and src-file (file-exists-p src-file))
                (delete-file src-file))))))))

(defun org-babel-julia--maybe-rename-output (output-file mime-type params)
  "Possibly rename OUTPUT-FILE with a more suitable extension.

MIME-TYPE is chosen by ObJulia. PARAMS is the list of block
parameters.
 
ObJulia can pick a mime-type better suited to the type of result
generated - for instance, png when writing a GR plot object.
Unless an output file is explicitly specified with the header arg
`:file', we rename the output file to a more suitable extension."
  (if-let* (((not (alist-get :file params)))
            (required-ext (alist-get mime-type org-babel-julia-mimes->exts
                                     nil nil #'equal))
            ((not (string= (file-name-extension output-file) required-ext)))
            (new-output-file (concat (file-name-sans-extension output-file)
                                     "." required-ext)))
      (progn (rename-file output-file new-output-file 'force)
             new-output-file)
    output-file))

;; NOTE: because we catch errors in ObJulia this is never actually called.
;;
;; TODO: Provide an option to not catch errors when using julia-snail?
;; julia-snail's error reporting is pretty slick.
;; NOTE: In that event, we can't access the UUID! Need to think more about this.
(defun org-babel-julia-snail-failure-callback (request-info)
  (when-let ((tmpfile (julia-snail--request-tracker-tmpfile request-info)))
    (and (file-exists-p tmpfile) (delete-file tmpfile))))

;;; Org interaction

(defun org-babel-julia--session-and-module-at-point (&optional info)
  (when-let* ((info (or info (org-babel-get-src-block-info 'no-eval)))
              (_ (string-equal (nth 0 info) "julia"))
              (params (nth 2 info))
              (session (org-babel-julia-get-session-name params)))
    (cons session (alist-get :module params))))

(defun org-babel-julia-doc-lookup ()
  (interactive)
  (pcase-let* ((`(,session . ,module) (org-babel-julia--session-and-module-at-point))
               (julia-snail-repl-buffer session))
    (call-interactively #'julia-snail-doc-lookup)))

(defun org-babel-julia--src-setup ()
  (when (and (eq major-mode 'julia-mode)
             (boundp 'org-src--babel-info))
    (pcase-let* ((`(,session . ,module) (org-babel-julia--session-and-module-at-point org-src--babel-info)))
      (setq-local julia-snail-repl-buffer session))))

(defvar-keymap org-babel-julia-mode-map
  :doc "Keymap for org-babel-julia-mode."
  "M-i" #'org-babel-julia-doc-lookup)
  

(define-minor-mode org-babel-julia-mode
  "Minor mode for interacting with a Julia REPL from an `org-mode' buffer."
  :group 'org-babel-julia
  :init-value nil
  :keymap org-babel-julia-mode-map
  (cond
   (org-babel-julia-mode
    (add-hook 'after-revert-hook 'org-babel-julia-mode nil t)
    ;; (add-hook 'completion-at-point-functions 'jupyter-org-completion-at-point nil t)
    (add-hook 'org-src-mode-hook 'org-babel-julia--src-setup)
    )
   (t
    (remove-hook 'after-revert-hook 'org-babel-julia-mode t)
    ;; (remove-hook 'completion-at-point-functions 'jupyter-org-completion-at-point t)
    (remove-hook 'org-src-mode-hook 'org-babel-julia--src-setup)
    )))

;;; Override module-at-point

;; We allow code blocks to be evaluated within modules using the :module
;; param. For julia-snail to be aware of this, we replace its `module-at-point'
;; function with one that first checks if we are in Org-mode or an Org-src
;; buffer.

(defun org-babel-julia-snail--module-at-point-advice (orig-fun &optional partial-module)
  "Advice for `julia-snail--module-at-point` to support Org Babel and ephemeral buffers."
  (let* ((base-module
          (cond
           ;; Inside org-edit-special (C-c ')
           ((bound-and-true-p org-src-mode)
            (when-let* ((info (bound-and-true-p org-src--babel-info))
                        (params (nth 2 info)))
              (alist-get :module params)))
           
           ;; Inside an inline org-mode julia block
           ((eq major-mode 'org-mode)
            (cdr (org-babel-julia--session-and-module-at-point)))
           
           ;; Fallback sentinel
           (t :use-orig))))
    
    (if (eq base-module :use-orig)
        ;; We are outside of Org. Delegate to the original function.
        ;; We still check for a file name to prevent the `expand-file-name` crash.
        (if (buffer-file-name (buffer-base-buffer))
            (funcall orig-fun partial-module)
          (or partial-module '("Main")))
      
      ;; Org context intercepted: build the namespace from the block header
      (or (if base-module
              (append `(,base-module) partial-module)
            partial-module)
          '("Main")))))

;; Apply the advice
(advice-add 'julia-snail--module-at-point :around #'org-babel-julia-snail--module-at-point-advice)

;;; Footer

(provide 'ob-julia-snail)
