;;(package-initialize t)
(defvar moon--file-name-handler-alist file-name-handler-alist)

(unless after-init-time
  ;; A big contributor to long startup times is the garbage collector, so we
  ;; up its memory threshold, temporarily and reset it later in
  ;; `moon|finalize'.
  (setq gc-cons-threshold 402653184
        gc-cons-percentage 1.0
        ;; consulted on every `require', `load' and various file reading
        ;; functions. You get a minor speed up by nooping this.
        file-name-handler-alist nil))
		
(defun moon-finalize ()
  "The main starup function."
  (timeit| "package-init"
    (moon-initialize-load-path))
  (unless noninteractive
    (moon-load-star))
  
  (unless noninteractive
    (dolist (hook '(moon-init-hook moon-post-init-hook))
    (run-hook-with-args hook)))
  
  (setq file-name-handler-alist moon--file-name-handler-alist
        gc-cons-threshold 16777216
        gc-cons-percentage 0.15)
		)
(add-hook 'emacs-startup-hook #'moon-finalize t)


		
(load (concat (expand-file-name user-emacs-directory) "core/core"))

(moon| 
       :feature
       key
       evil
       ui
       edit
       project
	   
       :completion
       ivy
       company
       snippet
	   
	   :os
	   mac
	   
       :utility
       org
	   
       :checker
       syntax
       ; spell
	   
       :lang
       cc
       lsp
       python
       elisp
       javascript
       web
       )


;;
;;; Settings evaluate befor loading any stars i.e. user-init
;;

