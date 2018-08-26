;; -*- lexical-binding: t -*-
(defconst EMACS26+
  (eval-when-compile (not (version< emacs-version "26"))))
(defconst EMACS27+
  (eval-when-compile (not (version< emacs-version "27"))))

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WIN (memq system-type '(cygwin windows-nt ms-dos)))

(defvar moon-emacs-d-dir (expand-file-name user-emacs-directory)
  "Basically ~/.emacs.d but a full path.")

(defvar moon-core-dir (concat moon-emacs-d-dir "core/")
  "Where core is located.")

(defvar moon-star-dir (concat moon-emacs-d-dir "star/")
  "Where stars shine.")

(defvar moon-local-dir (concat moon-emacs-d-dir ".local/")
  "Where package and other stuff goes. For files that are useful across sessions.")

(defvar moon-cache-dir (concat moon-local-dir ".cache/")
  "Where tmp files rest. For files that are dedicated to each session.")

(defvar moon-init-time nil
  "How long it takes for Emacs to start.")

(defvar lunary-version "1.0.0"
  "Current version of lunarymacs.")


(defvar moon-init-hook ()
  "A list of hooks run when Emacs is initialized, before `moon-post-init-hook'.")

(defvar moon-post-init-hook ()
  "A list of hooks that run after Emacs initialization is complete, and after `moon-init-hook'.")


;;
;; Config
;;

(dolist (dir (list moon-local-dir moon-cache-dir))
        (unless (file-directory-p dir)
          (make-directory dir t)))

(setq package-enable-at-startup nil)
(setq custom-file (concat moon-local-dir "custom.el"))
;(load custom-file)
(when (file-exists-p custom-file)
  (load custom-file))


;;
;; Func
;;

(defmacro timeit| (message &rest rest)
  "Time the execution of forms (REST) and print MESSAGE."
  (declare (indent 1))
  `(let ((start-time (current-time)))
     ,@rest
     (message (format "%s time: %.03f" ,message (float-time (time-subtract (current-time) start-time))))
    ))



;;
;; Init
;;

;; optimization on startup
;; https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast
(defvar tmp-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(load (concat moon-core-dir "core-package"))
(load| core-ui)
(load| core-edit)

(defun moon-finalize ()
  "The main starup function."
  (timeit| "package-init"
    (moon-initialize-load-path))
  (unless noninteractive
    (moon-load-star))
  
  (unless noninteractive
    (dolist (hook '(moon-init-hook moon-post-init-hook))
    (run-hook-with-args hook)))
  
  ;; If you forget to reset this, you'll get stuttering and random freezes!
  (setq gc-cons-threshold 800000
        gc-cons-percentage 0.1
        file-name-handler-alist tmp-file-name-handler-alist
        ))

(add-hook 'emacs-startup-hook #'moon-finalize t)


(provide 'core)

