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

(when (file-exists-p custom-file)
  (load custom-file))

(add-hook 'kill-emacs-query-functions
               'custom-prompt-customize-unsaved-options)
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

(load (concat moon-core-dir "core-package"))
(load| core-ui)
(load| core-edit)
(load| core-keybinds)


(add-to-list 'load-path moon-core-dir)
(require 'core-lib)

(fset #'yes-or-no-p #'y-or-n-p) ; y/n instead of yes/no

(setq-default
 ad-redefinition-action              'accept                               ; silence advised function warnings
 apropos-do-all                      t                                     ; make `apropos' more useful
 compilation-always-kill             t                                     ; kill compilation process before starting another
 compilation-ask-about-save          nil                                   ; save all buffers on `compile'
 confirm-nonexistent-file-or-buffer  t
 enable-recursive-minibuffers        nil
 idle-update-delay                   2                                     ; update ui less often

 ;; keep the point out of the minibuffer
 minibuffer-prompt-properties        '(read-only
                                       t
                                       point-entered
                                       minibuffer-avoid-prompt
                                       face
                                       minibuffer-prompt)

 ;; History & backup settings (save nothing, that's what git is for)
 create-lockfiles                    nil
 history-length                      500
 make-backup-files                   nil
 auto-save-default                   t
 backup-directory-alist              `((".*" . ,moon-cache-dir))
 auto-save-file-name-transforms      `((".*" ,moon-cache-dir t))
 auto-save-list-file-name            (concat moon-cache-dir "autosave")
 auto-save-timeout                   5

 ;; files
 abbrev-file-name                    (concat moon-local-dir "abbrev.el")
 recentf-save-file                   (concat moon-cache-dir "recentf")
 recentf-max-saved-items             300
 
 ;; edit
 indent-tabs-mode                    nil
 backup-inhibited                    t
 sentence-end-double-space           nil
 kill-ring-max                       200

 ;;ui
 use-dialog-box                      nil
 visible-cursor                      nil
 use-dialog-box                      nil
 ring-bell-function                  #'ignore
 visible-bell                        nil
 frame-title-format                  '("%f")                                 ; current file name
 split-height-threshold              nil
 )

(blink-cursor-mode                   -1)

;;;; natural title bar
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))



(provide 'core)

