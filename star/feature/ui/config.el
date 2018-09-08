;; -*- lexical-binding: t -*-

;;;
;;; Config
;;;

(global-hl-line-mode 1)

(add-hook 'prog-mode-hook #'hs-minor-mode)

(global-set-key (kbd "C-c C-h") #'hs-hide-block)
(global-set-key (kbd "C-c M-h") #'hs-show-block)


;;;
;;; Package
;;;

(use-package| solarized-theme
  :config
  (load-theme 'solarized-light t)
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :background "#f9f2d9")))
	
(use-package| doom-themes
  :config
  (add-to-list 'moon-toggle-theme-list 'doom-one)
  ; (customize|
   ; (set-face-attribute 'mode-line nil :background "#603D8E")
   ; (set-face-attribute 'lazy-highlight nil :inherit 'default :background nil :foreground "#CFD7E5" :distant-foreground nil)
   ; (set-face-attribute 'company-tooltip-common-selection nil :foreground "#C678DD"))
   )


(use-package| rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package| rainbow-mode
  :commands rainbow-mode)

(use-package| highlight-parentheses
  :init
  :config
  (set-face-attribute 'hl-paren-face nil :weight 'bold)
  (global-highlight-parentheses-mode 1)
  ;;highlight only the most inner pair
  (setq hl-paren-colors '("green"))
  ;; red in light theme, green in dark
  (change-by-theme '((spacemacs-dark . (progn (setq hl-paren-colors '("green")) (hl-paren-color-update)))
                     (spacemacs-light . (progn (setq hl-paren-colors '("red")) (hl-paren-color-update))))))


;;;;
;;;; Mode-line

(post-config| moody
  )

(use-package| moody
  :config
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (setq moody-slant-function #'moody-slant-apple-rgb)
  (setq x-underline-at-descent-line t)
  ;(moon/setup-moody)
  )

;;;;
;;;; Line number

(defvar moon-enable-nlinum-relative nil
  "Whether to enable relative line number.")

(use-package| nlinum
  :init 
  (add-hook 'moon-load-theme-hook #'moon/sync-nlinum-face)
  (add-hook 'moon-load-theme-hook #'moon/sync-nlinum-highlight-face)
  (setq nlinum-highlight-current-line t)
  :config
  (when moon-enable-nlinum-relative
    (global-nlinum-mode)
    (moon/sync-nlinum-face)
    (moon/sync-nlinum-highlight-face))
  )

(use-package| nlinum-relative
  :config
  (add-hook 'moon-load-theme-hook #'moon/sync-nlinum-relative-current-line-face)
  (add-hook 'nlinum-relative-mode-hook #'moon/sync-nlinum-relative-current-line-face)
  (when moon-enable-nlinum-relative
    (nlinum-relative-setup-evil)
    (global-nlinum-relative-mode 1)
    (moon/sync-nlinum-relative-current-line-face)
    (setq nlinum-relative-redisplay-delay 0.1)))

;;;;
;;;; Misc

(use-package| hl-todo
  :defer 3
  :config
  (add-to-list 'hl-todo-keyword-faces
               '("TOTEST" . "#d0bf8f"))
  (global-hl-todo-mode))

(post-config| general
  (moon-default-leader
    "tl" #'nlinum-mode ; toggle relative linum
    "tL" #'global-display-line-numbers-mode))

;; form feed
(use-package| form-feed
  :defer 3
  :config
  (add-hook 'emacs-lisp-mode-hook 'form-feed-mode))


;;;;
;;;; Desktop, Windows & buffer

(use-package| buffer-move
  :commands
  (buf-move-up
   buf-move-dowan
   buf-move-left
   buf-move-right))

(global-set-key (kbd "C-x C-h") #'buf-move-left)
(global-set-key (kbd "C-x C-l") #'buf-move-right)
(global-set-key (kbd "C-x C-j") #'buf-move-down)
(global-set-key (kbd "C-x C-k") #'buf-move-up)

(use-package| eyebrowse
  :commands
  (
   eyebrowse-switch-to-window-config-1
   eyebrowse-switch-to-window-config-2
   eyebrowse-switch-to-window-config-3
   eyebrowse-switch-to-window-config-4
   eyebrowse-switch-to-window-config-5
   eyebrowse-switch-to-window-config-6
   )
  :config
  (eyebrowse-mode 1)
  ;; default was ", "
  (setq eyebrowse-mode-line-separator " ")
  )


(post-config| general
  (moon-default-leader
    ;; eyebrowse
	"wd"  #'delete-window
    "wm"  #'delete-other-windows
    "w1"  #'eyebrowse-switch-to-window-config-1
    "w2"  #'eyebrowse-switch-to-window-config-2
    "w3"  #'eyebrowse-switch-to-window-config-3
    "w4"  #'eyebrowse-switch-to-window-config-4
    "w5"  #'eyebrowse-switch-to-window-config-5
    "w6"  #'eyebrowse-switch-to-window-config-6
    "wD"  #'eyebrowse-close-window-config
    ))


;;;;
;;;; Desktop resume

(post-config| general
  (moon-default-leader
    "wr" #'moon/desktop-read))

(add-hook 'moon-post-init-hook #'moon-setup-save-session)

;; copied from
;; https://gist.github.com/syl20bnr/4425094
(defun moon-setup-save-session ()
  "Setup desktop-save-mode.

Don't bother me with annoying prompts when reading
and saveing desktop."
  ;; (when (not (eq (emacs-pid) (desktop-owner))) ; Check that emacs did not load a desktop yet

    (desktop-save-mode 1) ; activate desktop mode
    (setq desktop-save t) ; always save
    ;; The default desktop is loaded anyway if it is locked
    (setq desktop-load-locked-desktop t)
    ;; Set the location to save/load default desktop
    (setq desktop-dirname moon-local-dir)

    ;; Make sure that even if emacs or OS crashed, emacs
    ;; still have last opened files.
    (add-hook 'find-file-hook
     (lambda ()
       (run-with-timer 5 nil
          (lambda ()
            ;; Reset desktop modification time so the user is not bothered
            (setq desktop-file-modtime (nth 5 (file-attributes (desktop-full-file-name))))
            (desktop-save moon-local-dir)))))

    ;; Add a hook when emacs is closed to we reset the desktop
    ;; modification time (in this way the user does not get a warning
    ;; message about desktop modifications)
    (add-hook 'kill-emacs-hook
              (lambda ()
                ;; Reset desktop modification time so the user is not bothered
                (setq desktop-file-modtime (nth 5 (file-attributes (desktop-full-file-name))))))
    ;; )
)


;;;;
;;;; Tab

(use-package| nerdtab
  :defer 2
  :config
  (setq nerdtab-window-position 'top)
  (dolist (index (number-sequence 0 9))
    (global-set-key (kbd (format "s-%d" index)) (intern (format "nerdtab-jump-%d" index))))
  (dolist (index (number-sequence 0 9))
    (global-set-key (kbd (format "C-s-%d" index)) (intern (format "nerdtab-kill-%d" index)))))

(post-config| general
  (moon-default-leader
    "tb" #'nerdtab-mode
    "bj" #'nerdtab-jump
    "bM" '(:ignore t :which-key "move tab to ")
    "bM0" #'nerdtab-move-to-0
    "bM1" #'nerdtab-move-to-1
    "bM2" #'nerdtab-move-to-2
    "bM3" #'nerdtab-move-to-3
    "bM4" #'nerdtab-move-to-4
    "bM5" #'nerdtab-move-to-5
    "bM6" #'nerdtab-move-to-6
    "bM7" #'nerdtab-move-to-7
    "bM8" #'nerdtab-move-to-8
    "bM9" #'nerdtab-move-to-9))


;;;;
;;;; Syntax

(defun moon-highlight-symbol ()
  "Hightlight symbol at point."
  (interactive)
  (evil-ex-search-activate-highlight `(,(thing-at-point 'symbol) t t)))

(post-config| general
  (moon-default-leader
    "ah" #'moon-highlight-symbol))

;;
;; auto highlight
;;

(defvar moon-auto-highlight nil
  "Wehther to highlight symbol at point after a delay.")

(defun moon-auto-highlight ()
  "Hightlight thing at point."
  (evil-ex-search-activate-highlight `(,(thing-at-point 'symbol) t t))
  (add-hook 'pre-command-hook #'moon-auto-highlight-hook))


(defun moon-auto-highlight-hook ()
  "Clean hightlight and remove self from `pre-command-hook'."
    (evil-ex-nohighlight)
    (remove-hook 'pre-command-hook #'moon-auto-highlight-hook))

(defvar moon-auto-highlight-timer nil
  "Idle timer of moon-auto-hightlight-mode.")

(define-minor-mode moon-auto-highlight-mode
  "Highlight symbol at point automatically after a delay."
  :global
  :lighter "AutoH"
  (if moon-auto-highlight-mode
      (setq moon-auto-highlight-timer (run-with-idle-timer 1 t #'moon-auto-highlight))
    (cancel-timer moon-auto-highlight-timer)))

;;
;;;; VC

;; not autoloaded
(use-package| diff-hl
  :config
  (setq diff-hl-draw-borders nil)
  (add-hook 'after-change-major-mode-hook
            (lambda ()
              "Enable `diff-hl-mode' or `diff-hl-margin-mode'."
              (if window-system
                  (diff-hl-mode)
                (diff-hl-margin-mode))))
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))
