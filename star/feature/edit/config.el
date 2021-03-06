;; -*- lexical-binding: t -*-

;;
;;; Package
;;

    
;;;; Edit

(use-package| (evil-moccur :fetcher github :repo "casouri/evil-moccur")
  :defer 2)

(use-package| expand-region
  :commands er/expand-region)
;; expand-region's prompt can't tell what key contract-region is bound to, so we
;; tell it explicitly.
(setq expand-region-contract-fast-key "V")
; (map!
  ; (after! evil
; ;; expand-region
      ; :v  "v"  #'er/expand-region
      ; :v  "V"  #'er/contract-region))
(post-config| general
  (moon-g-leader
    "v" #'er/expand-region
	"V" #'er/contract-region)
  (moon-default-leader
    "v" #'er/expand-region
	"V" #'er/contract-region))



(use-package| undo-tree
  :config (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t))

(use-package| hungry-delete
  :defer 2
  :config
  (global-set-key (kbd "<S-backspace>") #'hungry-delete-backward))

;;;; Navigation


(use-package| recentf-ext
  :commands (recentf counsel-recentf))
(use-package| recentf
  :init
  (add-hook 'find-file-hook (lambda ()
                              (unless recentf-mode
                                (recentf-mode)
                                (recentf-track-opened-file))))
  :config
  (setq recentf-save-file (concat moon-cache-dir "recentf")
        recentf-max-menu-items 0
        recentf-max-saved-items 300
        recentf-filename-handlers '(file-truename)
        recentf-exclude
        (list "^/tmp/" "^/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$"
              "^/var/folders/.+$"
              ;; ignore private moon temp files (but not all of them)
              (concat "^" (file-truename moon-cache-dir)))))

(use-package| avy
  :defer 2
  :config
  (setq avy-background t)
  (setq avy-all-windows nil))

(post-config| general
  (moon-default-leader
    "k" #'avy-goto-char-timer))

(use-package| minimap
  :config
  (setq
   minimap-width-fraction 0.1
   minimap-window-location 'right
   minimap-update-delay 0)
  (custom-theme-set-faces 'user
                          '(minimap-active-region-background
                            ((((background dark)) (:background "#61526E"))
                             (t (:background "#d3d3e7")))))
  :commands minimap-mode)

(post-config| general
  (moon-default-leader "tm" #'minimap-mode))

;; used for chinese editiing on macOS
(load| switch-input-mode)


;;;; code structure

(use-package| outshine
  :init
  (add-hook 'outline-minor-mode-hook 'outshine-hook-function)
  (add-hook 'prog-mode-hook 'outline-minor-mode)
  (defvar outline-minor-mode-prefix (kbd "C-c o")))

(post-config| general
  (moon-default-leader
    :keymaps 'outline-minor-mode-map
    "o" '(:ignore t :which-key "outline")
    "os"    #'outline-show-entry
    "oh"    #'outline-hide-entry
    "o M-b" #'outline-show-entry
    "o C-b" #'outline-hide-entry
    "o M-o" #'outline-show-all
    "o C-o" #'outline-hide-body))

(use-package| (color-moccur :fetcher url :url "http://www.emacswiki.org/emacs/download/color-moccur.el")
  :defer 3)
(use-package| (moccur-edit :fetcher url :url "https://www.emacswiki.org/emacs/download/moccur-edit.el")
  :after color-moccur)


;;;
;;; Config
;;;

;;;;
;;;; Default

(electric-pair-mode 1)

;; smooth scrolling
(setq scroll-conservatively 101)
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(post-config| general
  (general-define-key
   :states '(normal insert)
   "C-;" #'moon/insert-semi-at-eol
   )
  (general-define-key
   :states 'normal
   "J" #'moon/scroll-down-reserve-point
   "K" #'moon/scroll-up-reserve-point)) 

;; utf-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))


;;;; split screen vertically in ediff
(setq ediff-split-window-function #'split-window-horizontally)



;;;;
;;;; Fix

(defun moon/return-cancel-completion ()
  "Cancel completion and return."
  (interactive)
  (company-abort)
  (newline nil t))

(global-set-key (kbd "<C-return>") #'moon/return-cancel-completion)

;; never tested
;; http://emacsredux.com/blog/2013/04/21/edit-files-as-root/
(defadvice ivy-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

