;; -*- lexical-binding: t -*-

(setq moon-evil t)

(defvar moon-evil-mode-list
  '(lisp-interaction-mode
    text-mode
    fundamental-mode)
  "Modes in where you want evil enables.")

(defvar moon-non-evil-mode-list
  '(magit-mode)
  "Modes in where you don't want evil enables.")

(add-hook 'moon-post-init-hook (lambda () "Evilfy Messages and Scratch buffer."
                                 (switch-to-buffer "*Messages*")
                                 (evil-local-mode)
                                 (switch-to-buffer "*scratch*")
                                 (evil-local-mode)
                                 (switch-to-buffer (or moon-homepage-buffer "*scratch*"))))

(use-package| evil
  :config
  (evil-local-mode)
  ;; enabled evil when editing text
  (add-hook 'after-change-major-mode-hook #'moon-smart-evil)

  ;; fix paste issue in evil visual mode
  ;; http://emacs.stackexchange.com/questions/14940/emacs-doesnt-paste-in-evils-visual-mode-with-every-os-clipboard/15054#15054
  (fset 'evil-visual-update-x-selection 'ignore)
  ;; https://github.com/syl20bnr/spacemacs/issues/6636
  ;; setting this directly doesn't work
  ;; you have to set it through customize
  ;; (customize-set-variable evil-search-module 'evil-search)
  (setq evil-ex-substitute-global t))

(defun moon-smart-evil ()
  "Enable evil when major mode complies."
  (when (and (or (derived-mode-p 'prog-mode 'special-mode)
                 (member major-mode moon-evil-mode-list))
             (not (member major-mode moon-non-evil-mode-list)))
    (evil-local-mode)))

;;;; smart selection for evil search motions

;; + in visual mode
;; binded below by general.el
(defun moon/make-region-search-history ()
  "Make region a histroy so I can use cgn."
  (interactive)
  (let ((region (strip-text-properties (funcall region-extract-function nil))))
    (push region evil-ex-search-history)
    (setq evil-ex-search-pattern (evil-ex-make-search-pattern region))
    (evil-ex-search-activate-highlight evil-ex-search-pattern)
    (deactivate-mark))
  (goto-char (1- (point))))

(defun moon/pop-kill-ring-to-search-history ()
  "Pop text in kill ring to search history."
  (interactive)
  (let ((text (car kill-ring)))
    (push text evil-ex-search-history)
    (setq evil-ex-search-pattern (evil-ex-make-search-pattern text))
    (evil-ex-search-activate-highlight evil-ex-search-pattern)))


;; / in visual mode will start search immediatly
(defun moon-evil-ex-start-search-with-region-string ()
  (let ((selection (with-current-buffer (other-buffer (current-buffer) 1)
                     (when (evil-visual-state-p)
                       (let ((selection (buffer-substring-no-properties (region-beginning)
                                                                        (1+ (region-end)))))
                         (evil-normal-state)
                         selection)))))
    (when selection
      (evil-ex-remove-default)
      (insert selection)
      (evil-ex-search-activate-highlight (list selection
                                               evil-ex-search-count
                                               evil-ex-search-direction)))))

(advice-add #'evil-ex-search-setup :after #'moon-evil-ex-start-search-with-region-string)

;; # in visual mode
(defun moon-evil-ex-search-word-backward-advice (old-func count &optional symbol)
  (if (evil-visual-state-p)
      (let ((region (buffer-substring-no-properties
                     (region-beginning) (1+ (region-end)))))
        (setq evil-ex-search-pattern region)
        (deactivate-mark)
        (evil-ex-search-full-pattern region count 'backward))
    (apply old-func count symbol)))

;; \* in visual mode
(defun moon-evil-ex-search-word-forward-advice (old-func count &optional symbol)
  (if (evil-visual-state-p)
      (let ((region (buffer-substring-no-properties
                     (region-beginning) (1+ (region-end)))))
        (setq evil-ex-search-pattern region)
        (deactivate-mark)
        (evil-ex-search-full-pattern region count 'forward))
    (apply old-func count symbol)))

(advice-add #'evil-ex-search-word-backward :around #'moon-evil-ex-search-word-backward-advice)
(advice-add #'evil-ex-search-word-forward :around #'moon-evil-ex-search-word-forward-advice)


;;
;; Plugins
;;

(use-package| evil-commentary
  :commands (evil-commentary evil-commentary-yank evil-commentary-line)
  :config (evil-commentary-mode 1))
  
(use-package| evil-easymotion
  :commands (evilem-create evilem-default-keybindings)
  :config
  ;; Use evil-search backend, instead of isearch
  (evilem-make-motion evilem-motion-search-next #'evil-ex-search-next
                      :bind ((evil-ex-search-highlight-all nil)))
  (evilem-make-motion evilem-motion-search-previous #'evil-ex-search-previous
                      :bind ((evil-ex-search-highlight-all nil)))

  (evilem-make-motion evilem-motion-search-word-forward #'evil-ex-search-word-forward
                      :bind ((evil-ex-search-highlight-all nil)))
  (evilem-make-motion evilem-motion-search-word-backward #'evil-ex-search-word-backward
                      :bind ((evil-ex-search-highlight-all nil))))

(use-package| evil-matchit
  :commands (evilmi-jump-items global-evil-matchit-mode
             evilmi-outer-text-object evilmi-inner-text-object)
  :config (global-evil-matchit-mode 1)
  :init
  (global-set-key [remap evil-jump-item] #'evilmi-jump-items)
  (define-key evil-inner-text-objects-map "%" #'evilmi-inner-text-object)
  (define-key evil-outer-text-objects-map "%" #'evilmi-outer-text-object)
  :config
  ;; Fixes #519 where d% wouldn't leave a dangling end-parenthesis
  (evil-set-command-properties 'evilmi-jump-items :type 'inclusive :jump t)

  (defun +evil|simple-matchit ()
    "A hook to force evil-matchit to favor simple bracket jumping. Helpful when
the new algorithm is confusing, like in python or ruby."
    (setq-local evilmi-always-simple-jump t))
  (add-hook 'python-mode-hook #'+evil|simple-matchit))
  
(use-package| evil-embrace
  :after evil-surround
  :commands (embrace-add-pair embrace-add-pair-regexp)
  :hook (LaTeX-mode . embrace-LaTeX-mode-hook)
  :hook (org-mode . embrace-org-mode-hook)
  :init
  ;; Add extra pairs
  (add-hook! emacs-lisp-mode
    (embrace-add-pair ?\` "`" "'"))
  (add-hook! (emacs-lisp-mode lisp-mode)
    (embrace-add-pair-regexp ?f "([^ ]+ " ")" #'+evil--embrace-elisp-fn))
  (add-hook! (org-mode LaTeX-mode)
    (embrace-add-pair-regexp ?l "\\[a-z]+{" "}" #'+evil--embrace-latex))
  :config
  (setq evil-embrace-show-help-p nil)
  (evil-embrace-enable-evil-surround-integration)

  (defun +evil--embrace-get-pair (char)
    (if-let* ((pair (cdr-safe (assoc (string-to-char char) evil-surround-pairs-alist))))
        pair
      (if-let* ((pair (assoc-default char embrace--pairs-list)))
          (if-let* ((real-pair (and (functionp (embrace-pair-struct-read-function pair))
                                    (funcall (embrace-pair-struct-read-function pair)))))
              real-pair
            (cons (embrace-pair-struct-left pair) (embrace-pair-struct-right pair)))
        (cons char char))))

  (defun +evil--embrace-escaped ()
    "Backslash-escaped surround character support for embrace."
    (let ((char (read-char "\\")))
      (if (eq char 27)
          (cons "" "")
        (let ((pair (+evil--embrace-get-pair (string char)))
              (text (if (sp-point-in-string) "\\\\%s" "\\%s")))
          (cons (format text (car pair))
                (format text (cdr pair)))))))

  (defun +evil--embrace-latex ()
    "LaTeX command support for embrace."
    (cons (format "\\%s{" (read-string "\\")) "}"))

  (defun +evil--embrace-elisp-fn ()
    "Elisp function support for embrace."
    (cons (format "(%s " (or (read-string "(") "")) ")"))

  ;; Add escaped-sequence support to embrace
  (setf (alist-get ?\\ (default-value 'embrace--pairs-list))
        (make-embrace-pair-struct
         :key ?\\
         :read-function #'+evil--embrace-escaped
         :left-regexp "\\[[{(]"
         :right-regexp "\\[]})]")))

(use-package| evil-surround
  :commands (global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region)
  :config (global-evil-surround-mode 1))

(use-package| evil-escape
  :commands (evil-escape evil-escape-mode evil-escape-pre-command-hook)
  :init
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
        evil-escape-excluded-major-modes '(neotree-mode treemacs-mode)
        evil-escape-key-sequence "jk"
        evil-escape-delay 0.25)
  (add-hook 'pre-command-hook #'evil-escape-pre-command-hook)
  (evil-define-key* '(insert replace visual operator) 'global "\C-g" #'evil-escape)
  :config
  ;; no `evil-escape' in minibuffer
  (add-hook 'evil-escape-inhibit-functions #'minibufferp))

(use-package| evil-exchange
  :commands evil-exchange
  :config
  (defun +evil|escape-exchange ()
    (when evil-exchange--overlays
      (evil-exchange-cancel)
      t))
  (add-hook 'moon-escape-hook #'+evil|escape-exchange))


(use-package| evil-numbers
  :commands (evil-numbers/inc-at-pt evil-numbers/dec-at-pt))

(use-package| evil-ediff
  :after evil
  :defer 2
  :hook (ediff-mode . (lambda () (require 'evil-ediff))))

(use-package| evil-vimish-fold
  :commands (evil-vimish-fold/next-fold evil-vimish-fold/previous-fold
             evil-vimish-fold/delete evil-vimish-fold/delete-all
             evil-vimish-fold/create evil-vimish-fold/create-line)
  :init
  (setq vimish-fold-dir (concat moon-cache-dir "vimish-fold/")
        vimish-fold-indication-mode 'right-fringe)
  :config
  (vimish-fold-global-mode +1))
  
(use-package| evil-snipe
  :commands (evil-snipe-mode evil-snipe-override-mode
             evil-snipe-local-mode evil-snipe-override-local-mode)
  :init
  (setq evil-snipe-smart-case t
        evil-snipe-scope 'line
        evil-snipe-repeat-scope 'visible
        evil-snipe-char-fold t)
  :config
  (add-to-list 'evil-snipe-disabled-modes 'Info-mode nil #'eq)
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))
  
;; Without `evil-visualstar', * and # grab the word at point and search, no
;; matter what mode you're in. I want to be able to visually select a region and
;; search for other occurrences of it.
(use-package| evil-visualstar
  :commands (evil-visualstar/begin-search
             evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward)
  :init
  (evil-define-key* 'visual 'global
    "*" #'evil-visualstar/begin-search-forward
    "#" #'evil-visualstar/begin-search-backward))
	
;;
;;; Config
;;

;;
;; Replace some keys

(post-config| general
  (after-load| evil
    (general-define-key
     :states 'normal
     "c" (general-key-dispatch 'evil-change
           "s" #'isolate-quick-change
           "S" #'isolate-long-change)
     "d" (general-key-dispatch 'evil-delete
           "s" #'isolate-quick-delete
           "S" #'isolate-long-delete))
    
    (general-define-key
     :states 'visual
     ;; `evil-change' is not bound in `evil-visual-state-map' by default but
     ;; inherited from `evil-normal-state-map'
     ;; if you don't want "c" to be affected in visual state, you should add this
     "c" #'evil-change
     "d" #'evil-delete
     "s" #'isolate-quick-add
     "S" #'isolate-long-add
     "x" #'exchange-point-and-mark ; for expand-region
     "+" #'moon/make-region-search-history
     )

    (general-define-key
     :states 'insert
     "M-n" #'next-line
     "M-p" #'previous-line
     "C-a" #'beginning-of-line
     "C-e" #'end-of-line)

    (general-define-key
     :states '(normal visual)
     "H"   #'evil-beginning-of-line
     "L"   #'evil-end-of-line
     "P"   #'evil-paste-from-register)
    
    (moon-default-leader
      "sc" #'moon/clear-evil-search
      "ij" '((lambda () (interactive) (evil-insert-line-below)) :which-key "insert-line-below")
      "ik" '((lambda () (interactive) (evil-insert-line-above)) :which-key "insert-line-above")
      "uu" #'undo-tree-visualize
      "+" #'moon/pop-kill-ring-to-search-history)

    (moon-default-leader
      :keymaps 'term-mode-map
      "c" '((lambda ()
              (interactive)
              (term-char-mode)
              (evil-insert-state)) :which-key "char-mode")
      "l" #'term-line-mode
      "bl" #'evil-switch-to-windows-last-buffer)

    (general-define-key
     :keymaps 'override
     "s-e" #'evil-local-mode)))

;; This way "/" respects the current region
;; but not when you use 'evil-search as evil-search-module
;; https://stackoverflow.com/questions/202803/searching-for-marked-selected-text-in-emacs
(defun moon-isearch-with-region ()
  "Use region as the isearch text."
  (when mark-active
    (let ((region (funcall region-extract-function nil)))
      (deactivate-mark)
      (isearch-push-state)
      (isearch-yank-string region))))
(add-hook 'isearch-mode-hook #'moon-isearch-with-region)

