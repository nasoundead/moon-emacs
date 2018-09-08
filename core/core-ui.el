;; -*- lexical-binding: t -*-

;;
;;; Homepage
;;
(setq inhibit-startup-screen t)

(defvar moon-homepage-buffer "HOME"
  "The buffer name of the homepage")

;;
;;; Theme
;;

(defvar moon-load-theme-hook ()
  "Hook ran after `load-theme'")

(defvar moon-current-theme ""
  "The last loaded theme name in string.")

(defvar moon-toggle-theme-list ()
  "Themes that you can toggle bwtween by `moon/switch-theme'")

(defvar moon-theme-book '(solarized-light doom-one)
  "A list of themes that you can load with `moon/load-theme'.")

(defun moon-set-current-theme (&rest form)
  "Adveiced before `load-theme', set `moon-current-theme'."
  (setq moon-current-theme (symbol-name (car form))))

(defadvice load-theme (after run-load-theme-hook activate)
  (run-hook-with-args 'moon-load-theme-hook))

(advice-add #'load-theme :before #'moon-set-current-theme)

;;
;;; Font
;;

(defvar moon-magic-font-book
  '(
    ("Source Code Pro" . (moon-set-font| :family "Source Code Pro"
                                  :weight 'light
                                  :size 14))
    ("SF Mono" . (moon-set-font| :family "SF Mono" :weight 'light :size 14))
    ("Source Code Pro for Powerline" . (moon-set-font| :family "Source Code Pro for Powerline" :weight 'light :size 14))
    )

  "All the fonts you can switch between by `moon/load-font'
It is an alist with the form
((name . (moon-set-font| configuration))
 (name . (moon-set-font| :family \"family\" :weight 'weight)))

I have to do it this way because apply flattens a list ignoring quotes.
So '(:family \"SF Mono\" :weight 'light) will become
(:family \"SF Mono\" :weight quote light).
And such list cannot be passed into a `font-spec'.")

;;
;;; Function
;;

(defun change-by-theme (config-list)
  "Evaluate diffrent form based on what is the current theme.

CONFIG-LIST is a list of (theme . form).

For example:
  (change-by-theme 
    '((spacemacs-dark . (progn 
                         (setq hl-paren-colors '(\"green\")) 
                         (hl-paren-color-update)))
      (spacemacs-light . (progn 
                         (setq hl-paren-colors '(\"red\")) 
                         (hl-paren-color-update)))))"
  (add-hook
    'moon-load-theme-hook
    (lambda ()
      (dolist (config config-list)
        (let ((theme (symbol-name (car config)))
              (form (cdr config)))
          (when (equal moon-current-theme theme)
            (eval form)))))))

			
(when IS-WIN
;; Setting English Font
;; (set-face-attribute 'default nil :font "Monaco 11")

;; Chinese Font
; (dolist (charset '(kana han symbol cjk-misc bopomofo))
  ; (set-fontset-font (frame-parameter nil 'font)
                    ; charset (font-spec :family "宋体"
                                       ; :size 16)))
(set-fontset-font "fontset-default" 'chinese-gbk "宋体")
(setq face-font-rescale-alist '(("宋体" . 1.2)
                ("微软雅黑" . 1.1)
                ))									   
;; For Windows
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)									   
) 
(when IS-LINUX
;; For Linux
(global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)
(global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease)
) 			

(provide 'core-ui)

