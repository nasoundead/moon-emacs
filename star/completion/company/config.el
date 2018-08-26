;; -*- lexical-binding: t -*-

(use-package| company
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 2
        company-dabbrev-downcase nil
        company-tooltip-limit 15
		company-global-modes '(not eshell-mode comint-mode erc-mode message-mode help-mode gud-mode)
        company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend)
        company-backends '(company-capf company-dabbrev company-ispell)
        company-transformers '(company-sort-by-occurrence))
  (setq-default company-search-filtering t)
  (global-company-mode 1)
  (after-load| yasnippet
    (nconc company-backends '(company-yasnippet))))
  
(use-package| company-quickhelp
  :after company
  :config
  (setq company-quickhelp-delay nil)
  (company-quickhelp-mode +1))
(provide 'init-company)
  
(use-package| company-statistics
  :after company
  :config
  (setq company-statistics-file (concat moon-cache-dir "company-stats-cache.el"))
  (company-statistics-mode +1))

(post-config| general
  (general-define-key
   :keymaps '(company-active-map
              company-search-map)
   "C-j"      #'company-search-candidates
   "C-p"      #'company-select-previous
   "C-n"      #'company-select-next
   "<tab>"    #'company-complete-common-or-cycle
   "<backtab>" #'company-select-previous
   )
   
  (general-define-key
   :keymaps 'company-search-map
   "<escape>" #'company-abort))

;; (use-package| company-box
;;   ;; TODO common face
;;   :init
;;   ;; (defface company-box-candidates
;;   ;;   '((t (:inherit company-tooltip)))
;;   ;;   "Override face of company-box.")
;;   ;; (defface company-box-selection
;;   ;;   '((t (:inherit company-tooltip-selection)))
;;   ;;   "Override face of company-box.")
;;   ;; (defface company-box-annotation
;;   ;;   '((t (:inherit company-tooltip-annotation)))
;;   ;;   "Override face of company-box.")
;;   :config
;;   (setq company-box-enable-icon nil)
;;   (setq company-box-doc-delay 0.3)
;;   :hook (company-mode . company-box-mode))
