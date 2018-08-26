;;(package-initialize t)

(load (concat (expand-file-name user-emacs-directory) "core/core"))

(moon| :basic
       ;; non-evil
       homepage
       key
       evil
       ui
       other
       edit
       project
       :completion
       ivy
       company
       snippet
       ; :os
       ; mac
       :utility
       ;eshell
       ;tex
       ;dir
       ;git
       org
       ;; imagemagick
       ; :checker
       ; syntax
       ; spell
       :lang
       cc
       lsp
       python
       elisp
       ;; rust
       javascript
       web
       ;; lua
       )


;;
;;; Settings evaluate befor loading any stars i.e. user-init
;;

