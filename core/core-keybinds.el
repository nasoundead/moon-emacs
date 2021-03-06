;;; core-keybinds.el -*- lexical-binding: t; -*-

;; A centralized keybinds system, integrated with `which-key' to preview
;; available keybindings. All built into one powerful macro: `map!'. If evil is
;; never loaded, then evil bindings set with `map!' will be ignored.

(defvar moon-leader-key "SPC"
  "The leader prefix key, for global commands.")

(defvar moon-localleader-key "SPC m"
  "The localleader prefix key, for major-mode specific commands.")

(defvar moon-evil-state-alist
  '((?n . normal)
    (?v . visual)
    (?i . insert)
    (?e . emacs)
    (?o . operator)
    (?m . motion)
    (?r . replace)
    (?g . global))
  "A list of cons cells that map a letter to a evil state symbol.")

;;
(defvar moon-escape-hook nil
  "A hook run after C-g is pressed (or ESC in normal mode, for evil users). Both
trigger `moon/escape'.

If any hook returns non-nil, all hooks after it are ignored.")

(defun moon/escape ()
  "Run the `moon-escape-hook'."
  (interactive)
  (cond ((minibuffer-window-active-p (minibuffer-window))
         ;; quit the minibuffer if open.
         (abort-recursive-edit))
        ;; Run all escape hooks. If any returns non-nil, then stop there.
        ((cl-find-if #'funcall moon-escape-hook))
        ;; don't abort macros
        ((or defining-kbd-macro executing-kbd-macro) nil)
        ;; Back to the default
        ((keyboard-quit))))

(global-set-key [remap keyboard-quit] #'moon/escape)




;; `hydra'
(setq lv-use-seperator t)


;;
(defun moon--keybind-register (key desc &optional modes)
  "Register a description for KEY with `which-key' in MODES.

  KEYS should be a string in kbd format.
  DESC should be a string describing what KEY does.
  MODES should be a list of major mode symbols."
  (after! which-key
    (if modes
        (dolist (mode modes)
          (which-key-add-major-mode-key-based-replacements mode key desc))
      (which-key-add-key-based-replacements key desc))))


(defun moon--keyword-to-states (keyword)
  "Convert a KEYWORD into a list of evil state symbols.

For example, :nvi will map to (list 'normal 'visual 'insert). See
`moon-evil-state-alist' to customize this."
  (cl-loop for l across (substring (symbol-name keyword) 1)
           if (cdr (assq l moon-evil-state-alist)) collect it
           else do (error "not a valid state: %s" l)))


;; Register keywords for proper indentation (see `map!')
(put :after        'lisp-indent-function 'defun)
(put :desc         'lisp-indent-function 'defun)
(put :leader       'lisp-indent-function 'defun)
(put :local        'lisp-indent-function 'defun)
(put :localleader  'lisp-indent-function 'defun)
(put :map          'lisp-indent-function 'defun)
(put :map*         'lisp-indent-function 'defun)
(put :mode         'lisp-indent-function 'defun)
(put :prefix       'lisp-indent-function 'defun)
(put :textobj      'lisp-indent-function 'defun)
(put :unless       'lisp-indent-function 'defun)
(put :when         'lisp-indent-function 'defun)

;; specials
(defvar moon--keymaps nil)
(defvar moon--prefix  nil)
(defvar moon--defer   nil)
(defvar moon--local   nil)

(defmacro map! (&rest rest)
  "A nightmare of a key-binding macro that will use `evil-define-key*',
`define-key', `local-set-key' and `global-set-key' depending on context and
plist key flags (and whether evil is loaded or not). It was designed to make
binding multiple keys more concise, like in vim.

If evil isn't loaded, it will ignore evil-specific bindings.

States
    :n  normal
    :v  visual
    :i  insert
    :e  emacs
    :o  operator
    :m  motion
    :r  replace

    These can be combined (order doesn't matter), e.g. :nvi will apply to
    normal, visual and insert mode. The state resets after the following
    key=>def pair.

    If states are omitted the keybind will be global.

    This can be customized with `moon-evil-state-alist'.

    :textobj is a special state that takes a key and two commands, one for the
    inner binding, another for the outer.

Flags
    (:leader [...])            an alias for (:prefix moon-leader-key ...)
    (:localleader [...])       an alias for (:prefix moon-localleader-key ...)
    (:mode [MODE(s)] [...])    inner keybinds are applied to major MODE(s)
    (:map [KEYMAP(s)] [...])   inner keybinds are applied to KEYMAP(S)
    (:map* [KEYMAP(s)] [...])  same as :map, but deferred
    (:prefix [PREFIX] [...])   assign prefix to all inner keybindings
    (:after [FEATURE] [...])   apply keybinds when [FEATURE] loads
    (:local [...])             make bindings buffer local; incompatible with keymaps!

Conditional keybinds
    (:when [CONDITION] [...])
    (:unless [CONDITION] [...])

Example
    (map! :map magit-mode-map
          :m \"C-r\" 'do-something           ; assign C-r in motion state
          :nv \"q\" 'magit-mode-quit-window  ; assign to 'q' in normal and visual states
          \"C-x C-r\" 'a-global-keybind

          (:when IS-MAC
           :n \"M-s\" 'some-fn
           :i \"M-o\" (lambda (interactive) (message \"Hi\"))))"
  (let ((moon--keymaps moon--keymaps)
        (moon--prefix  moon--prefix)
        (moon--defer   moon--defer)
        (moon--local   moon--local)
        key def states forms desc modes)
    (while rest
      (setq key (pop rest))
      (cond
       ;; it's a sub expr
       ((listp key)
        (push (macroexpand `(map! ,@key)) forms))

       ;; it's a flag
       ((keywordp key)
        (cond ((eq key :leader)
               (push 'moon-leader-key rest)
               (setq key :prefix
                     desc "<leader>"))
              ((eq key :localleader)
               (push 'moon-localleader-key rest)
               (setq key :prefix
                     desc "<localleader>")))
        (pcase key
          (:when    (push `(if ,(pop rest)       ,(macroexpand `(map! ,@rest))) forms) (setq rest '()))
          (:unless  (push `(if (not ,(pop rest)) ,(macroexpand `(map! ,@rest))) forms) (setq rest '()))
          (:after   (push `(after! ,(pop rest)   ,(macroexpand `(map! ,@rest))) forms) (setq rest '()))
          (:desc    (setq desc (pop rest)))
          ((or :map :map*)
            (setq moon--keymaps (moon-enlist (pop rest))
                  moon--defer (eq key :map*)))
          (:mode
            (setq modes (moon-enlist (pop rest)))
            (unless moon--keymaps
              (setq moon--keymaps
                    (cl-loop for m in modes
                             collect (intern (format "%s-map" (symbol-name m)))))))
          (:textobj
            (let* ((key (pop rest))
                   (inner (pop rest))
                   (outer (pop rest)))
              (push (macroexpand `(map! (:map evil-inner-text-objects-map ,key ,inner)
                                        (:map evil-outer-text-objects-map ,key ,outer)))
                    forms)))
          (:prefix
            (let ((def (pop rest)))
              (setq moon--prefix
                    `(vconcat ,moon--prefix
                              ,(if (or (stringp def)
                                       (and (symbolp def)
                                            (stringp (symbol-value def))))
                                   `(kbd ,def)
                                 def)))
              (when desc
                (push `(moon--keybind-register ,(key-description (eval moon--prefix))
                                               ,desc ',modes)
                      forms)
                (setq desc nil))))
          (:local
           (setq moon--local t))
          (_ ; might be a state moon--prefix
           (setq states (moon--keyword-to-states key)))))

       ;; It's a key-def pair
       ((or (stringp key)
            (characterp key)
            (vectorp key)
            (symbolp key))
        (unwind-protect
            (catch 'skip
              (when (symbolp key)
                (setq key `(kbd ,key)))
              (when (stringp key)
                (setq key (kbd key)))
              (when moon--prefix
                (setq key (append moon--prefix (list key))))
              (unless (> (length rest) 0)
                (user-error "map! has no definition for %s key" key))
              (setq def (pop rest))
              (when desc
                (push `(moon--keybind-register ,(key-description (eval key))
                                               ,desc ',modes)
                      forms))
              (cond ((and moon--local moon--keymaps)
                     (push `(lwarn 'moon-map :warning
                                   "Can't local bind '%s' key to a keymap; skipped"
                                   ,key)
                           forms)
                     (throw 'skip 'local))
                    ((and moon--keymaps states)
                     (dolist (keymap moon--keymaps)
                       (when (memq 'global states)
                         (push `(define-key ,keymap ,key ,def) forms))
                       (when (featurep 'evil)
                         (when-let* ((states (delq 'global states)))
                           (push `(,(if moon--defer #'evil-define-key #'evil-define-key*)
                                   ',states ,keymap ,key ,def)
                                 forms)))))
                    (states
                     (dolist (state states)
                       (if (eq state 'global)
                           (push `(global-set-key ,key ,def) forms)
                         (when (featurep 'evil)
                           (push (if moon--local
                                     `(evil-local-set-key ',state ,key ,def)
                                   `(evil-define-key* ',state 'global ,key ,def))
                                 forms)))))
                    (moon--keymaps
                     (dolist (keymap moon--keymaps)
                       (push `(define-key ,keymap ,key ,def) forms)))
                    (t
                     (push `(,(if moon--local #'local-set-key #'global-set-key)
                             ,key ,def)
                           forms))))
          (setq states '()
                moon--local nil
                desc nil)))

       (t (user-error "Invalid key %s" key))))
    `(progn ,@(nreverse forms))))

(provide 'core-keybinds)
;;; core-keybinds.el ends here
