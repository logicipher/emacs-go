(use-package eglot
  :straight t
  :hook ((c++-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (emacs-lisp-mode . eglot-ensure))
  :init
  ;; setup keymap

  ;; keymap prefix
  (defvar lc-eglot-keymap-prefix "C-c l")
  (defvar lc-eglot-command-map
    (let ((map (make-sparse-keymap)))
      ;; workspace
      (define-key map "wq" 'eglot-shutdown)
      (define-key map "wr" 'eglot-reconnect)
      (define-key map "ws" 'eglot)

      ;;Toggle
      (define-key map "Th" 'eglot-inlay-hints-mode)

      ;; format
      (define-key map "==" 'eglot-format-buffer)
      (define-key map "=r" 'eglot-format)

      ;; goto
      (define-key map "gd" 'xref-find-definitions)
      (define-key map "gg" 'xref-find-definitions)
      (define-key map "gr" 'xref-find-references)
      ;;action
      (define-key map "aa" 'eglot-code-actions)
      (define-key map "aq" 'eglot-code-actions-quickfix)
      ;; help
      (define-key map "hh" 'eldoc)
      (define-key map "hb" 'flymake-show-buffer-diagnostics)
      map))

  ;; :bind (:map eglot-mode-map
  ;;             (lc-eglot-keymap-prefix . lc-eglot-command-map))
  :config
  (add-to-list 'eglot-server-programs
               '((c++-mode c-mode) . ("clangd" "-j=8" "--background-index" "--header-insertion=never" "--all-scopes-completion" "--clang-tidy" "--function-arg-placeholders" "--pch-storage=memory")))

  (define-key eglot-mode-map (kbd lc-eglot-keymap-prefix) lc-eglot-command-map)
  ;; (setq eldoc-echo-area-use-multiline-p nil)

  )

(provide 'eglot-setup)
