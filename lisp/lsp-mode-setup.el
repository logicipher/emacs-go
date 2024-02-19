(use-package lsp-mode
  :straight t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((c++-mode . lsp-mode)
         (c-mode . lsp-mode)
         ;; (elisp-mode . lsp-mode)
         ;; (emacs-lisp-mode . lsp-mode)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-clients-clangd-args '("-j=8" "--background-index" "--header-insertion=never" "--all-scopes-completion" "--clang-tidy" "--function-arg-placeholders" "--pch-storage=memory"))
  (progn
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-tramp-connection '("clangd" "-j=8" "--background-index" "--header-insertion=never" "--all-scopes-completion" "--clang-tidy" "--function-arg-placeholders" "--pch-storage=memory"))
		      :major-modes '(c-mode c++-mode)
		      :remote? t
		      :server-id 'clangd-remote)))
  )


(use-package lsp-ui
  :straight t
  :after lsp-mode
  :config
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-update-mode 'line)
  (setq lsp-ui-sideline-show-code-actions t)
  ;; (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-position 'at-point)
  ;; (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  ;; (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  :commands lsp-ui-mode
  :bind (:map lsp-ui-mode-map
              ("C-c l f" . lsp-ui-doc-focus-frame)
              ("C-c l u" . lsp-ui-doc-unfocus-frame))
  )

(use-package lsp-treemacs
  :straight t
  :after lsp-mode
  :hook (lsp-mode . lsp-treemacs-sync-mode))

(provide 'lsp-mode-setup)
