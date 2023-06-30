(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((c++-mode . lsp-mode)
         (c-mode . lsp-mode)
         ;; (elisp-mode . lsp-mode)
         ;; (emacs-lisp-mode . lsp-mode)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-clients-clangd-args '("-j=8" "--background-index" "--header-insertion=never" "--all-scopes-completion" "--clang-tidy" "--function-arg-placeholders" "--pch-storage=memory")))

(use-package lsp-ui
  :config
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-update-mode 'line)
  (setq lsp-ui-sideline-show-code-actions t)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  :commands lsp-ui-mode
  ;:bind (:map lsp-ui-mode-map
        ;      ([remap xref-find-references] . lsp-ui-peek-find-references)
        ;      ([remap xref-find-definitions] . lsp-ui-peek-find-definitions))
  )

(use-package flycheck
  :ensure t
  :hook ((c++-mode . flycheck-mode)
	 (c-mode . flycheck-mode)))


;;(use-package corfu
;;  :after lsp-mode
;;  :custom
;;  (corfu-cycle t)
;;  (corfu-auto t)
;;  :init (global-corfu-mode))

(use-package company
  :after lsp-mode
  :bind (:map company-active-map
         ("<tab>" . company-compelete-common-or-cycle))
  :init
  (global-company-mode))

(provide 'lsp-setup)
