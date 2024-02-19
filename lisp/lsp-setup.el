;;
;; LSP conf, use lsp-mode or eglot
;;

;; (require 'lsp-mode-setup)
(require 'eglot-setup)

(use-package flycheck
  :straight t
  :hook ((c++-mode . flycheck-mode)
	 (c-mode . flycheck-mode)))


;;(use-package corfu
;;  :after lsp-mode
;;  :custom
;;  (corfu-cycle t)
;;  (corfu-auto t)
;;  :init (global-corfu-mode))

(use-package company
  :straight t
  :bind (:map company-active-map
         ("<tab>" . company-complete-common-or-cycle))
  :hook
  ((prog-mode . company-mode))

  :config
  (setq company-tooltip-flip-when-above t
        ;; search candidates in space-separated regexp
        company-search-regexp-function 'company-search-words-in-any-order-regexp
        company-show-quick-access 'left)
  (global-set-key (kbd "C-c C-/") #'company-other-backend)

  (defun lc/eglot-company-hook ()
    (interactive)
    (setq-local company-backends
                '(company-bbdb
                  company-semantic
                  company-cmake
                  (company-capf :with company-yasnippet)
                  company-clang
                  company-files
                  (company-dabbrev-code company-gtags company-etags company-keywords)
                  company-oddmuse
                  company-dabbrev)))
  (defun lc/lsp-mode-company-hook ()
    (interactive)
    (setq-local company-backends
                '(company-bbdb
                  company-semantic
                  company-cmake
                  (company-yasnippet :with company-capf)
                  company-clang
                  company-files
                  (company-dabbrev-code company-gtags company-etags company-keywords)
                  company-oddmuse
                  company-dabbrev)))
  ;; after eglot lsp connected, it sets the company backend, so we need to
  ;; change it after connecting. This only happens when using eglot
  (if (featurep 'eglot-setup)
      (add-hook 'eglot-managed-mode-hook #'lc/eglot-company-hook)
    ;; in lsp-mode, company-capf seems to block yasnippet, so we use a different
    ;; hook
    (add-hook 'lsp-mode-hook #'lc/lsp-mode-company-hook))
  )


(provide 'lsp-setup)
