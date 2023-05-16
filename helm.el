(use-package helm
  :bind (("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 ("C-x b" . helm-buffers-list))
  :config
  (helm-mode 1)
  (setq helm-split-window-in-side-p t))

(use-package helm-lsp
  :bind (("C-c /" . helm-lsp-workspace-symbol)))
