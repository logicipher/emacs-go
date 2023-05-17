(use-package helm
  :init
  (setq helm-command-prefix-key "C-c c")
  :bind (("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 ("C-x b" . helm-buffers-list))
  :config
  (helm-mode 1)
  ;(setq helm-split-window-in-side-p t)
  (setq helm-default-display-buffer-functions '(display-buffer-in-side-window))
  ;(setq helm-show-completion-display-functions #'helm-display-buffer-in-own-frame)
)

(use-package helm-lsp
  :bind (("C-c /" . helm-lsp-workspace-symbol)))
