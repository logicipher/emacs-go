(use-package yasnippet
  :straight t
  :hook ((prog-mode . yas-minor-mode)
	 (org-mode . yas-minor-mode)
	 (emacs-lisp-mode . yas-minor-mode))
  :config
  (yas-reload-all)
  (setq yas-snippet-dirs
	'("~/.emacs.d/snippets"           ;; public personal snippets, held by git
	  "~/.emacs.d/private-snippets"   ;; private project-used snippets, not synced
	  )))

;; (use-package yasnippet-snippets)

(provide 'snippet-setup)
