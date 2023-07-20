(use-package tree-sitter
  :init
  (global-tree-sitter-mode)
  :hook (tree-sitter-after-on . tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :after tree-sitter)

(use-package cmake-font-lock)

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :init (setq markdown-command "pandoc"))

(provide 'lang-setup)
