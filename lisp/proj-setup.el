(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

(use-package smerge-mode
  :init (setq smerge-command-prefix "\C-cm"))

(use-package magit
  :init (setq magit-define-global-key-bindings t)
  :config
  (setq magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18)))

(provide 'proj-setup)
