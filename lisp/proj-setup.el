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

;; set compilation window property
(setq compilation-window-height 10)
(setq compilation-skip-threshold 2)
(setq compilation-scroll-output 'first-error)
(defun lc-compilation-hook ()
  "Make compilation window spilted vertically"
  (progn
    (if (not (get-buffer-window "*compilation*"))
	(let ((w (split-window-vertically)))
	  (select-window w)
	  (switch-to-buffer "*compilation*"))
      )
    ))
(add-hook 'compilation-mode-hook 'lc-compilation-hook)

(provide 'proj-setup)
