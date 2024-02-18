(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

; (use-package all-the-icons)
(use-package nerd-icons
  :ensure t)

;;themes
(use-package doom-themes
 :ensure t
 :config
 (setq doom-themes-enable-bold t
       doom-themes-enable-italic t)
 (load-theme 'doom-gruvbox t))
;(use-package doom-modeline
;  :ensure t
;  :init (doom-modeline-mode 1))

;; (use-package spacemacs-theme
;;   :config
;;   (setq spacemacs-theme-comment-italic t)
;;   (setq spacemacs-theme-keyword-italic t)
;;   (load-theme 'spacemacs-dark t))

(use-package telephone-line
  :ensure t
  :init (telephone-line-mode 1))

(use-package which-key
  :init
  (setq which-key-show-early-on-C-h t
        which-key-idle-delay 5
        which-key-idle-secondary-delay 0.2)
  :config (which-key-mode)
  :diminish which-key-mode)

(provide 'ui-setup)
