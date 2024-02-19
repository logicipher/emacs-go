(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package all-the-icons
  :straight t
  :if (display-graphic-p))
;; (use-package nerd-icons
;;   :straight t)

;;themes
(use-package doom-themes
 :straight t
 :config
 (setq doom-themes-enable-bold t
       doom-themes-enable-italic t)
 (load-theme 'doom-gruvbox t))
;(use-package doom-modeline
;  :straight t
;  :init (doom-modeline-mode 1))

;; (use-package spacemacs-theme
;;   :config
;;   (setq spacemacs-theme-comment-italic t)
;;   (setq spacemacs-theme-keyword-italic t)
;;   (load-theme 'spacemacs-dark t))

(use-package telephone-line
  :straight t
  :init (telephone-line-mode 1))

(use-package which-key
  :straight t
  :init
  (setq which-key-show-early-on-C-h t
        which-key-idle-delay 5
        which-key-idle-secondary-delay 0.2)
  :config (which-key-mode)
  :diminish which-key-mode)

(provide 'ui-setup)
