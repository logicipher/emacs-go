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
 (load-theme 'doom-nord t))
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
  :init (which-key-mode)
  :diminish which-key-mode)

(provide 'ui-setup)
