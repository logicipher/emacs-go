(setq backup-directory-alist '(("" . "~/.emacs-backup")))

;;
;; some hooks
;;

;; auto complete braces pair
(add-hook 'prog-mode-hook #'electric-pair-mode)

(provide 'misce-setup)
