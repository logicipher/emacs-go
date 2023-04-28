;; use separate init file
(setq user-init-file (expand-file-name "user.el" user-emacs-directory))
(when (file-exists-p user-init-file)
  (load user-init-file))
