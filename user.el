(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'prepare-setup)

(require 'misce-setup)

(require 'ui-setup)

(require 'window-setup)

(require 'lsp-setup)

(require 'helm-setup)

(require 'proj-setup)

(require 'lang-setup)


(setq helm-file (expand-file-name "helm.el" user-emacs-directory))
;; (when (file-exists-p helm-file)
;;   (load helm-file))
(setq ivy-file (expand-file-name "ivy.el" user-emacs-directory))
;(when (file-exists-p ivy-file)
;  (load ivy-file))



