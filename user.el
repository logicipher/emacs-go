(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'basic-setup)
(require 'prepare-setup)
(require 'ui-setup)
(require 'window-setup)
(require 'lsp-setup)
(require 'helm-setup)
(require 'proj-setup)
(require 'lang-setup)
(require 'snippet-setup)

(use-package tramp
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
)

