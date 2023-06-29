(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode)
;; (global-display-line-numbers-mode t)
(set-face-attribute 'default (selected-frame) :height 150)
(setq inhibit-startup-message t
      cursor-type 'bar)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq backup-directory-alist '(("" . "~/.emacs-backup")))

;; Define package repositories
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; use use-package to simplify the config
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

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

(use-package tree-sitter
  :init
  (global-tree-sitter-mode)
  :hook (tree-sitter-after-on . tree-sitter-hl-mode))
(use-package tree-sitter-langs
  :after tree-sitter)

(use-package telephone-line
  :ensure t
  :init (telephone-line-mode 1))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode)

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

(use-package cmake-font-lock)

(setq helm-file (expand-file-name "helm.el" user-emacs-directory))
(when (file-exists-p helm-file)
  (load helm-file))
(setq ivy-file (expand-file-name "ivy.el" user-emacs-directory))
;(when (file-exists-p ivy-file)
;  (load ivy-file))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((c++-mode . lsp-mode)
         (c-mode . lsp-mode)
         ;; (elisp-mode . lsp-mode)
         ;; (emacs-lisp-mode . lsp-mode)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-clients-clangd-args '("-j=8" "--background-index" "--header-insertion=never" "--all-scopes-completion" "--clang-tidy" "--function-arg-placeholders" "--pch-storage=memory")))

(use-package lsp-ui
  :config
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-update-mode 'line)
  (setq lsp-ui-sideline-show-code-actions t)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  :commands lsp-ui-mode
  ;:bind (:map lsp-ui-mode-map
        ;      ([remap xref-find-references] . lsp-ui-peek-find-references)
        ;      ([remap xref-find-definitions] . lsp-ui-peek-find-definitions))
  )

(use-package flycheck
  :ensure t
  :hook ((c++-mode . flycheck-mode)
	 (c-mode . flycheck-mode)))


;;(use-package corfu
;;  :after lsp-mode
;;  :custom
;;  (corfu-cycle t)
;;  (corfu-auto t)
;;  :init (global-corfu-mode))

(use-package company
  :after lsp-mode
  :bind (:map company-active-map
         ("<tab>" . company-compelete-common-or-cycle))
  :init
  (global-company-mode))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "pandoc"))

;;
;; some hooks
;;

;; auto complete braces pair
(add-hook 'prog-mode-hook #'electric-pair-mode)

(defcustom lc-window-command-prefix (kbd "C-x w")
  "Prefix for window operation commands."
  :type 'string
  :group 'lc)
(defvar lc-window-cmd-help-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<left>") #'shrink-window-horizontally)
    (define-key map (kbd "<right>") #'enlarge-window-horizontally)
    (define-key map (kbd "<up>") #'enlarge-window)
    (define-key map (kbd "<down>") #'shrink-window)
    (define-key map (kbd "h") #'windmove-left)
    (define-key map (kbd "j") #'windmove-down)
    (define-key map (kbd "k") #'windmove-up)
    (define-key map (kbd "l") #'windmove-right)
    map)
  "Keymap for self-use window commands after `lc-window-command-prefix'")
(fset 'lc-window-cmd-help-map lc-window-cmd-help-map)
(global-set-key lc-window-command-prefix lc-window-cmd-help-map)

;; set compilation window property
(setq compilation-window-height 10)
(setq compilation-scroll-output 'first-error)
(defun lc-compilation-hook ()
  "Make compilation window spilted vertically"
  (progn
    (if (not (get-buffer-window "*compilation*"))
        (progn
          (split-window-vertically)
          ))
    ))
(add-hook 'compilation-mode-hook 'lc-compilation-hook)
