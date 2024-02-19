;;
;; This file contains the basic settings for emacs without
;; using any external packages
;;

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode)
;; (global-display-line-numbers-mode t)
(set-face-attribute 'default (selected-frame) :height 150)
(setq inhibit-startup-message t
      cursor-type 'hbar)
(defalias 'yes-or-no-p 'y-or-n-p)

;;
;; we want no tabs, detect and prevent inserting tabs
;;
(setq x-stretch-cursor t)
(setq-default indent-tabs-mode nil)
(setq tab-stop-list (number-sequence 4 120 4))

;;
;; self-defined window operation key bindings
;;
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

;;
;; backup and auto-save files settings
;;
(setq backup-directory-alist '(("" . "~/.emacs-backup")))
(setq make-backup-files t
      backup-by-copying t
      version-control t
      delete-old-versions t
      ;; delete-by-moving-to-trash t
      kept-old-versions 6
      kept-new-versions 9
      auto-save-default t
      auto-save-timeout 20
      auto-save-interval 100 )
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs-backup/" t)))

(define-minor-mode private-mode
  "Define minor mode callled `private-mode'.
For sensitive files like ssh key files,
it disables the backup and auto-saving."
  :init-value nil
  :lighter " Pf"
  (if private-mode
      (progn
	;; disable backup in this buffer
	(set (make-local-variable 'backup-inhibited) t)
	;; disable auto-save
	(if auto-save-default
	    auto-save-mode -1))
    ;; restore backup
    (kill-local-variable 'backup-inhibited)
    ;; restore auto-save
    (if auto-save-default
	auto-save-mode 1)))
(defvar private-files "\\(\\.\\(gpg\\)\\'\\)\\|id_rsa"
  "Files matching this pattern are considered as
sensitive data. Minor `private-mode' is enabled
for them so backup and auto-saving are disabled
for them")

(add-to-list 'auto-mode-alist (cons private-files 'private-mode))


;;
;; some hooks
;;

;; auto complete braces pair
(add-hook 'prog-mode-hook #'electric-pair-mode)

;; better search experience
(setq grep-command "ag --vimgrep"
      grep-use-null-device nil)

;;
;; better compilation buffer
;;
(setq compilation-window-height 10)
(setq compilation-skip-threshold 2)
(setq compilation-scroll-output 'first-error)
(defun lc-compilation-hook ()
  "Make compilation window spilted vertically"
  (progn
    ;; check it's not grep mode since grep-mode is derived from compilation-mode
    ;; when embark-export grep buffer, it tries to split minibuffer which causes
    ;; error
    (unless (or (string= major-mode "grep-mode") (get-buffer-window "*compilation*"))
	(let ((w (split-window-vertically)))
	  (select-window w)
	  (switch-to-buffer "*compilation*"))
      )
    ))
(add-hook 'compilation-mode-hook 'lc-compilation-hook)

;; make compilation aware of ansi color characters
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)


;;
;; browser setting if the system is wsl
;;
(with-eval-after-load 'browse-url
  (when (and (eq system-type 'gnu/linux)
             (string-match "Linux.*[mM]icrosoft.*Linux"
                           (shell-command-to-string "uname -a")))
    (setq
     browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe"
     browse-url-generic-args '("/c" "start")
     browse-url-browser-function #'browse-url-generic))
  )

(setq-default fill-column 80)

(setq enable-recursive-minibuffers t)

(provide 'basic-setup)
