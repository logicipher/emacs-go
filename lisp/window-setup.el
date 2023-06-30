;;
;; features for window management and properties

(use-package ace-window
  :bind (("C-x o" . ace-window)))

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


(provide 'window-setup)
