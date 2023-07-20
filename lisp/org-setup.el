;;
;; org mode setup
;;
(use-package org-preview-html
  ;; :hook (org-mode . org-preview-html-mode)
  :config
  (setq
   org-preview-html-refresh-configuration 'save
   ;;org-preview-html-viewer 'xwidget
   ))

(provide 'org-setup)
