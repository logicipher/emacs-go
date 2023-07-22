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

(use-package org
  :config
  ;; for our custom language, we dont need to load it with
  ;; `org-babel-do-load-languages' since this function just `require'
  ;; the feature called ob-* using the car of the pair.
  (require 'org-custom-babel)
  (org-babel-do-load-languages 'org-babel-load-languages '((C . t)))
  )


(provide 'org-setup)
