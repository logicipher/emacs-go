;;
;; org mode setup
;;
(add-hook 'org-mode-hook #'visual-line-mode)
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
  (custom-set-variables '(org-agenda-prefix-format
    '((agenda  . " %i %-12:c%?-12t% s")
      (todo  . " %l%l %i %-12:c")
      (tags  . " %i %-12:c")
      (search . " %i %-12:c"))))
    (add-to-list 'org-export-backends 'md)
  )


(use-package pdf-tools
  :defer t
  :config
  (pdf-loader-install)
  (setq pdf-view-midnight-colors '("#000000" . "#dbeedd")))

(provide 'org-setup)
