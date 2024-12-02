(use-package avy
  :straight t
  ;; :after embark
  :init
  (avy-setup-default)
  :bind
  (("C-:" . avy-goto-word-1)
   ("C-;" . avy-goto-char-timer)
   ("C-c C-j" . avy-resume))
  :config
  (setq avy-timeout-seconds 0.8)

  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark)
  )

(provide 'avy-setup)
