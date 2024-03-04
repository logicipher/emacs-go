;; -*- lexical-binding: t; -*-


;; (use-package s
;;   :straight t)
(use-package eglot
  :straight t
  :hook ((prog-mode . eglot-ensure))
  :init


  :config
  (add-to-list 'eglot-server-programs
               '((c++-mode c-mode) . ("clangd" "-j=16" "--background-index" "--header-insertion=never" "--all-scopes-completion" "--clang-tidy" "--function-arg-placeholders" "--pch-storage=memory")))

  ;; make eglot highlight with light-grey background
  (set-face-attribute 'eglot-highlight-symbol-face nil
                      :background (if (display-graphic-p)
                                      "gray29"
                                    "color-239"))

  ;;
  ;; Help function to execute all code actions
  (defun lc/eglot-code-actions-all (beg &optional end action-kind interactive)
    "Find LSP code actions of type ACTION-KIND between BEG and END.
Interactively, offer to execute them.
If ACTION-KIND is nil, consider all kinds of actions.
Interactively, default BEG and END to region's bounds else BEG is
point and END is nil, which results in a request for code actions
at point.  With prefix argument, prompt for ACTION-KIND."
    (interactive
     `(,@(eglot--code-action-bounds)
       ,(and current-prefix-arg
             (completing-read "[eglot] Action kind: "
                              '("quickfix" "refactor.extract" "refactor.inline"
                                "refactor.rewrite" "source.organizeImports")))
       t))
    (eglot-server-capable-or-lose :codeActionProvider)
    (let* ((server (eglot--current-server-or-lose))
           (actions
            (eglot--request
             server
             :textDocument/codeAction
             (list :textDocument (eglot--TextDocumentIdentifier)
                   :range (list :start (eglot--pos-to-lsp-position beg)
                                :end (eglot--pos-to-lsp-position end))
                   :context
                   `(:diagnostics
                     [,@(cl-loop for diag in (flymake-diagnostics beg end)
                                 when (cdr (assoc 'eglot-lsp-diag
                                                  (eglot--diag-data diag)))
                                 collect it)]
                     ,@(when action-kind `(:only [,action-kind]))))))
           ;; Redo filtering, in case the `:only' didn't go through.
           (actions (cl-loop for a across actions
                             when (or (not action-kind)
                                      ;; github#847
                                      (string-prefix-p action-kind (plist-get a :kind)))
                             collect a)))
      (if interactive
          (lc/eglot--read-execute-code-action-all actions server action-kind)
        actions)))

  (defun lc/eglot--read-execute-code-action-all (actions server &optional action-kind)
    "Helper for interactive calls to `eglot-code-actions'."
    (progn
      (or (cl-loop for a in actions
                   do (eglot-execute server a))
          (apply #'eglot--error
                 (if action-kind `("No \"%s\" code actions here" ,action-kind)
                   `("No code actions here"))))))
  ;; setup keymap
  ;;
  ;; keymap prefix
  (defvar lc-eglot-keymap-prefix "C-c l")
  (defvar lc-eglot-command-map
    (let ((map (make-sparse-keymap)))
      ;; workspace
      (define-key map "wq" 'eglot-shutdown)
      (define-key map "wr" 'eglot-reconnect)
      (define-key map "ws" 'eglot)

      ;;Toggle
      (define-key map "Th" 'eglot-inlay-hints-mode)

      ;; format
      (define-key map "==" 'eglot-format-buffer)
      (define-key map "=r" 'eglot-format)

      ;; goto
      (define-key map "gd" 'xref-find-definitions)
      (define-key map "gg" 'xref-find-definitions)
      (define-key map "gr" 'xref-find-references)
      ;;action
      (define-key map "aa" 'eglot-code-actions)
      (define-key map "aq" 'eglot-code-actions-quickfix)
      (define-key map "aA" 'lc/eglot-code-actions-all)
      ;; help
      (define-key map "hh" 'eldoc)
      (define-key map "hb" 'flymake-show-buffer-diagnostics)
      ;; rename
      (define-key map "rr" 'eglot-rename)
      map))
  (define-key eglot-mode-map (kbd lc-eglot-keymap-prefix) lc-eglot-command-map)
  (setq eldoc-echo-area-use-multiline-p nil)




  
  (defun lc/eglot-handle-render-for-echo-area (contents &optional _range)
    "Personal handler for extracting a single line form RENDERED, appropriate for
  display in echo area."
    ;; split contents with "---" so we get the last part of hover doc which is
    ;; the signature there are total three parts
    (let ((signature (car (last (split-string (string-trim (eglot--hover-info contents _range)) "---")))))
      ;; remove possible newline in signature
      (string-trim (replace-regexp-in-string "[\n ]+" " " signature))))

  ;; This function is used to display signature in echo area. Its implementation
  ;; mixes the signature extraction method from lsp-mode which just steals the
  ;; signature from hover doc instead of using textDocument/signatureHelp
  (defun lc/eglot-signature-function (cb)
    "A member of `edloc-documentation-functions', for signature"
    (when (eglot-server-capable :hoverProvider)
      (let ((buf (current-buffer)))
        (require 'jsonrpc)
        (jsonrpc-async-request
         (eglot--current-server-or-lose)
         :textDocument/hover (eglot--TextDocumentPositionParams)
         ;; lexical-binding shall be enabled for this file so it can capture outter vairable
         :success-fn (eglot--lambda ((Hover) contents range)
                        (eglot--when-buffer-window buf
                          (let ((info (unless (seq-empty-p contents)
                                        (lc/eglot-handle-render-for-echo-area contents range))))
                            (funcall cb info
                                     :echo (and info (string-match "\n" info))))))
         :deferred :textDocument/hover))
      t))
  (defun lc/eglot-eldoc-hook ()
    (cond
     (eglot--managed-mode
      (unless (eglot--stay-out-of-p 'eldoc)
        ;; since we cannot remove this hook when eglot disconnect, when
        ;; restarting it has to always be added as the first one. But there is
        ;; side-effect when eglot just shuts down but not restart
        (add-hook 'eldoc-documentation-functions #'lc/eglot-signature-function -100 t)))))

  ;; unlike the doc of eglot-managed-mode-hook, it's run only when eglot starts
  ;; to manage the buffer
  (add-hook 'eglot-managed-mode-hook #'lc/eglot-eldoc-hook)
  )

(provide 'eglot-setup)
