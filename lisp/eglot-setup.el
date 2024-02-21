;; -*- lexical-binding: t; -*-


;; (use-package s
;;   :straight t)
(use-package eglot
  :straight t
  :hook ((c++-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (emacs-lisp-mode . eglot-ensure))
  :init
  ;; setup keymap

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
      ;; help
      (define-key map "hh" 'eldoc)
      (define-key map "hb" 'flymake-show-buffer-diagnostics)
      map))

  :config
  (add-to-list 'eglot-server-programs
               '((c++-mode c-mode) . ("clangd" "-j=16" "--background-index" "--header-insertion=never" "--all-scopes-completion" "--clang-tidy" "--function-arg-placeholders" "--pch-storage=memory")))

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
    (unless (eglot--stay-out-of-p 'eldoc)
      (add-hook 'eldoc-documentation-functions #'lc/eglot-signature-function nil t)))
  (add-hook 'eglot-managed-mode-hook #'lc/eglot-eldoc-hook)
  )

(provide 'eglot-setup)
