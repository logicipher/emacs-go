(use-package yasnippet
  :straight t
  :hook ((prog-mode . yas-minor-mode)
	 (org-mode . yas-minor-mode)
	 (emacs-lisp-mode . yas-minor-mode))
  :config
  (yas-reload-all)
  (setq yas-snippet-dirs
	'("~/.emacs.d/snippets"           ;; public personal snippets, held by git
	  "~/.emacs.d/private-snippets"   ;; private project-used snippets, not synced
	  )))

;; (use-package yasnippet-snippets)

;; handy function for yas
(defun find-project-root (file-path)
  "Find the root directory of the project by looking for .git or a known root marker, starting from FILE."
  (locate-dominating-file file-path ".git/"))

(defun get-relative-path-to-include-or-src-no-projectile (header-file)
  "Return the relative path of HEADER-FILE to the `include` or `src` directories,
with slashes replaced by underscores, starting from the header file."
  (let* ((project-root (find-project-root header-file))
         (file-path (file-relative-name header-file project-root))
         (relative-path (if (string-match "\\(include\\|src\\|test\\|tests\\)/.*" file-path)
                            (match-string 0 file-path)
                          nil))
         (file-name-without-ext (file-name-sans-extension relative-path)))
    (if file-name-without-ext
        (replace-regexp-in-string "/" "_" file-name-without-ext)
      nil)))


(provide 'snippet-setup)
