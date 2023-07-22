;;
;; This file contains babel support for customized languages
;;


;;
;; C++, support output assembly to the results
;; new language: cpp-asm
;;
(require 'ob-C)
(defconst org-babel-header-args:cpp-asm
  org-babel-header-args:C++)

(defun org-babel-expand-body:cpp-asm (body params)
  "Expand a block of C++ code with org-babel according to its header arguments."
  (org-babel-C-expand-C++ body params))

(defun org-babel-execute:cpp-asm (body params)
  "Compile a block of C++ code and generate an assembly file."
  (cpp-asm-execute body params))

(defun cpp-asm-execute (body params)
  "The worker of compiling c++ codes into assembly."
  (let* ((tmp-src-file (org-babel-temp-file
                        "C-src-" ".cpp"))
         (tmp-asm-file
          (org-babel-process-file-name
           (org-babel-temp-file "C-asm-" ".s")))
         (flags (cdr (assq :flags params)))
         (flags (mapconcat 'identity
                           (if (listp flags) flags (list flags)) " "))
         (libs (org-babel-read
                (or (cdr (assq :libs params))
                    (org-entry-get nil "libs" t))
                nil))
         (libs (mapconcat #'identity
                          (if (listp libs) libs (list libs))
                          " "))
         (full-body
          (org-babel-C-expand-C++ body params)))
    (with-temp-file tmp-src-file (insert full-body))
    (org-babel-eval
     (format "%s -S -o %s %s %s %s" ;; -S make sure emitting assembly
             org-babel-C++-compiler
             tmp-asm-file
             flags
             (org-babel-process-file-name tmp-src-file)
             libs)
     "")
    (with-temp-buffer             ;; read assembly file into a string and return
      (insert-file-contents tmp-asm-file)
      (buffer-string))))

;; make cpp-asm have c++-mode in org-edit-src-code
(add-to-list 'org-src-lang-modes '("cpp-asm" . c++))


(provide 'org-custom-babel)
