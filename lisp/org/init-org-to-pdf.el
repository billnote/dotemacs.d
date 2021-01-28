;;; init-org-to-pdf.el --- Summary
;;; Commentary:
;;; init org file publish pdf config
;;; Code:
(require 'ox-latex)

(defcustom cn-article-class-file "~/.emacs.d/lisp/org/cn-latex.class"
  "The cn-article latex classes."
  :type 'string
  :group 'org-export)

(defcustom cn-report-class-file "~/.emacs.d/lisp/org/cn-report.class"
  "The cn-article latex classes."
  :type 'string
  :group 'org-export)

(defcustom cn-book-class-file "~/.emacs.d/lisp/org/cn-book.class"
  "The cn-article latex classes."
  :type 'string
  :group 'org-export)

(defcustom cn-beamer-class-file "~/.emacs.d/lisp/org/cn-beamer.class"
  "The cn-article latex classes."
  :type 'string
  :group 'org-export)

(setq org-latex-pdf-process
      '("xelatex -interaction nonstopmode %f"
        "xelatex -interaction nonstopmode %f"))
;; code执行免应答（Eval code without confirm）
(setq org-confirm-babel-evaluate nil)
(setq-default TeX-master nil)
(defun org-mode-article-modes ()
  (reftex-mode t)
  (and (buffer-file-name)
       (file-exists-p (buffer-file-name))
       (reftex-parse-all)))
(add-hook 'org-mode-hook
          (lambda ()
            (if (member "REFTEX" org-todo-keywords-1)
                (org-mode-article-modes))))
(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))

(bh/add-variables-to-list 'org-latex-classes
                          '("cn-article"
                            "{{{cn-article-class}}}
                                   [NO-DEFAULT-PACKAGES]
                                   [NO-PACKAGES]"
                            ("\\section{%s}" . "\\section*{%s}")
                            ("\\subsection{%s}" . "\\subsection*{%s}")
                            ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                            ("\\paragraph{%s}" . "\\paragraph*{%s}")
                            ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                          
                          '("cn-report"
                            "{{{cn-report-class}}}
                                   [NO-DEFAULT-PACKAGES]
                                   [NO-PACKAGES]"
                            ("\\part{%s}" . "\\part*{%s}")
                            ("\\chapter{%s}" . "\\chapter*{%s}")
                            ("\\section{%s}" . "\\section*{%s}")
                            ("\\subsection{%s}" . "\\subsection*{%s}")
                            ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                            ("\\paragraph{%s}" . "\\paragraph*{%s}")
                            ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))

                          '("cn-book"
                            "{{{cn-book-class}}}
                                   [NO-DEFAULT-PACKAGES]
                                   [NO-PACKAGES]"
                            ("\\part{%s}" . "\\part*{%s}")
                            ("\\chapter{%s}" . "\\chapter*{%s}")
                            ("\\section{%s}" . "\\section*{%s}")
                            ("\\subsection{%s}" . "\\subsection*{%s}")
                            ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                            ("\\paragraph{%s}" . "\\paragraph*{%s}")
                            ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))

                          '("cn-beamer"
                            "{{{cn-beamer-class}}}
                                   [NO-DEFAULT-PACKAGES]
                                   [NO-PACKAGES]"
                            ("\\section{%s}" . "\\section*{%s}")
                            ("\\subsection{%s}" . "\\subsection*{%s}")
                            ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                            )
                          )

;; 使用Listings宏包格式化源代码(只是把代码框用listing环境框起来，还需要额外的设置)
(setq org-latex-listings t)
;; Options for \lset command（reference to listing Manual)
(setq org-latex-listings-options
      '(("style" "codestyle")))
;; Make Org use ido-completing-read for most of its completing prompts.
(setq org-completion-use-ido t)

(defun replace-cn-class (contents backend info)
  "Replace cn article marco.
CONTENTS source string.
BACKEND backend.
INFO info."
  (interactive)
  (when (org-export-derived-backend-p backend 'latex)
    (cond ((string-match "{{{cn-article-class}}}" contents)
           (replace-regexp-in-string "{{{cn-article-class}}}" (f-read-text cn-article-class-file) contents))
          ((string-match "{{{cn-report-class}}}" contents)
           (replace-regexp-in-string "{{{cn-report-class}}}" (f-read-text cn-report-class-file) contents))
          ((string-match "{{{cn-book-class}}}" contents)
           (replace-regexp-in-string "{{{cn-book-class}}}" (f-read-text cn-book-class-file) contents))
          ((string-match "{{{cn-beamer-class}}}" contents)
           (replace-regexp-in-string "{{{cn-beamer-class}}}" (f-read-text cn-beamer-class-file) contents))
          )))

(defun bn/replace-cn-class-contents (contents &rest valpair)
  "Replace marco, CONTENTS source string, VALPAIR marco and class file pair."
  (while valpair
    (let ((val (pop valpair)))
      (bh/replace-contents (car val) (cdr val) contents))))

(defun bh/replace-contents (macro class-file contents)
  "Use CLASS-FILE content replace MACRO in the CONTENTS."
  (when (string-match macro contents)
    (replace-regexp-in-string macro (f-read-text class-file) contents)))

(add-to-list 'org-export-filter-final-output-functions 'replace-cn-class)

(setq ps-paper-type 'a4
      ps-font-size 16.0
      ps-print-header nil
      ps-landscape-mode nil)


(provide 'init-org-to-pdf)
;;; init-org-to-pdf.el ends here
