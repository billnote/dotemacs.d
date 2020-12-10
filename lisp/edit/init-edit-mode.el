;;; init-edit-mode.el --- Summary
;;; Commentary:
;;; init edit major mode config
;;; Code:

;; dot
(use-package graphviz-dot-mode
  :config (add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode)))


;; gnuplot
(use-package gnuplot
  :config
  (use-package gnuplot-mode))


;; thrift
(use-package thrift
  :config
  (add-to-list 'auto-mode-alist '("\\.thrift\\'" . thrift-mode)))


;; protobuf
(use-package protobuf-mode
  :init
  (defconst protobuf-style
    '((c-basic-offset . 2)
      (indent-tabs-mode . nil)))
  :config
  (add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))
  (add-hook 'protobuf-mode-hook
            (lambda () (c-add-style "pb-style" protobuf-style t))))


;; plantuml

(use-package plantuml-mode
  :after org
  :config
  (let ((url "https://jaist.dl.sourceforge.net/project/plantuml/plantuml.jar"))
    (setq plantuml-jar-path (expand-file-name "plantuml.jar" "~/.emacs.d/tools/plantuml/"))
    (setq org-plantuml-jar-path plantuml-jar-path)
    (unless (file-exists-p plantuml-jar-path)
      (url-copy-file url plantuml-jar-path)))
  (setq plantuml-default-exec-mode 'jar)
  (add-to-list
   'org-src-lang-modes '("plantuml" . plantuml))  
  (add-to-list 'auto-mode-alist '("\\.uml\\'" . plantuml-mode)))


;; crontab

;; (use-package crontab-mode
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.cron\\(tab\\)?\\'" . crontab-mode))
;;   (add-to-list 'auto-mode-alist '("cron\\(tab\\)?\\."    . crontab-mode)))


;; textile
(use-package textile-mode
  :config
  (setq auto-mode-alist
        (cons '("\\.textile\\'" . textile-mode) auto-mode-alist)))


;; markdown
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config
  (after-load 'whitespace-cleanup-mode
    (push 'markdown-mdoe whitespace-cleanup-mode-ignore-modes)))


;; csv
(use-package csv-mode
  :config
  (add-auto-mode 'csv-mode "\\.[Cc][Ss][Vv]\\'")
  (setq csv-separators '("," ";" "|" " ")))


;; yaml
(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
  :bind (:map yaml-mode-map
              ("\C-m" . newline-and-indent)))


;; dockerfile
(use-package dockerfile-mode
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))


(provide 'init-edit-mode)
;;; init-edit-mode.el ends here
