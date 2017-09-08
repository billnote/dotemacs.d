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
  :config
  (setq plantuml-jar-path "~/.emacs.d/tools/plantuml/plantuml.jar")
  (add-to-list 'auto-mode-alist '("\\.uml\\'" . plantuml-mode)))


;; crontab
(use-package crontab-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.cron\\(tab\\)?\\'" . crontab-mode))
  (add-to-list 'auto-mode-alist '("cron\\(tab\\)?\\."    . crontab-mode)))


;; textile
(use-package textile-mode
  :config
  (setq auto-mode-alist
        (cons '("\\.textile\\'" . textile-mode) auto-mode-alist)))


;; markdown
(use-package markdown-mode
  :config
  (after-load 'whitespace-cleanup-mode
    (push 'markdown-mdoe whitespace-cleanup-mode-ignore-modes)))


;; csv
(use-package csv-mode
  :config
  (use-package csv-nav)
  (add-auto-mode 'csv-mode "\\.[Cc][Ss][Vv]\\'")
  (setq csv-separators '("," ";" "|" " ")))


(provide 'init-edit-mode)
;;; init-edit-mode.el ends here
