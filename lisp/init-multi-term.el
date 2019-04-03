;;; init-multi-term.el --- Summary
;;; Commentary:
;;; config multi-term
;;; Code:

(use-package multi-term
  :config
  (setq multi-term-program "/usr/local/Cellar/fish/2.6.0/bin/fish")
  (set-face-attribute :font "Fira Mono for Powerline"))

(provide 'init-multi-term)
;;; init-multi-term.el ends here
