;;; init-translate.el --- Summary
;;; Commentary:
;;; init translate config
;;; Code:
(use-package popup)
(use-package youdao-dictionary
  :bind
  (("C-c t" . youdao-dictionary-search-at-point+)
   ("C-c T" . youdao-dictionary-search-and-replace)))

(provide 'init-translate)
;;; init-translate.el ends here
