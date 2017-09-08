;;; init-python.el --- Summary
;;; Commentary:
;;; init python config
;;; Code:

(add-to-list 'auto-mode-alist '("SCon(struct|script)\\'" . python-mode))
(use-package pip-requirements)
(use-package anaconda-mode
  :config
  (after-load 'python
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode))
  (use-package company-anaconda
    :config
    (after-load 'company
      (add-hook 'python-mode-hook
                (lambda (sanityinc/local-push-company-backend 'company-anaconda))))))

(provide 'init-python)
;;; init-python.el ends here
