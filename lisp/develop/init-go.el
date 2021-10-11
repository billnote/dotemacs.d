;;; init-go.el --- Summary
;;; Commentary:
;;; init go lang config
;;; Code:
(use-package go-mode
  :init
  (use-package flymake)
  (use-package go-errcheck)
  (use-package go-eldoc)
  (use-package go-snippets)
  (use-package company-go)
  :config
  (use-package flymake-go)
  (use-package dap-go)
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  (add-hook 'go-mode-hook 'company-mode)
  (add-hook 'go-mode-hook (lambda ()  (set (make-local-variable 'company-backends) '(company-go))
                            (company-mode)))
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'flymake-mode-hook
            (lambda()
              (local-set-key (kbd "C-c C-e n") 'flymake-goto-next-error)))
  (add-hook 'flymake-mode-hook
            (lambda()
              (local-set-key (kbd "C-c C-e p") 'flymake-goto-prev-error)))
  (add-hook 'flymake-mode-hook
            (lambda()
              (local-set-key (kbd "C-c C-e m") 'flymake-popup-current-error-menu)))

  :bind (:map go-mode-map
              ("C-c C-r" . go-remove-unused-imports)
              ("C-c i" . go-goto-imports)
              ("M-." . godef-jump)))

(provide 'init-go)
;;; init-go.el ends here
