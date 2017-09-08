;;; init-rust.el --- Summary
;;; Commentary:
;;; init rust config
;;; Code:

(use-package rust-mode
  :init
  (use-package racer)
  (use-package company)
  (use-package company-racer)
  (use-package flycheck)
  (use-package flycheck-rust)
  (use-package cargo)
  :config
  (setq company-idle-delay 0.2)
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
  (setq rust-format-on-save t)
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode)
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  (add-hook 'rust-mode-hook #'cargo-minor-mode)
  (setq company-tooltip-align-annotations t)
  :bind (("TAB" . company-indent-or-complete-common)))

(provide 'init-rust)
;;; init-rust.el ends here
