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

;; (use-package lsp-mode
;;   :init
;;   (add-hook 'prog-mode-hook 'lsp-mode)
;;   :config
;;   (require 'lsp-imenu)
;;   (add-hook 'lsp-after-open-hook #'lsp-enable-imenu)
;;   (use-package lsp-ui
;;     :ensure f ; comes with lsp-mode
;;     :after flycheck
;;     :config
;;     (add-hook 'lsp-mode-hook 'lsp-ui-mode))
;;   (add-hook 'prog-major-mode #'lsp-prog-major-mode-enable))

;; (with-eval-after-load 'lsp-mode
;;   (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls"))
;;   (require 'lsp-rust))
;; (add-hook 'rust-mode-hook #'lsp-rust-enable)
;; (add-hook 'rust-mode-hook #'flycheck-mode)

;; (use-package lsp-rust
;;   :defer t
;;   :after lsp-mode
;;   :config
;;   (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls"))
;;   (add-hook 'rust-mode-hook #'lsp-rust-enable)
;;   (add-hook 'rust-mode-hook #'flycheck-mode))

(provide 'init-rust)
;;; init-rust.el ends here
