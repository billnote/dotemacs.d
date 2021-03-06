;;; init-rust.el --- Rust -*- lexical-binding: t -*-
;;; Commentary:
;;; init rust config
;;; Code:

;; (use-package rust-mode
;;   :init
;;   (use-package racer
;;     :config
;;     (setq racer-rust-src-path
;;           (let* ((sysroot (string-trim
;;                            (shell-command-to-string "rustc --print sysroot")))
;;                  (lib-path (concat sysroot "/lib/rustlib/src/rust/library"))
;;                  (src-path (concat sysroot "/lib/rustlib/src/rust/src")))
;;             (or (when (file-exists-p lib-path) lib-path)
;;                 (when (file-exists-p src-path) src-path)))))
;;   (use-package company)
;;   (use-package flycheck)
;;   (use-package flycheck-rust
;;     :after rust-mode
;;     :config
;;     (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
;;   (use-package cargo)
;;   :config
;;   (setq company-idle-delay 0.2)
;;   (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
;;   (setq rust-format-on-save t)
;;   (add-hook 'rust-mode-hook #'racer-mode)
;;   (add-hook 'racer-mode-hook #'eldoc-mode)
;;   (add-hook 'racer-mode-hook #'company-mode)
;;   (add-hook 'rust-mode-hook #'cargo-minor-mode)
;;   (setq company-tooltip-align-annotations t)
;;   :custom
;;   (rust-format-on-save (executable-find "rustfmt"))
;;   :bind (("TAB" . company-indent-or-complete-common))
;;   :commands lsp)

(use-package rustic
  :init
  ;; to use rustic-mode even if rust-mode also installed
  (setq auto-mode-alist (delete '("\\.rs\\'" . rust-mode) auto-mode-alist))
  (use-package company)
  (use-package flycheck)
  (use-package flycheck-rust
    :after rustic-mode
    :config
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
  (use-package cargo)
  :mode ("\\.rs\\'" . rustic-mode)
  :config
  (setq company-idle-delay 0.6)
  (add-hook 'rustic-mode-hook #'company-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode)
  (add-hook 'rustic-mode-hook #'cargo-minor-mode)
  (add-hook 'rustic-mode-hook #'lsp)
  :custom
  ((rustic-format-trigger 'on-save)
   (rustic-format-on-save nil)
   (rustic-lsp-format t)))

(use-package cargo
  :ensure t
  :hook (rustic-mode . cargo-minor-mode))

(use-package toml-mode :ensure t)

(use-package dap-mode
  :hook
  ((after-init . dap-mode))
  (rustic-mode . (lambda () (require 'dap-gdb-lldb)))
  :config
  (use-package dap-ui
    :ensure nil
    :config
    (dap-ui-mode t))
  (dap-tooltip-mode t)
  (tooltip-mode t))

(use-package which-key
  :config
  (which-key-mode))

(provide 'init-rust)
;;; init-rust.el ends here
