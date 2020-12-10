;;; init-rust.el --- Rust -*- lexical-binding: t -*-
;;; Commentary:
;;; init rust config
;;; Code:

(use-package rust-mode
  :init
  (use-package racer
    :config
    (setq racer-rust-src-path
          (let* ((sysroot (string-trim
                           (shell-command-to-string "rustc --print sysroot")))
                 (lib-path (concat sysroot "/lib/rustlib/src/rust/library"))
                 (src-path (concat sysroot "/lib/rustlib/src/rust/src")))
            (or (when (file-exists-p lib-path) lib-path)
                (when (file-exists-p src-path) src-path)))))
  (use-package company)
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
  :custom
  (rust-format-on-save (executable-find "rustfmt"))
  :bind (("TAB" . company-indent-or-complete-common)))

(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

(use-package toml-mode :ensure t)

;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
(setq lsp-key-map-refix "s-l")
(setq lsp-rust-server 'rust-analyzer)

(use-package lsp-mode
  :hook ((rust-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
  :after lsp-mode
  :delight
  :commands lsp-ui-mode
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ("C-c u" . lsp-ui-imenu))
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-delay 0.2)
  (lsp-ui-doc-position 'top) ;; top, bottom, or at-point
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-doc-max-width 80)
  (lsp-ui-doc-max-height 20)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions nil)
  (lsp-ui-sideline-code-actions-prefix "💡")
  :config
  (use-package lsp-ui-flycheck
    :ensure nil
    :after lsp-mode
    :config
    (setq lsp-ui-flycheck-enable t)))

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :commands lsp-treemacs-symbol)

(use-package dap-mode
  :hook
  ((after-init . dap-mode))
  (rust-mode . (lambda () (require 'dap-gdb-lldb)))
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
