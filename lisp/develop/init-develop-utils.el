;;; init-develop-utils.el --- Summary
;;; Commentary:
;;; init develop utils config
;;; Code:


;; (use-package smartparens
;;   :config
;;   (add-hook 'prog-mode-hook #'smartparens-mode))


(use-package flycheck
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  (use-package flycheck-color-mode-line
    :config
    (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))


(use-package company
  :init
  (setq tab-always-indent 'complete)
  (add-to-list 'completion-styles 'initials t)
  (setq completion-cycle-threshold 5)
  :diminish CMP
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (define-key company-mode-map (kbd "M-/") 'company-complete)
  (define-key company-active-map (kbd "M-/") 'company-select-next)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (setq-default company-backends '((company-capf company-dabbrev-code) company-dabbrev)
                company-dabbrev-other-buffers 'all)
  (use-package company-quickhelp
    :config
    (add-hook 'after-init-hook 'company-quickhelp-mode))
  (after-load 'page-break-lines-mode
    (defvar sanityinc/page-break-lines-on-p nil)
    (make-variable-buffer-local 'sanityinc/page-break-lines-on-p)

    (defun sanityinc/page-break-lines-disable (&rest ignore)
      (when (setq sanityinc/page-break-lines-on-p (bound-and-true-p page-break-lines-mode))
        (page-break-lines-mode -1)))

    (defun sanityinc/page-break-lines-maybe-reenable (&rest ignore)
      (when sanityinc/page-break-lines-on-p
        (page-break-lines-mode 1)))

    (add-hook 'company-completion-started-hook 'sanityinc/page-break-lines-disable)
    (add-hook 'company-completion-finished-hook 'sanityinc/page-break-lines-maybe-reenable)
    (add-hook 'company-completion-cancelled-hook 'sanityinc/page-break-lines-maybe-reenable)))

(defun sanityinc/local-push-company-backend (backend)
  "Add BACKEND to a buffer-local version of `company-backends'."
  (set (make-local-variable 'company-backends)
       (append (list backend) company-backends)))


(use-package yasnippet
  :config
  (setq yas-snippet-dirs
	'("~/.emacs.d/lisp/snippets"))
  (yas-global-mode 1))


(use-package projectile
  :config
  (add-hook 'after-init-hook 'projectile-global-mode)
  (setq-default
   projectile-mode-line
   '(:eval
     (if (file-remote-p default-directory)
         " Pr"
       (format " Pr[%s]" (projectile-project-name))))))


(use-package origami
  :config
  (add-hook 'prog-mode-hook 'origami-mode)
  :bind (:map origami-mode-map
              ("C-c f" . origami-recursively-toggle-node)
              ("C-c F" . origami-toggle-all-nodes)))
;; TODO add bind-key


(use-package imenu-list)


;; config lsp-mode
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-rust-server 'rust-analyzer)
  (setq lsp-clients-lua-language-server-install-dir "~/.emacs.d/tools/lua-language-server/")
  :hook ((rustic-mode . lsp)
         ;;(rust-mode . lsp)
         (go-mode . lsp)
         (lua-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  ;; (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-cargo-load-out-dirs-from-check t)
  :commands lsp
  :config
  (add-hook 'hack-local-variables-hook
            (lambda () (when (derived-mode-p 'rustic-mode) (lsp)))))

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
              ("C-c u" . lsp-ui-imenu)
              ("C-c i" . lsp-ui-doc-focus-frame))
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-delay 0.8)
  (lsp-ui-doc-position 'at-point) ;; top, bottom, or at-point
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-doc-max-width 80)
  (lsp-ui-doc-max-height 20)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-code-actions-prefix "💡")
  :config
  (use-package lsp-ui-flycheck
    :ensure nil
    :after lsp-mode
    :config
    (setq lsp-ui-flycheck-enable t)))

;;(use-package lsp-ui :commands lsp-ui-mode)

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :commands lsp-treemacs-symbol)

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


(provide 'init-develop-utils)
;;; init-develop-utils.el ends here
