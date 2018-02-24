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

(provide 'init-develop-utils)
;;; init-develop-utils.el ends here
