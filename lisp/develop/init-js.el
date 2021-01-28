;;; init-js.el --- Support for Javascript and derivatives -*- lexical-binding: t -*-
;;; Commentary:
;;; init js mode config
;;; Code:


(use-package json-mode)

(use-package js2-mode
  :init
  (setq-default js-indent-level 2
                js2-bounce-indent-p nil)
  :config
  (add-to-list 'auto-mode-alist '("\\.\\(js\\|es6\\)\\(\\.erb\\)?\\'" . js-mode))
  ;; Disable js2 mode's syntax error highlighting by default...
  (autoload 'flycheck-get-checker-for-buffer "flycheck")
  (defun sanityinc/disable-js2-checks-if-flycheck-active ()
    (unless (flycheck-get-checker-for-buffer)
      (setq-local js2-mode-show-parse-errors t)
      (setq-local js2-mode-show-strict-warnings t)
      (when (derived-mode-p 'js-mode)
        (js2-minor-mode 1))))
  (add-hook 'js2-mode-hook 'sanityinc/disable-js2-checks-if-flycheck-active)
  (add-hook 'js2-mode-hook (lambda () (setq mode-name "JS2")))
  (js2-imenu-extras-setup)
  ;; Javascript nests {} and () a lot, so I find this helpful
  (when (executable-find "ag")
    (use-package xref-js2)
    :conifg
    (add-hook 'js2-mode-hook
              (lambda () (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
    :bind (:map js2-mode-map
                ("M-." . nil))))


(use-package coffee-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.coffee\\.erb\\'" . coffee-mode)))


(use-package js-comint
  :config
  (define-minor-mode inferior-js-keys-mode
    "Bindings for communicating with an inferior js interpreter."
    nil " InfJS" inferior-js-minor-mode-map)
  (dolist (hook '(js2-mode-hook js-mode-hook))
    (add-hook hook 'inferior-js-keys-mode))
  (setq inferior-js-program-command "js")
  :bind (:map inferior-js-minor-mode-map
              ("C-x C-e" . js-send-last-sexp)
              ("C-M-x" . js-send-last-sexp-and-go)
              ("C-c b" . js-send-buffer)
              ("C-c C-b" . js-send-buffer-and-go)
              ("C-c l" . js-load-file-and-go)))


(use-package skewer-mode
  :config
  (add-hook 'skewer-mode-hook
            (lambda () (inferior-js-keys-mode -1))))



;; mocha test
(use-package mocha)

(provide 'init-js)
;;; init-js.el ends here
