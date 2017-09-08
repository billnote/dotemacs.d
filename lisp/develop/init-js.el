;;; init-js.el --- Summary
;;; Commentary:
;;; init js mode config
;;; Code:


(defcustom preferred-javascript-mode
  (first (remove-if-not #'fboundp '(js2-mode js-mode)))
  "Javascript mode to use for .js files."
  :type 'symbol
  :group 'programming
  :options '(js2-mode js-mode))

(defconst preferred-javascript-indent-level 2)

;; Need to first remove from list if present, since elpa adds entries too, which
;; may be in an arbitrary order
(eval-when-compile (require 'cl))
(setq auto-mode-alist (cons `("\\.\\(js\\|es6\\)\\(\\.erb\\)?\\'" . ,preferred-javascript-mode)
                            (loop for entry in auto-mode-alist
                                  unless (eq preferred-javascript-mode (cdr entry))
                                  collect entry)))


(use-package json-mode)

(use-package js2-mode
  :init
  (setq-default js2-basic-offset 2
                js2-bounce-indent-p nil)
  :config
  ;; Disable js2 mode's syntax error highlighting by default...
  (autoload 'flycheck-get-checker-for-buffer "flycheck")
  (defun sanityinc/disable-js2-checks-if-flycheck-active ()
    (unless (flycheck-get-checker-for-buffer)
      (set (make-local-variable 'js2-mode-show-parse-errors) t)
      (set (make-local-variable 'js2-mode-show-strict-warnings) t)))
  (add-hook 'js2-mode-hook 'sanityinc/disable-js2-checks-if-flycheck-active)
  (add-hook 'js2-mode-hook (lambda () (setq mode-name "JS2")))
  (js2-imenu-extras-setup)
  (setq-default js-indent-level preferred-javascript-indent-level)
  (add-to-list 'interpreter-mode-alist (cons "node" preferred-javascript-mode))
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
  (setq coffee-js-mode preferred-javascript-mode
        coffee-tab-width preferred-javascript-indent-level)
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


(use-package tern
  :config
  (use-package company-tern)
  :config
  (add-to-list 'company-backends 'company-tern)
  (setq company-tern-meta-as-single-line t)
  (after-load 'context-coloring
    '(tern-context-coloring-setup)))


;; mocha test
(use-package mocha)

(provide 'init-js)
;;; init-js.el ends here
