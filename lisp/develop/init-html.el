;;; init-html.el --- Summary
;;; Commentary:
;;; init html config
;;; Code:

(use-package tidy
  :config (add-hook 'html-mode-hook (lambda () (tidy-build-menu html-mode-map))))

(use-package tagedit
  :config
  (after-load 'sgml-mode
    (tagedit-add-paredit-like-keybindings)
    (add-hook 'sgml-mode-hook (lambda () (tagedit-mode 1))))
  (add-auto-mode 'html-mode "\\.\\(jsp\\|tmpl\\)\\'"))


;;; ERB
(use-package mmm-mode)

(defun sanityinc/ensure-mmm-erb-loaded ()
  (require 'mmm-erb))

(require 'derived)

(defun sanityinc/set-up-mode-for-erb (mode)
  (add-hook (derived-mode-hook-name mode) 'sanityinc/ensure-mmm-erb-loaded)
  (mmm-add-mode-ext-class mode "\\.erb\\'" 'erb))

(let ((html-erb-modes '(html-mode html-erb-mode nxml-mode)))
  (dolist (mode html-erb-modes)
    (sanityinc/set-up-mode-for-erb mode)
    (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?\\'" 'html-js)
    (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?\\'" 'html-css)))

(mapc 'sanityinc/set-up-mode-for-erb
      '(coffee-mode js-mode js2-mode js3-mode markdown-mode textile-mode))

(mmm-add-mode-ext-class 'html-erb-mode "\\.jst\\.ejs\\'" 'ejs)

(add-auto-mode 'html-erb-mode "\\.rhtml\\'" "\\.html\\.erb\\'")
(add-to-list 'auto-mode-alist '("\\.jst\\.ejs\\'"  . html-erb-mode))
(mmm-add-mode-ext-class 'yaml-mode "\\.yaml\\(\\.erb\\)?\\'" 'erb)

(dolist (mode (list 'js-mode 'js2-mode 'js3-mode))
  (mmm-add-mode-ext-class mode "\\.js\\.erb\\'" 'erb))


(provide 'init-html)
;;; init-html.el ends here
