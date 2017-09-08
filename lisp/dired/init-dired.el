;;; init-dired.el --- Summary
;;; Commentary:
;;; init dired config
;;; Code:
(setq-default diredp-hide-details-initially-flag nil
              dired-dwim-target t)

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

(after-load 'dired
  (use-package dired+)
  (use-package dired-sort)
  (when (fboundp 'global-dired-hide-details-mode)
    (global-dired-hide-details-mode -1))
  (setq dired-recursive-deletes 'top)
  (setq insert-directory-program "gls" dired-use-ls-dired t)
  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  (use-package diff-hl
    :config
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode)))

(use-package neotree
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-window-width 40)
  :bind (([f8] . neotree-toggle)))

(provide 'init-dired)
;;; init-dired.el ends here
