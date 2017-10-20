;;; init-dired.el --- Summary
;;; Commentary:
;;; init dired config
;;; Code:

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

(after-load 'dired
  (use-package dired-sort
    :config
    (setq-default dired-dwim-target t))
  (use-package diredfl
    :config
    (diredfl-global-mode))
  (setq dired-recursive-deletes 'top)
  (use-package all-the-icons-dired
    :config
    (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))
  (use-package diff-hl
    :config
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode))
  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  (define-key dired-mode-map (kbd "C-c C-p") 'wdired-change-to-wdired-mode))

(use-package neotree
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-window-width 40)
  :bind (([f8] . neotree-toggle)))

(provide 'init-dired)
;;; init-dired.el ends here
