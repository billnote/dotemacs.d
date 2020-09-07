;;; init-mode-line-themes.el --- Summary
;;; Commentary:
;;; init mode line themes
;;; Code:
(use-package spaceline
  :pin melpa-stable
  :demand t)
(use-package spaceline-config
  :ensure nil
  :after spaceline
  :config
  (spaceline-toggle-buffer-encoding-on)
  (spaceline-toggle-input-method-on)
  )

(use-package spaceline-all-the-icons
  :after spaceline
  :init
  (use-package all-the-icons)
  (use-package git-gutter)
  (use-package anzu)
  (use-package fancy-battery
    :config
    (fancy-battery-mode))
  (use-package he-weather
    :config
    (setq he-weather-location "上海")
    (setq he-weather-update-interval 3600)
    (he-weather-mode))
  :config
  (spaceline-all-the-icons-theme)
  (spaceline-all-the-icons--setup-anzu)            ;; Enable anzu searching
  (spaceline-all-the-icons--setup-package-updates) ;; Enable package update indicator
  (spaceline-all-the-icons--setup-git-ahead)       ;; Enable # of commits ahead of upstream in git
  (spaceline-all-the-icons--setup-paradox)         ;; Enable Paradox mode line
  (spaceline-all-the-icons--setup-neotree)         ;; Enable Neotree mode line
  (setq spaceline-all-the-icons-separator-type 'wave)
  (spaceline-toggle-all-the-icons-bookmark-on)
  (spaceline-toggle-all-the-icons-dedicated-on)
  (spaceline-toggle-all-the-icons-fullscreen-on)
  (spaceline-toggle-all-the-icons-buffer-position-on)
  (spaceline-toggle-all-the-icons-narrowed-on)
  (spaceline-toggle-all-the-icons-hud-on)
  (spaceline-toggle-all-the-icons-battery-status-on)
  (spaceline-toggle-all-the-icons-weather-on)
  (spaceline-toggle-all-the-icons-git-status)
  (spaceline-toggle-all-the-icons-flycheck-status-off)
  (spaceline-toggle-all-the-icons-flycheck-status-info-off)
  (spaceline-toggle-all-the-icons-multiple-cursors-on)
  (spaceline-all-the-icons-theme 'buffer-encoding 'input-method)
  )

(provide 'init-mode-line-themes)
;;; init-mode-line-themes.el ends here
