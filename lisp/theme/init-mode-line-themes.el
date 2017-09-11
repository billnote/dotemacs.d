;;; init-mode-line-themes.el --- Summary
;;; Commentary:
;;; init mode line themes
;;; Code:

(use-package spaceline-all-the-icons
  :after spaceline
  :init
  (use-package powerline)
  (use-package spaceline)
  (use-package all-the-icons)
  (use-package git-gutter)
  (use-package anzu)
  (use-package fancy-battery
    :config
    (fancy-battery-mode))
  (use-package yahoo-weather
    :config
    (setq yahoo-weather-location "上海")
    (setq yahoo-weather-update-interval 7200)
    (yahoo-weather-mode))
  :config

  (spaceline-all-the-icons--setup-anzu)            ;; Enable anzu searching
  (spaceline-all-the-icons--setup-package-updates) ;; Enable package update indicator
  (spaceline-all-the-icons--setup-git-ahead)       ;; Enable # of commits ahead of upstream in git
  (spaceline-all-the-icons--setup-paradox)         ;; Enable Paradox mode line
  (spaceline-all-the-icons--setup-neotree)         ;; Enable Neotree mode line
  (setq spaceline-all-the-icons-separator-type 'wave)
  (spaceline-all-the-icons-theme)
  (spaceline-toggle-all-the-icons-bookmark-on)
  (spaceline-toggle-all-the-icons-dedicated-on)
  (spaceline-toggle-all-the-icons-fullscreen-on)
  (spaceline-toggle-all-the-icons-buffer-position-on)
  (spaceline-toggle-all-the-icons-narrowed-on)
  (spaceline-toggle-all-the-icons-buffer-position-on)
  (spaceline-toggle-all-the-icons-hud-on)
  (spaceline-toggle-all-the-icons-battery-status-on)
  (spaceline-toggle-all-the-icons-weather-on)
  (spaceline-toggle-all-the-icons-git-status-on)
  (spaceline-toggle-all-the-icons-flycheck-status-on)
  (spaceline-toggle-all-the-icons-flycheck-status-info-on)
  
  (spaceline-define-segment buffer-encoding
    "The full `buffer-file-coding-system'."
    (format "%s" buffer-file-coding-system))
  (spaceline-all-the-icons-theme 'buffer-encoding))


(provide 'init-mode-line-themes)
;;; init-mode-line-themes.el ends here
