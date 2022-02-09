;;; init-mode-line-themes.el --- Summary
;;; Commentary:
;;; init mode line themes
;;; Code:

(use-package spaceline)
(use-package spaceline-config
  :ensure nil
  :config
  (setq-default
   powerline-text-scale-factor 1.0))
(use-package spaceline)
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
  ;; (he-weather-mode)
  )

(use-package spaceline-all-the-icons
  :after spaceline
  :config
  (setq spaceline-all-the-icons-hide-long-buffer-path t)
  (setq spaceline-all-the-icons-separator-scale 1.8)
  (spaceline-all-the-icons-theme)
  (spaceline-all-the-icons--setup-anzu)            ;; Enable anzu searching
  (spaceline-all-the-icons--setup-package-updates) ;; Enable package update indicator
  (spaceline-all-the-icons--setup-git-ahead)       ;; Enable # of commits ahead of upstream in git
  (spaceline-all-the-icons--setup-paradox)         ;; Enable Paradox mode line
  (spaceline-all-the-icons--setup-neotree)         ;; Enable Neotree mode line
  (setq spaceline-all-the-icons-separator-type 'slant)

  (spaceline-toggle-all-the-icons-bookmark-on)
  (spaceline-toggle-all-the-icons-battery-status-on)
  (spaceline-toggle-all-the-icons-buffer-position-on)
  (spaceline-toggle-all-the-icons-buffer-id-on)
  (spaceline-toggle-all-the-icons-buffer-path-off)
  (spaceline-toggle-all-the-icons-buffer-size-on)
  (spaceline-toggle-all-the-icons-dedicated-on)
  (spaceline-toggle-all-the-icons-eyebrowse-workspace-on)
  (spaceline-toggle-all-the-icons-flycheck-status-on)
  (spaceline-toggle-all-the-icons-flycheck-status-info-on)
  (spaceline-toggle-all-the-icons-fullscreen-on)
  ;; TOOD need fix mode-line can not display error
  (spaceline-toggle-all-the-icons-git-ahead-off)
  (spaceline-toggle-all-the-icons-git-status-on)
  (spaceline-toggle-all-the-icons-hud-on)
  ;; TODO need fix some bugs about invalid face attrubite :family
  (spaceline-toggle-all-the-icons-mode-icon-off)
  ;; TOOD need fix mode-line can not display error
  (spaceline-toggle-all-the-icons-minor-modes-off)
  (spaceline-toggle-all-the-icons-modified-on)
  (spaceline-toggle-all-the-icons-multiple-cursors-on)
  (spaceline-toggle-all-the-icons-narrowed-on)
  (spaceline-toggle-all-the-icons-neotree-close-bracket-on)
  (spaceline-toggle-all-the-icons-neotree-context-on)
  (spaceline-toggle-all-the-icons-neotree-dirs-on)
  (spaceline-toggle-all-the-icons-neotree-files-on)
  (spaceline-toggle-all-the-icons-neotree-index-on)
  (spaceline-toggle-all-the-icons-neotree-open-bracket-on)
  (spaceline-toggle-all-the-icons-nyan-cat-on)
  (spaceline-toggle-all-the-icons-org-clock-current-task-on)
  (spaceline-toggle-all-the-icons-package-updates-on)
  (spaceline-toggle-all-the-icons-paradox-filter-on)
  (spaceline-toggle-all-the-icons-paradox-line-count-on)
  (spaceline-toggle-all-the-icons-paradox-status-installed-on)
  (spaceline-toggle-all-the-icons-paradox-status-new-on)
  (spaceline-toggle-all-the-icons-paradox-status-upgrade-on)
  (spaceline-toggle-all-the-icons-paradox-total-on)
  (spaceline-toggle-all-the-icons-region-info-on)
  (spaceline-toggle-all-the-icons-sunrise-off)
  (spaceline-toggle-all-the-icons-sunset-off)
  (spaceline-toggle-all-the-icons-temperature-off)
  (spaceline-toggle-all-the-icons-text-scale-on)
  (spaceline-toggle-all-the-icons-time-on)
  (spaceline-toggle-all-the-icons-vc-icon-off)
  (spaceline-toggle-all-the-icons-vc-status-off)
  (spaceline-toggle-all-the-icons-weather-off)
  (spaceline-toggle-all-the-icons-which-function-on)
  (spaceline-toggle-all-the-icons-window-number-on)

  (spaceline-all-the-icons-theme 'buffer-encoding 'input-method)
  )

(provide 'init-mode-line-themes)
;;; init-mode-line-themes.el ends here
