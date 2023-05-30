;;; init-eaf.el --- Summary
;;; Commentary:
;;; init emacs-applocation-framework
;;; Code:

(use-package eaf
  :load-path "~/.emacs.d/emacs-application-framework"
  :custom
  ;; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker t)
  (browse-url-browser-function 'eaf-open-browser)
  :config
  (use-package eaf-terminal)
  (use-package eaf-image-viewer)
  (use-package eaf-browser)
  (use-package eaf-markdown-previewer)
  (use-package eaf-pdf-viewer)
  (use-package eaf-mindmap)
  (use-package eaf-org-previewer)
  (use-package eaf-demo)
  (defalias 'browse-web #'eaf-open-browser)
  (setq eaf-mindmap-dark-mode nil) ; default option
  (setq eaf-browser-dark-mode nil)
  (setq eaf-terminal-dark-mode nil)
  (setq eaf-pdf-dark-mode nil) ; see below
  (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  (eaf-bind-key nil "M-q" eaf-browser-keybinding)) ;; unbind, see more in the Wiki



(provide 'init-eaf)
;;; init-eaf.el ends here
