;;; init-git.el --- Summary
;;; Commentary:
;;; init git config
;;; Code:


;; init magit
(use-package git-magit
  :init
  (use-package git-blamed)
  (use-package gitignore-mode)
  (use-package gitconfig-mode)
  (use-package git-timemachine)
  (use-package fullframe)
  :config
  (setq-default magit-diff-refine-hunk t)
  (add-hook 'magit-popup-mode-hook 'sanityinc/no-trailing-whitespace)
  (fullframe magit-status magit-mode-quit-window)
  (when mac?
    (add-hook 'magit-mode-hook (lambda () (local-unset-key [(meta h)]))))
  (use-package git-commit
    :config (add-hook 'git-commit-mode-hook 'goto-address-mode))
  (use-package git-messenger
    :bind (("C-x v p" . git-messenger:popup-message)))
  :bind (([(meta f12)] . magit-status)
         ("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)
         :map magit-status-mode-map
         ("C-M-<up>" . magit-section-up)))


;; init git gutter
;; (use-package git-gutter
;;   :config
;;   (global-git-gutter-mode t)
;;   (git-gutter:linum-setup)
;;   (custom-set-variables
;;    '(git-gutter:window-width 2)
;;    '(git-gutter:modified-sign "☁")
;;    '(git-gutter:added-sign "☀")
;;    '(git-gutter:deleted-sign "☂")))


;; init github
(use-package yagist)
(use-package bug-reference-github
  :config (add-hook 'prog-mode-hook 'bug-reference-prog-mode))
(use-package github-clone)
(use-package github-issues)
;;(use-package magit-gh-pulls)

(provide 'init-git)
;;; init-git.el ends here
