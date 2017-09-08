;;; init-ivy.el --- Summary
;;; Commentary:
;;; init ivy config
;;; Code:

(use-package ivy
  :diminish ivy-mode
  :config
  (setq-default ivy-use-virtual-buffers t
                ivy-virtual-abbreviate 'fullpath
                ivy-count-format ""
                projectile-completion-system 'ivy
                ivy-initial-inputs-alist
                '((man . "^")
                  (woman . "^")))
  (defun sanityinc/enable-ivy-flx-matching ()
    "Make `ivy' matching work more like IDO."
    (interactive)
    (use-package flx
      :config
      (setq-default ivy-re-builders-alist
        	    '((t . ivy--regex-fuzzy)))))
  (add-hook 'after-init-hook
            (lambda ()
              (when (bound-and-true-p ido-ubiquitous-mode)
                (ido-ubiquitous-mode -1))
              (when (bound-and-true-p ido-mode)
                (ido-mode -1))
              (ivy-mode 1)))
  :bind (:map ivy-minibuffer-map
              ("RET" . ivy-alt-done)
              ("<up>" . ivy-previous-line-or-history)
              ([("C-j" "C-RET")] . ivy-immediate-done)
              ))

(use-package ivy-historian
  :config (add-hook 'after-init-hook (lambda () (ivy-historian-mode t))))

(use-package counsel
  :diminish counsel-mode
  :init (setq-default counsel-mode-override-describe-bindings t)
  :config
  (add-hook 'after-init-hook 'counsel-mode)
  (when (executable-find "ag")
    (use-package projectile
      :init
      (defun sanityinc/counsel-ag-project (initial-input)
        "Search using `counsel-ag' from the project root for INITIAL-INPUT."
        (interactive (list (thing-at-point 'symbol)))
        (counsel-ag initial-input (condition-case err
                                      (projectile-project-root)
                                    (error default-directory))))
      :bind (("M-?" . sanityinc/counsel-ag-project)))))

(use-package swiper
  :init
  (defun sanityinc/swiper-at-point (sym)
    "Use `swiper' to search for the symbol at point."
    (interactive (list (thing-at-point 'symbol)))
    (swiper sym))
  :bind (:map ivy-mode-map
         ("M-s /" . sanityinc/swiper-at-point)))

(provide 'init-ivy)
;;; init-ivy.el ends here
