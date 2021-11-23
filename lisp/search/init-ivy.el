;;; init-ivy.el --- Use ivy for minibuffer completion and more -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package ivy
  :diminish ivy-mode
  :config
  (add-hook 'after-init-hook 'ivy-mode)
  (setq-default ivy-use-virtual-buffers t
                ivy-virtual-abbreviate 'fullpath
                ivy-count-format ""
                projectile-completion-system 'ivy
                ivy-magic-tilde nil
                ivy-dynamic-exhibit-delay-ms 150
                ivy-use-selectable-prompt t)
  :bind (
         :map ivy-minibuffer-map
         ("RET" . ivy-alt-done)
         ([("C-j" "C-RET")] . ivy-immediate-done)
         ("<up>" . ivy-previous-line-or-history)
         ("<down>" . ivy-next-line-or-history)
         :map ivy-occur-grep-mode-map
         ("C-c C-q" . ivy-wgrep-change-to-wgrep-mode))
  :config
  (use-package ivy-rich
    :init
    (setq ivy-virtual-abbreviate 'abbreviate
          ivy-rich-switch-buffer-align-virtual-buffer nil
          ivy-rich-path-style 'abbrev)
    (add-hook 'ivy-mode-hook (lambda () (ivy-rich-mode ivy-mode)))))

(use-package counsel
  :diminish counsel-mode
  :init
  (add-hook 'after-init-hook 'counsel-mode)
  (setq-default counsel-mode-override-describe-bindings t
                ivy-initial-inputs-alist
                '((Man-completion-table . "^")
                  (woman . "^")))
  :config
  (use-package projectile
    :init
    (let ((search-function
           (cond
            ((executable-find "rg") 'counsel-rg)
            ((executable-find "ag") 'counsel-ag)
            ((executable-find "pt") 'counsel-pt)
            ((executable-find "ack") 'counsel-ack))))
      (when search-function
        (defun sanityinc/counsel-search-project (initial-input &optional use-current-dir)
          "Search using `counsel-rg' or similar from the project root for INITIAL-INPUT.
If there is no project root, or if the prefix argument
USE-CURRENT-DIR is set, then search from the current directory
instead."
          (interactive (list (let ((sym (thing-at-point 'symbol)))
                               (when sym (regexp-quote sym)))
                             current-prefix-arg))
          (let ((current-prefix-arg)
                (dir (if use-current-dir
                         default-directory
                       (condition-case err
                           (projectile-project-root)
                         (error default-directory)))))
            (funcall search-function initial-input dir)))))
    :config
    (add-to-list 'ivy-height-alist (cons 'counsel-ag 20))
    :bind (("C-c f" . sanityinc/counsel-search-project))))

(use-package swiper
  :bind (
         :map ivy-mode-map
         ("M-s /" . swiper-thing-at-point)))

(use-package ivy-xref
  :init
  (setq xref-show-xrefs-function 'ivy-xref-show-xrefs))

(provide 'init-ivy)
;;; init-ivy.el ends here
