;;; init-sql.el --- Summary
;;; Commentary:
;;; init sql mode config
;;; Code:

(use-package sql-indent
  :init
  (defun sanityinc/pop-to-sqli-buffer ()
    "Switch to the corresponding sqli buffer."
    (interactive)
    (if sql-buffer
        (progn
          (pop-to-buffer sql-buffer)
          (goto-char (point-max)))
      (sql-set-sqli-buffer)
      (when sql-buffer
        (sanityinc/pop-to-sqli-buffer))))
  :config
  (after-load 'sql
    ;; sql-mode pretty much requires your psql to be uncustomised from stock settings
    (push "--no-psqlrc" sql-postgres-options)

    (when (package-installed-p 'dash-at-point)
      (defun sanityinc/maybe-set-dash-db-docset ()
        (when (eq sql-product 'postgres)
          (set (make-local-variable 'dash-at-point-docset) "psql")))

      (add-hook 'sql-mode-hook 'sanityinc/maybe-set-dash-db-docset)
      (add-hook 'sql-interactive-mode-hook 'sanityinc/maybe-set-dash-db-docset)
      (defadvice sql-set-product (after set-dash-docset activate)
        (sanityinc/maybe-set-dash-db-docset))))
  (setq-default sql-input-ring-file-name
                (expand-file-name ".sqli_history" user-emacs-directory))
  :bind (:map sql-mode-map
              ("C-c C-z" . sanityinc/pop-to-sqli-buffer)))

;; See my answer to https://emacs.stackexchange.com/questions/657/why-do-sql-mode-and-sql-interactive-mode-not-highlight-strings-the-same-way/673
(defun sanityinc/font-lock-everything-in-sql-interactive-mode ()
  (unless (eq 'oracle sql-product)
    (sql-product-font-lock nil nil)))
(add-hook 'sql-interactive-mode-hook 'sanityinc/font-lock-everything-in-sql-interactive-mode)

(after-load 'page-break-lines
  (push 'sql-mode page-break-lines-modes))


(provide 'init-sql)
;;; init-sql.el ends here
