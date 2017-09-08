;;; init-anzu.el --- Summary
;;; Commentary:
;;; init anzu config
;;; Code:

(use-package anzu
  :init
  ;; Search back/forth for the symbol at point
  ;; See http://www.emacswiki.org/emacs/SearchAtPoint
  (defun isearch-yank-symbol ()
    "*Put symbol at current point into search string."
    (interactive)
    (let ((sym (symbol-at-point)))
      (if sym
          (progn
            (setq isearch-regexp t
                  isearch-string (concat "\\_<" (regexp-quote (symbol-name sym)) "\\_>")
                  isearch-message (mapconcat 'isearch-text-char-description isearch-string "")
                  isearch-yank-flag t))
        (ding)))
    (isearch-search-and-update))
  
  ;; http://www.emacswiki.org/emacs/ZapToISearch
  (defun sanityinc/isearch-exit-other-end (rbeg rend)
    "Exit isearch, but at the other end of the search string.
This is useful when followed by an immediate kill."
    (interactive "r")
    (isearch-exit)
    (goto-char isearch-other-end))

  :config
  (global-anzu-mode)
  (setq anzu-mode-lighter "")
  :bind (([remap query-replace-regexp] . anzu-query-replace-regexp)
         ([remap query-replace] . anzu-query-replace)
         :map isearch-mode-map
         ([remap isearch-delete-char] . isearch-del-char)
         ("C-M-w" . isearch-yank-symbol)
         ([(control return)] . sanityinc/isearch-exit-other-end)))

(provide 'init-anzu)
;;; init-anzu.el ends here
