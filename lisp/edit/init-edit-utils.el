;;; init-edit-utils.el --- Summary
;;; Commentary:
;;; init edit utils from purcell config.  thanks.
;;; Code:

(use-package unfill)

(when (fboundp 'electric-pair-mode)
  (electric-pair-mode))
(when (eval-when-compile (version< "24.4" emacs-version))
  (electric-indent-mode 1))

;;----------------------------------------------------------------------------
;; Some basic preferences
;;----------------------------------------------------------------------------
(setq-default
 blink-cursor-interval 0.4
 bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
 buffers-menu-max-size 30
 case-fold-search t
 column-number-mode t
 delete-selection-mode t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 indent-tabs-mode nil
 make-backup-files nil
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
 tooltip-delay 1.5
 truncate-lines nil
 truncate-partial-width-windows nil)

(global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(transient-mark-mode t)


 ;;; A simple visible bell which works in all terminal types

(defun sanityinc/flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.05 nil 'invert-face 'mode-line))

(setq-default
 ring-bell-function 'sanityinc/flash-mode-line)



;;; Newline behaviour

(global-set-key (kbd "RET") 'newline-and-indent)
(defun sanityinc/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(global-set-key (kbd "S-<return>") 'sanityinc/newline-at-end-of-line)



(when (eval-when-compile (string< "24.3.1" emacs-version))
  ;; https://github.com/purcell/emacs.d/issues/138
  (after-load 'subword
    (diminish 'subword-mode)))


(use-package indent-guide
  :diminish indent-guide-mode
  :config (add-hook 'prog-mode-hook 'indent-guide-mode))



;; (use-package nlinum-relative
;;   :config
;;   (setq nlinum-relative-current-symbol "->")    ;; or "" for display current line number
;;   (setq nlinum-relative-offset 1))              ;; 1 if you want 0, 2, 3...

;; ;;;###autoload
;; (define-globalized-minor-mode global-nlinum-relative-mode nlinum-relative-mode
;;   (lambda () (unless (minibufferp) (nlinum-relative-mode))))
;; (global-nlinum-relative-mode)
;; (nlinum-relative-off)                         ;; default linum

;; (use-package nlinum
;;   :config
;;   (global-nlinum-mode)
;;   (setq nlinum-highlight-current-line t))

(require 'linum)

(global-linum-mode)
(use-package hlinum
  :config
  (hlinum-activate)
  (setq linum-highlight-in-all-buffersp t))
(defvar linum-current-line 1 "Current line number.")
(defvar linum-border-width 1 "Border width for linum.")

;; (defface linum-current-line
;;   `((t :inherit linum
;;        :foreground "goldenrod"
;;        :weight bold
;;        ))
;;   "Face for displaying the current line number."
;;   :group 'linum)

;; (defadvice linum-update (before advice-linum-update activate)
;;   "Set the current line."
;;   (setq linum-current-line (line-number-at-pos)
;;         ;; It's the same algorithm that linum dynamic. I only had added one
;;         ;; space in front of the first digit.
;;         linum-border-width (number-to-string
;;                             (+ 1 (length
;;                                   (number-to-string
;;                                    (count-lines (point-min) (point-max))))))))

;; (defun linum-highlight-current-line (line-number)
;;   "Highlight the current line number using `linum-current-line' face."
;;   (let ((face (if (= line-number linum-current-line)
;;                   'linum-current-line
;;                 'linum)))
;;     (propertize (format (concat "%" linum-border-width "d") line-number)
;;                 'face face)))

;; (setq linum-format 'linum-highlight-current-line)


(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))



(when (fboundp 'global-prettify-symbols-mode)
  (global-prettify-symbols-mode))


(use-package undo-tree
  :diminish undo-tree-mode
  :config (global-undo-tree-mode))


(use-package symbol-overlay
  :diminish symbol-overlay-mode
  :config
  (dolist (hook '(prog-mode-hook html-mode-hook css-mode-hook))
    (add-hook hook 'symbol-overlay-mode))
  :bind (:map symbol-overlay-mode-map
	      ("M-n" . symbol-overlay-jump-next)
	      ("M-p" . symbol-overlay-jump-prev)))

;;----------------------------------------------------------------------------
;; Zap *up* to char is a handy pair for zap-to-char
;;----------------------------------------------------------------------------
(autoload 'zap-up-to-char "misc" "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-Z") 'zap-up-to-char)



(use-package browse-kill-ring
  :init
  (setq browse-kill-ring-separator "\f")
  :config
  (after-load 'page-break-lines
    (push 'browse-kill-ring-mode page-break-lines-modes))
  :bind (("M-Y" . browse-kill-ring)
	 :map browse-kill-ring-mode-map
	 ("C-g" . browse-kill-ring-quit)
	 ("M-n" . browse-kill-ring-forward)
	 ("M-p" . browse-kill-ring-previous)))

;;----------------------------------------------------------------------------
;; Don't disable narrowing commands
;;----------------------------------------------------------------------------
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;;----------------------------------------------------------------------------
;; Show matching parens
;;----------------------------------------------------------------------------
(show-paren-mode 1)

;;----------------------------------------------------------------------------
;; Expand region
;;----------------------------------------------------------------------------
(use-package expand-region
  :bind (("C-=" . er/expand-region)))

;;----------------------------------------------------------------------------
;; Don't disable case-change functions
;;----------------------------------------------------------------------------
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;;----------------------------------------------------------------------------
;; Rectangle selections, and overwrite text when the selection is active
;;----------------------------------------------------------------------------
(cua-selection-mode t)                  ; for rectangles, CUA is nice


;;----------------------------------------------------------------------------
;; Handy key bindings
;;----------------------------------------------------------------------------
(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)

(use-package avy
  :bind (("C-;" . avy-goto-word-or-subword-1)))

(use-package multiple-cursors
  :bind (
	 ;; multiple-cursors
	 ("C-<" . mc/mark-previous-like-this)
	 ("C->" . mc/mark-next-like-this)
	 ("C-c C-<" . mc/mark-all-like-this)
	 ;; From active region to multiple cursors:
	 ("C-c m r" . set-rectangular-region-anchor)
	 ("C-c m c" . mc/edit-lines)
	 ("C-c m e" . mc/edit-ends-of-lines)
	 ("C-c m a" . mc/edit-beginnings-of-lines)))

;; Train myself to use M-f and M-b instead
(global-unset-key [M-left])
(global-unset-key [M-right])

(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(global-set-key (kbd "C-M-<backspace>") 'kill-back-to-indentation)


;;----------------------------------------------------------------------------
;; Page break lines
;;----------------------------------------------------------------------------
(use-package page-break-lines
  :diminish page-break-lines-mode
  :config (global-page-break-lines-mode))

;;----------------------------------------------------------------------------
;; Shift lines up and down with M-up and M-down. When paredit is enabled,
;; it will use those keybindings. For this reason, you might prefer to
;; use M-S-up and M-S-down, which will work even in lisp modes.
;;----------------------------------------------------------------------------
(use-package move-dup
  :bind (([M-up] . md/move-lines-up)
	 ([M-down] . md/move-lines-down)
	 ("C-c d" . md/duplicate-down)
	 ("C-c D" . md/duplicate-up)))

;;----------------------------------------------------------------------------
;; Fix backward-up-list to understand quotes, see http://bit.ly/h7mdIL
;;----------------------------------------------------------------------------
(defun backward-up-sexp (arg)
  "Jump up to the start of the ARG'th enclosing sexp."
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

(global-set-key [remap backward-up-list] 'backward-up-sexp) ; C-M-u, C-M-up


;;----------------------------------------------------------------------------
;; Cut/copy the current line if no region is active
;;----------------------------------------------------------------------------
(use-package whole-line-or-region
  :diminish whole-line-or-region-mode
  :config
  (whole-line-or-region-mode t)
  (make-variable-buffer-local 'whole-line-or-region-mode))

(defun suspend-mode-during-cua-rect-selection (mode-name)
  "Add an advice to suspend `MODE-NAME' while selecting a CUA rectangle."
  (let ((flagvar (intern (format "%s-was-active-before-cua-rectangle" mode-name)))
        (advice-name (intern (format "suspend-%s" mode-name))))
    (eval-after-load 'cua-rect
      `(progn
         (defvar ,flagvar nil)
         (make-variable-buffer-local ',flagvar)
         (defadvice cua--activate-rectangle (after ,advice-name activate)
           (setq ,flagvar (and (boundp ',mode-name) ,mode-name))
           (when ,flagvar
             (,mode-name 0)))
         (defadvice cua--deactivate-rectangle (after ,advice-name activate)
           (when ,flagvar
             (,mode-name 1)))))))

(suspend-mode-during-cua-rect-selection 'whole-line-or-region-mode)




(defun sanityinc/open-line-with-reindent (n)
  "A version of `open-line' which reindents the start and end positions.
If there is a fill prefix and/or a `left-margin', insert them
on the new line if the line would have been blank.
With arg N, insert N newlines."
  (interactive "*p")
  (let* ((do-fill-prefix (and fill-prefix (bolp)))
	 (do-left-margin (and (bolp) (> (current-left-margin) 0)))
	 (loc (point-marker))
	 ;; Don't expand an abbrev before point.
	 (abbrev-mode nil))
    (delete-horizontal-space t)
    (newline n)
    (indent-according-to-mode)
    (when (eolp)
      (delete-horizontal-space t))
    (goto-char loc)
    (while (> n 0)
      (cond ((bolp)
	     (if do-left-margin (indent-to (current-left-margin)))
	     (if do-fill-prefix (insert-and-inherit fill-prefix))))
      (forward-line 1)
      (setq n (1- n)))
    (goto-char loc)
    (end-of-line)
    (indent-according-to-mode)))

(global-set-key (kbd "C-o") 'sanityinc/open-line-with-reindent)


;;----------------------------------------------------------------------------
;; Random line sorting
;;----------------------------------------------------------------------------
(defun sort-lines-random (beg end)
  "Sort lines in region randomly."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (s1 s2) (eq (random 2) 0)))))))




(use-package highlight-escape-sequences
  :config (hes-mode))

(use-package drag-stuff
  :config
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

(use-package idle-highlight-mode
  :init
  (defun coding-hook ()
    (make-local-variable 'column-number-mode)
    (column-number-mode t)
    (idle-highlight-mode t))
  :config
  (add-hook 'prog-mode-hook 'coding-hook))


;;; Whitespace
(setq-default show-trailing-whitespace t)

(defun sanityinc/no-trailing-whitespace ()
  "Turn off display of trailing whitespace in this buffer."
  (setq show-trailing-whitespace nil))

;; But don't show trailing whitespace in SQLi, inf-ruby etc.
(dolist (hook '(special-mode-hook
                Info-mode-hook
                eww-mode-hook
                term-mode-hook
                comint-mode-hook
                compilation-mode-hook
                twittering-mode-hook
                minibuffer-setup-hook))
  (add-hook hook #'sanityinc/no-trailing-whitespace))

(use-package whitespace-cleanup-mode
  :config (global-whitespace-cleanup-mode t)
  :bind (([remap just-one-space] . cycle-spacing)))

(provide 'init-edit-utils)
;;; init-edit-utils.el ends here
