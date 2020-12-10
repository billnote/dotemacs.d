;;; init.el --- Summary
;;; Commentary:
;;; init config
;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(package-initialize)

(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)

(use-package f)

(defconst mac? (eq system-type 'darwin))

(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

;;----------------------------------------------------------------------------
;;Handier way to add modes to auto-mode-alist
;;----------------------------------------------------------------------------
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

(defun file-contents (filename)
  "Return the contents of FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))


;;load path
(let ((default-directory  "~/.emacs.d/lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))


;;custom config
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq network-security-level 'low)

;;shell path
(require 'init-exec-path)

;;session
(require 'init-sessions)

;;search
(require 'init-ivy)
(require 'init-anzu)
(require 'init-dash)

;;frame
(require 'init-frame)
(require 'init-windows)
(require 'init-ibuffer)

;; edit tools
(require 'init-edit-utils)
(require 'init-edit-mode)
(require 'init-tools)

;; command
(require 'init-command)

;; keys
(require 'init-keys)

;; dired
(require 'init-dired)

;; chinese fonts
(require 'init-cnfonts)

;; git
(require 'init-git)

;; develop
(require 'init-develop)

;; org
(require 'init-org-utils)
(require 'init-org-publish)
(require 'init-org-to-pdf)

;; shell
(require 'init-multi-term)

;; blog
(require 'init-wp-blog)

;; themes
(require 'init-themes)
(require 'init-mode-line-themes)

;; translate
(require 'init-translate)

;; scrum
(require 'scrum)
(require 'org-gantt)



;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(require 'server)
(unless (server-running-p)
  (server-start))

(when mac?
  (use-package osx-location))
(use-package regex-tool)

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;; init.el ends here
