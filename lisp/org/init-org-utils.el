;;; init-org-utils.el --- Summary
;;; Commentary:
;;; init org config
;;; Code:

(use-package org-plus-contrib
  :init
  (use-package gnuplot-mode)
  :config
  (use-package org-fstree)
  (when mac?
    (use-package grab-mac-link)
    (use-package org-mac-iCal))
  (use-package org-cliplink)

  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         :map org-mode-map
         ("C-M-<up>" . org-up-element)
         ("C-c g" . org-mac-grab-link))
  )


;;; capture
(setq org-capture-templates
      '(("u" "UnionPay AdServer" entry (file+headline "~/.todo/projects.org" "Yellowstone AdServer") "* TODO %? %^g\n %i" :prepend t)
        ("p" "PMP AdServer" entry (file+headline "~/.todo/projects.org" "PMP AdServer") "* TODO %? %^g\n %i")
        ("w" "PMP Web" entry (file+headline "~/.todo/projects.org" "PMP Web") "* TODO %? %^g\n %i")
        ("t" "Tracking Server" entry (file+headline "~/.todo/projects.org" "Tracking Server") "* TODO %? %^g\n %i")
        ("c" "Cookie Mapping" entry (file+headline "~/.todo/projects.org" "Cookie Mapping Server") "* TODO %? %^g\n %i")
        ("r" "Report" entry (file+headline "~/.todo/projects.org" "Report") "* TODO %? %^g\n %i")
        ("m" "UPM Access" entry (file+headline "~/.todo/projects.org" "UPM Access") "* TODO %? %^g\n %i")
        ("o" "Other Task" entry (file+headline "~/.todo/projects.org" "Other Task") "* TODO %? %^g\n %i")
        ("i" "Idea" entry (file+headline "~/.todo/task.org" "Ideas") "* TODO %? %^g\n %i" :prepend t)
        ("j" "Task" entry (file+headline "~/.todo/task.org" "Tasks") "* TODO %? %^g\n %i" :prepend t)
        ("n" "Note" entry (file+headline "~/.todo/note.org" "Notes") "* %U %?\n\n %i" :prepend t :empty-lines 1)
        ("a" "Account" table-line (file+headline "~/.account/.account.org.gpg" "bill accounts") "| %? | %? | %? | %U |")))


  ;;; agenda
(setq org-agenda-files (list "~/.todo/" "~/.todo/.bill/"))
(setq org-agenda-include-diary t)
(setq org-agenda-custom-commands
      '(("A" "PRIORITY A"
         ((tags-todo "+PRIORITY=\"A\""))
         ((org-agenda-compact-blocks t))) ;; options set here apply to the entire block
        ("B" "PRIORITY B"
         ((tags-todo "+PRIORITY=\"B\""))
         ((org-agenda-compact-blocks t)))
        ("C" "PRIORITY C"
         ((tags-todo "+PRIORITY=\"C\""))
         ((org-agenda-compact-blocks t)))
        ))
(setq org-plantuml-jar-path "~/.emacs.d/tools/plantuml/plantuml.jar")
(setq org-ditaa-jar-path "~/.emacs.d/tools/ditaa/ditaa.jar")


;; 各种Babel语言支持
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (emacs-lisp . t)
   (matlab . t)
   (C . t)
   (perl . t)
   (sh . t)
   (ditaa . t)
   (python . t)
   (haskell . t)
   (dot . t)
   (latex . t)
   (js . t)
   (java . t)
   (plantuml . t)
   (gnuplot . t )
   (sql . t)
   ))
;;生成图像不提示
(setq org-confirm-babel-evaluate nil)
(setq org-src-fontify-natively t)
;; add color font setting
(org-add-link-type
 "color" nil
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "<span style=\"color:%s;\">%s</span>" path desc))
    ((eq format 'latex)
     (format "{\\color{%s}%s}" path desc)))))
;; org-mode highlight
(org-add-link-type
 "hl" nil
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "<font style=\"background-color:%s;\">%s</font>" path desc))
    ((eq format 'latex)
     (format "\\colorbox{%s}{%s}" path desc))))) ;; require \usepackage{color}


;; Various preferences
(setq org-log-done t
      org-edit-timestamp-down-means-later t
      org-archive-mark-done nil
      org-hide-emphasis-markers t
      org-catch-invisible-edits 'show
      org-export-coding-system 'utf-8
      org-fast-tag-selection-single-key 'expert
      org-html-validation-link nil
      org-export-kill-product-buffer-when-displayed t
      org-tags-column 80)


;; Refiling
(setq org-refile-use-cache nil)
;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))

(after-load 'org-agenda
  (add-to-list 'org-agenda-after-show-hook 'org-show-entry))

(defadvice org-refile (after sanityinc/save-all-after-refile activate)
  "Save all org buffers after each refile operation."
  (org-save-all-org-buffers))

;; Exclude DONE state tasks from refile targets
(defun sanityinc/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-refile-target-verify-function 'sanityinc/verify-refile-target)

(defun sanityinc/org-refile-anywhere (&optional goto default-buffer rfloc msg)
  "A version of `org-refile' which allows refiling to any subtree."
  (interactive "P")
  (let ((org-refile-target-verify-function))
    (org-refile goto default-buffer rfloc msg)))

(defun sanityinc/org-agenda-refile-anywhere (&optional goto rfloc no-update)
  "A version of `org-agenda-refile' which allows refiling to any subtree."
  (interactive "P")
  (let ((org-refile-target-verify-function))
    (org-agenda-refile goto rfloc no-update)))

;; Targets start with the file name - allows creating level 1 tasks
;;(setq org-refile-use-outline-path (quote file))
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes 'confirm)


;;; Show the clocked-in task - if any - in the header line
(defun sanityinc/show-org-clock-in-header-line ()
  (setq-default header-line-format '((" " org-mode-line-string " "))))

(defun sanityinc/hide-org-clock-from-header-line ()
  (setq-default header-line-format nil))

(add-hook 'org-clock-in-hook 'sanityinc/show-org-clock-in-header-line)
(add-hook 'org-clock-out-hook 'sanityinc/hide-org-clock-from-header-line)
(add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line)

(after-load 'org-clock
  (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
  (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu))



(when (and mac? (file-directory-p "/Applications/org-clock-statusbar.app"))
  (add-hook 'org-clock-in-hook
            (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e"
                                 (concat "tell application \"org-clock-statusbar\" to clock in \"" org-clock-current-task "\""))))
  (add-hook 'org-clock-out-hook
            (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e"
                                 "tell application \"org-clock-statusbar\" to clock out"))))



;; Remove empty LOGBOOK drawers on clock out
(defun sanityinc/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "LOGBOOK" (point))))

(after-load 'org-clock
  (add-hook 'org-clock-out-hook 'sanityinc/remove-empty-drawer-on-clock-out 'append))



;; TODO: warn about inconsistent items, e.g. TODO inside non-PROJECT
;; TODO: nested projects!



  ;;; Archiving
(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archive")

  ;;; Org clock

;; Save the running clock and all clock history when exiting Emacs, load it on startup
(after-load 'org
  (org-clock-persistence-insinuate))
(setq org-clock-persist t)
(setq org-clock-in-resume t)

;; Save clock data and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Save state changes in the LOGBOOK drawer
(setq org-log-into-drawer t)
;; Removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Show clock sums as hours and minutes, not "n days" etc.
(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

(provide 'init-org-utils)
;;; init-org-utils.el ends here
