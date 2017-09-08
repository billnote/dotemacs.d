;;; init-dash.el --- Summary
;;; Commentary:
;;; init dash for macOS config
;;; Code:

(defun sanityinc/dash-installed-p ()
  "Return t if Dash is installed on this machine, or nil otherwise."
  (let ((lsregister "/System/Library/Frameworks/CoreServices.framework/Versions/A/Frameworks/LaunchServices.framework/Versions/A/Support/lsregister"))
    (and (file-executable-p lsregister)
         (not (string-equal
               ""
               (shell-command-to-string
                (concat lsregister " -dump|grep com.kapeli.dash")))))))

(when (and mac? (sanityinc/dash-installed-p))
  (use-package dash-at-point
    :bind (("C-c C-f" . dash-at-point))))

(provide 'init-dash)
;;; init-dash.el ends here
