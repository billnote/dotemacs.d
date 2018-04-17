;;; init-exec-path.el --- Summary
;;; Commentary:
;;; exec path from shell config
;;; Code:

(use-package exec-path-from-shell
  :config
  (after-load 'exec-path-from-shell
    (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "PATH" "GOPATH"))
      (add-to-list 'exec-path-from-shell-variables var)))
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(provide 'init-exec-path)
;;; init-exec-path.el ends here
