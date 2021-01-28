;;; init-lua.el --- lua -*- lexical-binding: t -*-
;;; Commentary:
;;; init lua edit mode
;;; Code:



(use-package lua-mode
  :ensure t
  :mode "\\.lua$"
  :interpreter "lua"
  :hook (lua-mode . set-company-backends-for-lua)
  :config
  (use-package company-lua)
  (defun set-company-backends-for-lua()
    "Set lua company backend."
    (setq-local company-backends '(
                                   (
                                    company-lsp
                                    company-lua
                                    company-keywords
                                    company-gtags
                                    company-yasnippet
                                    )
                                   company-capf
                                   company-dabbrev-code
                                   company-files
                                   )))
  (use-package flycheck-luacheck
    :hook (lua-mode . (lambda () (flycheck-luacheck-standards ' ("ngx_lua")))))
  (setq lua-indent-level 4)
  (setq lua-indent-string-contents t)
  (setq lua-prefix-key nil))

(provide 'init-lua)
;;; init-lua.el ends here
