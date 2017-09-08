;;; init-wp-blog.el --- Summary
;;; Commentary:
;;; init wordpress blog config
;;; Code:

(use-package org2blog
  :init
  (use-package xml-rpc)
  (use-package metaweblog)
  :config
  (let (credentials)
    ;; only required if your auth file is not already in the list of auth-sources
    (add-to-list 'auth-sources "~/.authinfo.gpg")
    (setq credentials (auth-source-user-and-password "myblog"))
    (setq org2blog/wp-blog-alist
          `(("my-blog"
             :url "http://billhuang.me/xmlrpc.php"
             :username ,(car credentials)
             :password ,(cadr credentials)))))
  (setq org2blog/wp-use-sourcecode-shortcode 't)
  (setq org2blog/wp-sourcecode-default-params nil)
  (setq org2blog/wp-sourcecode-langs
        '("actionscript3" "bash" "coldfusion" "cpp" "csharp" "css" "delphi"
          "erlang" "fsharp" "diff" "groovy" "javascript" "java" "javafx" "matlab"
          "objc" "perl" "php" "text" "powershell" "python" "ruby" "scala" "sql"
          "vb" "xml"
          "sh" "emacs-lisp" "lisp" "lua"))
  (setq org-src-fontify-natively 't))

(provide 'init-wp-blog)
;;; init-wp-blog.el ends here
