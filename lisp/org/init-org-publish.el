;;; init-org-publish.el --- Summary
;;; Commentary:
;;; init org publish config
;;; Code:

(defgroup org-publish nil
  "Customization of the org publish")

(defcustom work-wiki-base-directory nil
  "Where is the work wiki org file base on directory."
  :type 'string
  :group 'org-publish)

(defcustom work-wiki-publish-directory nil
  "Where is the work wiki publish directory."
  :type 'string
  :group 'org-publish)

(setq org-publish-project-alist
      '(
        ;;; mplus wiki setting
        ("m-html"
         :base-directory "~/Documents/m/server/wiki/org"
         :publishing-directory "~/Documents/m/server/wiki/html"
         :base-extension "org"
         :recursive t
         :publishing-function org-html-publish-to-html
         :exclude "mplusadx-api-dsp-upload.org"
         :sitemap-sort-folders "last")
        ("m-static"
         :base-directory "~/Documents/m/server/wiki/org"
         :publishing-directory "~/Documents/m/server/wiki/html"
         :recursive t
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|mp3\\|swf\\|zip\\|gz\\|txt"
         :publishing-function org-publish-attachment)
        ("m-pdf"
         :base-directory "~/Documents/m/server/wiki/org"
         :publishing-directory "~/Documents/m/server/wiki/pdf"
         :base-extension "org"
         :recursive t
         :publishing-function org-latex-publish-to-pdf
         :exclude "mplusadx-api-dsp-upload.org"
         :sitemap-sort-folders "last"
         )
        ("g-static"
         :base-directory "~/Documents/m/server/wiki/org"
         :publishing-directory "/home/bill.huang/workspace/Documents/server/sources"
         :recursive t
         :base-extension "tex\\|org\\|css\\|js\\|png\\|jpg\\|gif\\|mp3\\|swf\\|zip\\|gz\\|txt\\|json\\|uml\\|xml\\|java"
         :publishing-function org-publish-attachment)
        ("g-pdf"
         :base-directory "~/Documents/m/server/wiki/pdf"
         :publishing-directory "/home/bill.huang/workspace/Documents/server"
         :recursive t
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|swf\\|zip\\|gz\\|txt"
         :publishing-function org-publish-attachment)
        ("mwiki"
         :components ("m-html" "m-static" "m-pdf" "g-static" "g-pdf")
         :author "bill.huang@mplusmedia.com")

        ;;; my blogs setting
        ("blog-html"
         :base-directory "~/Documents/mymind/org"
         :publishing-directory "~/Documents/mymind/blog"
         :base-extension "org"
         :recursive t
         :publishing-function org-html-publish-to-html
         :auto-sitemap t
         :sitemap-filename "sitemap.org"
         :sitemap-title "sitemap"
         :sitemap-sort-folders "last"
         :html-head-extra "<link rel=\"alternate\" type=\"application/rss+xml\"
                href=\"http://billhuang.me/sitemap.xml\"
                title=\"RSS feed for billhuang.me\">")
        ("blog-rss"
         :base-directory "~/Documents/mymind/org"
         :publishing-directory "~/Documents/mymind/blog"
         :base-extension "org"
         :publishing-function org-rss-publish-to-rss
         :html-link-home "http://billhuang.me/"
         :exclude ".*"
         :include ("sitemap.org"))
        ("blog-static"
         :base-directory "~/Documents/mymind/org"
         :publishing-directory "~/Documents/mymind/blog"
         :recursive t
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|svg\\|pdf\\|mp3\\|swf\\|zip\\|gz\\|txt\\|el"
         :publishing-function org-publish-attachment)
        ("blog-sources"
         :base-directory "~/Documents/mymind/org"
         :publishing-directory "~/Documents/mymind/blog/sources"
         :recursive t
         :base-extension "dot\\|org\\|java"
         :publishing-function org-publish-attachment)
        ("blog"
         :components ("blog-html" "blog-rss" "blog-static" "blog-sources")
         :author "billyhuang@sina.cn")))

(provide 'init-org-publish)
;;; init-org-publish.el ends here
