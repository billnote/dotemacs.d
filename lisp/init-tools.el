;;; init-tools.el --- Working tools -*- lexical-binding: t -*-
;;; Commentary:
;;; 工作常用工具集
;;; Code:

(defun t/ftime (timestamp &optional timezone)
  "Format TIMESTAMP to %Y-%m-%d %H:%M:%S with TIMEZONE."
  (let ((time (if timezone
                  (format-time-string "%Y-%m-%d %H:%M:%S" (seconds-to-time timestamp)(* timezone 3600))
                (format-time-string "%Y-%m-%d %H:%M:%S" (seconds-to-time timestamp)))))
    (with-temp-buffer
      (insert time)
      (clipboard-kill-ring-save (point-min) (point-max)))
    time))

(defun t/eurl (url)
  "Encode URL."
  (let ((encode-url (url-hexify-string url)))
    (with-temp-buffer
      (insert encode-url)
      (clipboard-kill-ring-save (point-min) (point-max)))
    encode-url))

(defun t/lmd5 (plaintext)
  "This function return an lowercase MD5 hash, source by PLAINTEXT."
  (let ((ciphertext (md5 plaintext)))
    (with-temp-buffer
      (insert ciphertext)
      (clipboard-kill-ring-save (point-min) (point-max)))
    ciphertext))

(defun t/umd5 (plaintext)
  "This function return an uppercase MD5 hash, source by PLAINTEXT."
  (let ((ciphertext (upcase (md5 plaintext))))
    (with-temp-buffer
      (insert ciphertext)
      (clipboard-kill-ring-save (point-min) (point-max)))
    ciphertext))


(provide 'init-tools)
;;; init-tools.el ends here
