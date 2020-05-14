;; -*- lexical-binding: t; -*-
(require 'request)
(require 'subr-x)

(defconst toutiao-url "https://toutiao.io/contribute")

(defvar toutiao-user-id (getenv "TOUTIAO_USER_ID"))

(defvar toutiao-request-sync nil)

(defun toutiao--get-cookie ()
  (unless toutiao-user-id
    (error "请先设置toutiao-user-id"))
  (format "permanent_user_id=%s" toutiao-user-id))

(defun toutiao--get-catalogs (dom)
  (let* ((post-dom (dom-by-id dom "new_post"))
         (span-doms (dom-by-tag post-dom 'span)))
    (mapcar (lambda (span)
              (let ((text (dom-texts span))
                    (value (dom-attr (car (dom-by-tag span 'input)) 'value)))
                (cons (string-trim text) (string-trim value))))
            span-doms)))

(defun toutiao--get-post-url (dom)
  (let ((post-dom (dom-by-id dom "new_post")))
    (concat "https://toutiao.io" (dom-attr post-dom 'action))))

(defun toutiao--generate-data (dom)
  (let ((name (dom-attr dom 'name))
        (value (dom-attr dom 'value)))
    (cons (string-trim name) (string-trim value))))

(defun toutiao--get-post-data (dom post_url post_title &optional catalog)
  (let* ((post-dom (dom-by-id dom "new_post"))
         (catalog-alist (toutiao--get-catalogs dom))
         (catalogs (mapcar #'car catalog-alist))
         (catalog (or catalog (completing-read ":" catalogs)))
         (catalog-id (cdr (assoc-string catalog catalog-alist)))
         (hidden-doms (dom-elements post-dom 'type "hidden"))
         (submit-doms (dom-elements post-dom 'type "submit"))
         (hidden-data (mapcar #'toutiao--generate-data hidden-doms))
         (submit-data (mapcar #'toutiao--generate-data submit-doms))
         (article-data `(("post[url]" . ,post_url)
                      ("post[title]" . ,post_title)
                      ("post[subject_id]" . ,catalog-id))))
    (nconc hidden-data submit-data article-data)))

;;;###autoload
(defun toutiao-post (&optional article-url article-title article-catalog)
  (interactive)
  (let* ((article-url (or article-url (read-string "请输入分享文章的URL:")))
         (article-title (or article-title (read-string "请输入分享文章的标题:")))
         (request-headers `(("Accept" . "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8")
                            ("Cookie" . ,(toutiao--get-cookie))
                            ("Upgrade-Insecure-Requests" . "1")
                            ("Connection" . "keep-alive")))
         (request-parser (lambda ()
                           (libxml-parse-html-region (point) (point-max)))))
    (request toutiao-url
      :method "GET"
      :headers request-headers
      :parser request-parser
      :sync toutiao-request-sync
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (message "DEBUG:%s" data)
                  (let ((post-url (toutiao--get-post-url data))
                        (post-data (toutiao--get-post-data data article-url article-title article-catalog)))
                    (request post-url
                      :method "POST"
                      :data post-data
                      :headers request-headers
                      :parser request-parser
                      :sync toutiao-request-sync
                      :success (cl-function
                                (lambda (&key response &allow-other-keys)
                                  (let ((location (request-response-header response "Location")))
                                    (message "URL is %s" location)))))))))))

(provide 'toutiao)
