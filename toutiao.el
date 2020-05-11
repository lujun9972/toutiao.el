(require 'request)

(defconst toutiao-url "https://toutiao.io/contribute")

(defvar toutiao-user-id (getenv "TOUTIAO-USER-ID"))

(defun toutiao--get-catalogs (dom)
  (let* ((post-dom (dom-by-id dom "new_post"))
         (span-doms (dom-by-tag post-dom 'span)))
    (mapcar (lambda (span)
              (let ((text (dom-texts span))
                    (value (dom-attr (car (dom-by-tag span 'input)) 'value)))
                (cons text value)))
            span-doms)))

(defun toutiao--get-post-url (dom)
  (let ((post-dom (dom-by-id dom "new_post")))
    (concat "https://toutiao.io" (dom-attr post-dom 'action))))

(defun toutiao--generate-data (dom)
  (let ((name (dom-attr dom 'name))
        (value (dom-attr dom 'value)))
    (cons name value)))

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

(defun toutiao-post (article-url article-title &optional article-catalog)
  (interactive)
  (let* ((request-headers `(("Accept" . "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8")
                            ("Cookie" . ,(format "permanent_user_id=%s" toutiao-user-id))
                            ("Upgrade-Insecure-Requests" . "1")
                            ("Connection" . "keep-alive")))
         (request-parser (lambda ()
                           (libxml-parse-html-region (point) (point-max))))
         (response (request toutiao-url
                     :method "GET"
                     :headers request-headers
                     :parser request-parser
                     :sync t))
         (data-dom (request-response-data response))
         (post-url (toutiao--get-post-url data-dom))
         (post-data (toutiao--get-post-data data-dom article-url article-title article-catalog)))
    (request post-url
      :method "POST"
      :data post-data
      :headers request-headers
      :parser #'buffer-string
      :sync t)))

;; (toutiao-post "https://www.lujun9972.win/blog/2020/05/03/org-mode%E5%8D%8F%E5%8A%A9%E8%BF%9B%E8%A1%8C%E6%9C%88%E5%BA%A6%E5%9B%9E%E9%A1%BE/index.html" "org-mode协助进行月度回顾")
