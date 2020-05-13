(require 'request)
(require 'subr-x)
;; (require 'url)
;; (require 'url-http)

(defconst toutiao-url "https://toutiao.io/contribute")

(defvar toutiao-user-id (getenv "TOUTIAO_USER_ID"))

(defvar toutiao-request-sync nil)

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
    (message "%s" catalog-alist)
    (nconc hidden-data submit-data article-data)))

;; (defun toutiao-request (url)
;;   (let* ((url-request-extra-headers `(("Accept" . "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8")
;;                                       ("Cookie" . ,(format "permanent_user_id=%s" toutiao-user-id))
;;                                       ("Upgrade-Insecure-Requests" . "1")
;;                                       ("Connection" . "keep-alive")))
;;          (response-buffer (url-retrieve-synchronously url))
;;          (response (with-current-buffer response-buffer
;;                      (goto-char (+ 1 url-http-end-of-headers))
;;                      (libxml-parse-html-region (point) (point-max)))))
;;     (kill-buffer response-buffer)
;;     response))

;; (defun toutiao-post (article-url article-title &optional article-catalog)
;;   (interactive)
;;   (let* ((contribute-data (toutiao-request toutiao-url))
;;          (post-url (toutiao--get-post-url contribute-data))
;;          (post-data (toutiao--get-post-data contribute-data article-url article-title article-catalog))
;;          (url-request-data (mapconcat (lambda (pair)
;;                                         (let ((k (car pair))
;;                                               (v (cdr pair)))
;;                                           (concat (url-hexify-string (format "%s" k))
;;                                                   "="
;;                                                   (url-hexify-string (format "%s" v)))))
;;                                       post-data
;;                                       "&"))
;;          (url-request-method "POST"))
;;     (message "toutiao DEBUG:%s,%s" post-url url-request-data)
;;     (toutiao-request post-url)))

;;;###autoload
(defun toutiao-post (article-url article-title &optional article-catalog)
  (interactive)
  (let* ((request-headers `(("Accept" . "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8")
                            ("Cookie" . ,(format "permanent_user_id=%s" toutiao-user-id))
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
                  (message "toutio DEBUG:data = [%s]" data)
                  (let ((post-url (toutiao--get-post-url data))
                        (post-data (toutiao--get-post-data data article-url article-title article-catalog)))
                    (message "toutiao DEBUG:[%s][%s]" post-url post-data)
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

;; (toutiao-post "https://www.lujun9972.win/blog/2020/02/08/%E7%94%A8%E7%9B%B4%E6%96%B9%E5%9B%BE%E5%B1%95%E7%A4%BA%E9%98%85%E8%AF%BB%E6%97%B6%E9%97%B4%E5%88%86%E5%B8%83%E6%83%85%E5%86%B5/index.html" "用直方图展示阅读时间分布情况" "Emacs之怒")
;; (setq toutiao-request-sync t)

(provide 'toutiao)
