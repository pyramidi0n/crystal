(in-package :cl-user)
(defpackage :crystal.template
  (:use
   :cl
   :spinneret)
  (:export
   :generate

   :start-preview
   :stop-preview))
(in-package :crystal.template)

;; ------------------------------------------------------------------------------

(defparameter *system-path* (namestring (asdf:system-source-directory :crystal.template)))
(defparameter *output-path* (concatenate 'string *system-path* "www"))
(defparameter *site-path* (concatenate 'string *system-path* "site"))
(defparameter *static-path* (concatenate 'string *site-path* "/static/"))
(defparameter *styles-path* (concatenate 'string *site-path* "/styles/"))
(defparameter *posts-path* (concatenate 'string *site-path* "/posts/"))

(defparameter *page-routes* '())
(defparameter *static-routes-prefix* nil)
(defparameter *styles-routes-prefix* nil)
(defparameter *post-routes-prefix* nil)
(defparameter *post-template* nil)

;; ------------------------------------------------------------------------------

(defun generate ()
  (labels ((path-collision-p ()
             (let* ((page-paths
                      (mapcar (lambda (route)
                                (car route))
                              *page-routes*))
                    (paths
                      (append page-paths
                              (list *static-routes-prefix*
                                    *styles-routes-prefix*
                                    *post-routes-prefix*))))
               (not (= (length paths)
                       (length (remove-duplicates paths :test #'string=))))))
           (verify-paths ()
             (when (path-collision-p)
               (error "Error: path collision. Verify your route definitions are unique.")))
           (clean ()
             (uiop:delete-directory-tree
              (parse-namestring (concatenate 'string *output-path* "/"))
              :validate t
              :if-does-not-exist :ignore))
           (static ()
             (let* ((static-source-pathname (parse-namestring *static-path*))
                    (static-output-pathname (parse-namestring
                                             (concatenate 'string
                                                          *output-path*
                                                          *static-routes-prefix*))))
               (when (probe-file static-source-pathname)
                 (copy-directory:copy static-source-pathname
                                      static-output-pathname))))
           (styles ()
             (mapcar (lambda (path)
                       (let* ((styles-output-path (concatenate 'string
                                                               *output-path*
                                                               *styles-routes-prefix*))
                              (style-output-path (concatenate 'string
                                                              styles-output-path
                                                              (pathname-name path)
                                                              ".css")))
                         (ensure-directories-exist (parse-namestring styles-output-path))
                         (alexandria:write-string-into-file
                          (apply #'concatenate
                                 'string
                                 (mapcar #'lass:compile-and-write
                                         (with-input-from-string
                                             (s (uiop:read-file-string path))
                                           (let ((read nil)
                                                 (forms '()))
                                             (loop while (setf read (read s nil))
                                                   do (push read forms))
                                             (nreverse forms)))))
                          (parse-namestring style-output-path))))
                     (uiop:directory-files (parse-namestring *styles-path*))))
           (pages ()
             (mapcar (lambda (route)
                       (let* ((url (car route))
                              (template (cadr route))
                              (url-path (concatenate 'string *output-path* url))
                              (index-path (concatenate 'string url-path "index.html")))
                         (ensure-directories-exist (parse-namestring url-path))
                         (alexandria:write-string-into-file
                          (funcall (symbol-function template))
                          (parse-namestring index-path))))
                     *page-routes*))
           (posts ()
             (when (and *post-routes-prefix* *post-template*)
               (mapcar (lambda (post-pathname)
                         (let* ((url-path (concatenate 'string
                                                       *output-path*
                                                       *post-routes-prefix*
                                                       (pathname-name post-pathname)
                                                       "/"))
                                (index-path (concatenate 'string url-path "index.html"))
                                (markdown-html (with-output-to-string (s)
                                                 (cl-markdown:markdown
                                                  (alexandria:read-file-into-string post-pathname)
                                                  :format :html
                                                  :stream s))))
                           (ensure-directories-exist (parse-namestring url-path))
                           (alexandria:write-string-into-file
                            (funcall (symbol-function *post-template*) markdown-html)
                            (parse-namestring index-path))))
                       (uiop:directory-files (parse-namestring *posts-path*))))))
    (handler-case
        (progn
          (verify-paths)
          (clean)
          (static)
          (styles)
          (pages)
          (posts)
          t)
      (error (e)
        (format *error-output* "~a~%" e) nil))))

;; ------------------------------------------------------------------------------

(defmacro page-routes (&body body)
  `(setf *page-routes* (quote ,body)))

(defmacro static-routes-prefix (&body body)
  `(setf *static-routes-prefix* ,@body))

(defmacro styles-routes-prefix (&body body)
  `(setf *styles-routes-prefix* ,@body))

(defmacro post-routes-prefix (&body body)
  `(setf *post-routes-prefix* ,@body))

(defmacro post-template (&body body)
  `(setf *post-template* (quote ,@body)))

;; ------------------------------------------------------------------------------

(defun static (filepath)
  (concatenate 'string *static-routes-prefix* filepath))

(defun style (filename)
  (concatenate 'string *styles-routes-prefix* filename))

;; ------------------------------------------------------------------------------

(let ((server-acceptor nil))
  (defun start-preview ()
    (setf server-acceptor
          (make-instance 'hunchentoot:easy-acceptor :port 5000))
    (hunchentoot:start server-acceptor)
    (push
     (hunchentoot:create-prefix-dispatcher
      "/"
      (lambda ()
        (let ((request-path (hunchentoot:request-pathname hunchentoot:*request* "/")))
          (when (null request-path)
            (setf (hunchentoot:return-code*) hunchentoot:+http-forbidden+)
            (hunchentoot:abort-request-handler))
          (when (or (= 0 (length (namestring request-path)))
                    (char= #\/
                           (char (namestring request-path)
                                 (1- (length (namestring request-path))))))
            (setf request-path
                  (parse-namestring (concatenate 'string
                                                 (namestring request-path)
                                                 "index.html"))))
          (hunchentoot:handle-static-file
           (merge-pathnames request-path
                            (parse-namestring
                             (concatenate 'string *output-path* "/")))))))
     hunchentoot:*dispatch-table*))

  (defun stop-preview ()
    (hunchentoot:stop server-acceptor)
    (setf server-acceptor nil)))
