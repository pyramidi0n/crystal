(in-package :cl-user)
(defpackage :crystal
  (:use
   :cl)
  (:export
   :init))
(in-package :crystal)

;; -----------------------------------------------------------------------------

(defparameter *system-path* (namestring (asdf:system-source-directory :crystal)))
(defparameter *template-system-path*
  (str:concat (namestring (asdf:system-source-directory :crystal.template))
              "template/"))

(defparameter *template-package-name* ":crystal.template")

(defparameter *rel.src.main* "src/main.lisp")
(defparameter *rel.site.config* "site/config.lisp")
(defparameter *rel.site.pages* "site/pages.lisp")
(defparameter *rel.asd* "template.asd")

;; -----------------------------------------------------------------------------

(defun template-code-str (relative-path)
  (alexandria:read-file-into-string
   (parse-namestring
    (str:concat *template-system-path* relative-path))))

(defun keyword-str (str)
  (str:concat ":" str))

;; -----------------------------------------------------------------------------

(defun src.main (site-package-name)
  (str:replace-all *template-package-name*
                   (keyword-str site-package-name)
                   (template-code-str *rel.src.main*)))

(defun site.config (site-package-name)
  (str:replace-all *template-package-name*
                   (keyword-str site-package-name)
                   (template-code-str *rel.site.config*)))

(defun site.pages (site-package-name)
  (str:replace-all *template-package-name*
                   (keyword-str site-package-name)
                   (template-code-str *rel.site.pages*)))

(defun site.system (site-system-name &key
                                       description
                                       version
                                       author
                                       license)
  (let ((code-str (template-code-str *rel.asd*)))
    (labels ((replace-keyval-str (code-str key val)
               (str:replace-all (str:concat key " \"\"")
                                (str:concat key " \"" val "\"")
                                code-str))
             (update-code-str (key val)
               (setf code-str (replace-keyval-str code-str key val))))
      (update-code-str "defsystem" site-system-name)
      (update-code-str "description" description)
      (update-code-str "version" version)
      (update-code-str "author" author)
      (update-code-str "license" license)
      code-str)))

(defun init (site-directory &key
                              (description "")
                              (version "1.0.0")
                              (author "")
                              (license ""))
  (let ((site-package-name (first
                            (last
                             (pathname-directory
                              (parse-namestring
                               site-directory))))))
    (labels ((write-site-code (code-func dest-relative-path &rest args)
               (alexandria:write-string-into-file
                (apply code-func (append (list site-package-name) args))
                (parse-namestring
                 (str:concat site-directory dest-relative-path))
                :if-exists :overwrite
                :if-does-not-exist :create)))
      (copy-directory:copy (parse-namestring *template-system-path*)
                           (parse-namestring site-directory))
      (delete-file (parse-namestring (str:concat site-directory *rel.asd*)))
      (write-site-code #'src.main *rel.src.main*)
      (write-site-code #'site.config *rel.site.config*)
      (write-site-code #'site.pages *rel.site.pages*)
      (write-site-code #'site.system
                       (str:concat site-package-name ".asd")
                       :description description
                       :version version
                       :author author
                       :license license)))
  t)
