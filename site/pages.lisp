(in-package :crystal)

;; ------------------------------------------------------------------------------

(defmacro page ((&key title) &body body)
  `(with-html-string
     (:doctype)
     (:html
      (:head
       (:title ,title)
       (:link :rel "stylesheet" :href (style "page.css")))
      (:body ,@body))))

(defun index ()
  (page (:title "Crystal")
    (:h1 "Index")
    (:p (:a :href "/about/" "About"))
    (:p (:a :href "/posts/2024-05-10-post/" "(2024-05-10) Post"))))

(defun about ()
  (page (:title "Crystal | About")
    (:h1 "About")
    (:p (:a :href "/" "Index"))
    (:p (:a :href "/posts/2024-05-10-post/" "(2024-05-10) Post"))))

(defun post (markdown-html)
  (page (:title "Crystal | Post")
    (:raw markdown-html)))
