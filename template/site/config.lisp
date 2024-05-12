(in-package :crystal.template)

;; ------------------------------------------------------------------------------

(page-routes
  ("/" index)
  ("/about/" about))

(static-routes-prefix "/static/")
(styles-routes-prefix "/styles/")

(post-routes-prefix "/posts/")
(post-template post)
