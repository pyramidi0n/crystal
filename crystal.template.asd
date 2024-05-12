(defsystem "crystal.template"
  :description "A website template for use with the Crystal static site generator."
  :version "1.0.0"
  :author "Stephen Youts"
  :license "BSD-2"
  :depends-on ("alexandria"
               "cl-markdown"
               "copy-directory"
               "hunchentoot"
               "lass"
               "spinneret"
               "uiop")
  :components ((:module "template"
                :components
                ((:file "main" :pathname "src/main")
                 (:file "config" :pathname "site/config" :depends-on ("main"))
                 (:file "pages" :pathname "site/pages" :depends-on ("main"))))))
