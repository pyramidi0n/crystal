(defsystem ""
  :description ""
  :version ""
  :author ""
  :license ""
  :depends-on ("alexandria"
               "cl-markdown"
               "copy-directory"
               "hunchentoot"
               "lass"
               "spinneret"
               "uiop")
  :components ((:file "main" :pathname "src/main")
               (:file "config" :pathname "site/config" :depends-on ("main"))
               (:file "pages" :pathname "site/pages" :depends-on ("main"))))
