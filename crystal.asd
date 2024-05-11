(defsystem "crystal"
  :description "A Lispy static site generator using Spinneret, Lass, and Markdown."
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
  :components ((:static-file "LICENSE")
               (:static-file "README.md")

               (:file "main" :pathname "src/main")

               (:file "config" :pathname "site/config" :depends-on ("main"))
               (:file "pages" :pathname "site/pages" :depends-on ("main"))))
