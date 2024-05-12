(defsystem "crystal"
  :description "A Lispy static site generator using Spinneret, Lass, and Markdown."
  :version "1.0.0"
  :author "Stephen Youts"
  :license "BSD-2"
  :depends-on ("crystal.template"
               "alexandria"
               "copy-directory"
               "str")
  :components ((:static-file "LICENSE")
               (:static-file "README.md")

               (:module "src"
                :components
                ((:file "main" :pathname "main")))))
