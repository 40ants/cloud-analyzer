(pushnew "~/projects/lisp/cffi/" asdf:*central-registry*
         :test #'equal)
(pushnew "~/projects/lisp/cl-plus-ssl/" asdf:*central-registry*
         :test #'equal)
(pushnew "~/projects/lisp/reblocks/" asdf:*central-registry*
         :test #'equal)


(defsystem "yandex-disk-cleaner"
  :class :package-inferred-system
  :pathname "src"
  :depends-on ("yandex-disk-cleaner/widgets/analyzer"))


(asdf:register-system-packages "colored" '(#:org.shirakumo.alloy.colored))
