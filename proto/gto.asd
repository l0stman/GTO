(in-package :asdf-user)

(defsystem "gto"
  :description "Tools to compute GTO solutions to HUNL."
  :author "rrl <endian.sign@gmail.com"
  :components ((:file "packages")
               (:file "utils" :depends-on ("packages"))
               (:file "range" :depends-on ("packages" "utils"))
               (:file "equity" :depends-on ("packages" "utils"))
               (:file "cfr" :depends-on ("packages"))
               (:file "five-bet"
                :depends-on ("packages" "range" "equity" "cfr"))))
