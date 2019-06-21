(asdf:defsystem :youtube
  :description "Play youtube urls with or without video using mpv"
  :author "Mihai Olteanu <mihai_olteanu@fastmail.fm>"
  :license  "GPLv3"
  :version "0.1"
  :depends-on (:alexandria
               :bordeaux-threads
               :yason
               :cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "youtube")))
