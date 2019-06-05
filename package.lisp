(defpackage :youtube
  (:use :cl)
  (:import-from :uiop :run-program)
  (:import-from :alexandria :if-let)
  (:import-from :yason :parse)
  (:export play
           play/pause
           replay
           seek
           percent-pos
           switch-to-browser
           quit))
