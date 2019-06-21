(defpackage :youtube
  (:use :cl)
  (:import-from :uiop :run-program)
  (:import-from :uiop :launch-program)
  (:import-from :alexandria :if-let)
  (:import-from :bordeaux-threads :make-thread)
  (:import-from :yason :parse)
  (:import-from :cl-ppcre :scan-to-strings)
  (:export play
           play/pause
           replay
           seek
           percent-pos
           time-pos
           duration
           switch-to-browser
           turn-video-on
           quit))
