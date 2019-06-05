(in-package :youtube)

(defparameter *socket* "/tmp/mpv-cl-socket")
(defparameter *playing-url* nil)

(defun play (youtube-url &key (video nil) (pos "0"))
  "Play the youtube url through mpv with audio only or with video."
  (set-playing-url youtube-url)
  (run-program
   (concatenate 'string
                "mpv --ytdl-format=best --input-ipc-server="
                *socket* " "
                (unless video
                  "--vid=no ")
                youtube-url
                "\\&feature=youtu.be\\&t="
                pos))
  (clear-playing-url))

(defun turn-video-on ()
  (let ((url (playing-url))
        (pos (time-pos)))
    (quit)
    ;wait for mpv to close and url to be cleared, otherwise, the clearing of the
    ;url might happen after playing the video which would result in a state
    ;where there is no playing url but mpv is running.
    (sleep 1)                           
    (play url :video t :pos pos)))

(defun send-command (&rest args)
  (when (running-p)
    (parse
     (with-output-to-string (out)
       (run-program
        (format nil "echo '{\"command\": [~{\"~a\"~^, ~}]}' | socat - ~A"
                args *socket*)
        :output out)))))

(defun set-property (property value)
  (send-command "set_property" property value))

(defun get-property (property)
  (if-let ((response (send-command "get_property" property)))
    (gethash "data" response)))

(defun playing-url ()
  *playing-url*)

(defun set-playing-url (url)
  (setf *playing-url* url))

(defun clear-playing-url ()
  (set-playing-url nil))

(defun running-p ()
  "Check if mpv player has been started, regardles if it's paused or actually
playing."
  *playing-url*)

(defun paused-p ()
  "Check if mpv is paused."
  (get-property "pause"))

(defun pause ()
  "Pause the player"
  (unless (paused-p)
    (play/pause)))

(defun play/pause ()
  "Toggle playing status"
  (send-command "cycle" "pause"))

(defun replay ()
  "Rewind the song at the beginning, effectively replaying it."
  (set-property "percent-pos" 0))

(defun seek (seconds)
  "Forward or backward play by seconds, if the seconds is negative."
  (send-command "seek" seconds))

(defun percent-pos ()
  "Current playing song position, in percent."
  (get-property "percent-pos"))

(defun time-pos ()
  "Current playing song position, in seconds, as string."
  (write-to-string (round (get-property "time-pos"))))

(defun switch-to-browser (&key (from-beginning nil))
  (pause)              ;don't want mpv and youtube to both run at the same time
  (run-program (concatenate
                'string  "xdg-open \"" (playing-url)
                (unless from-beginning
                  (concatenate
                   'string "&feature=youtu.be&t="
                   (time-pos)
                   ;; url for xdg-open must be surrounded by quotes
                   "\"")))))

(defun quit ()
  (send-command "quit")
  (clear-playing-url))
