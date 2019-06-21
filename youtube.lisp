(in-package :youtube)

(defparameter *socket* "/tmp/mpv-cl-socket")
(defparameter *playing-url* nil)

(defun youtube-url-p (url-or-song)
  "Decide if the string is a youtube url and return it back if it is."
  (and (or (scan-to-strings "youtube\.com" url-or-song)
           (scan-to-strings "youtu\.be" url-or-song))
       url-or-song))

(defun url-from-string (str)
  "Use youtube-dl to search for a youtube url based on the input string.
The --get-url option returns a nasty url. --get-thumbnail returns a url that
contains the url id we're interested it."
  (let ((url-id nil))
    (with-output-to-string (out)
      (run-program
       (format nil
               "youtube-dl --get-thumbnail \"ytsearch:~a\""
               str)
       :output out)
      (let* ((thumb-url (get-output-stream-string out))
             (partial-id (scan-to-strings "/vi/.*/" thumb-url)))
        (setf url-id (subseq partial-id 4 (- (length partial-id) 1)))))
    (format nil "https://www.youtube.com/watch?v=~a" url-id)))

(defun play (url-or-song &key (video nil) (pos "0"))
  "Play the youtube url through mpv. If video is T, open the video with mpv
player, if not, run mpv in the background. If position is specified, in seconds,
start playback from there."
  (let ((url (or (youtube-url-p url-or-song)
                 (url-from-string url-or-song))))
    (set-playing-url url)
    (handler-case
        (run-program
         (concatenate 'string
                      "mpv --ytdl-format=best --input-ipc-server="
                      *socket* " "
                      (unless video
                        "--vid=no ")
                      url
                      "\\&feature=youtu.be\\&t=" pos))
      (uiop/run-program:subprocess-error (e)
        (declare (ignore e))
        ;; Even though the youtube url is valid, some videos are not available;
        ;; mpv just crashes in this case; ignore it and move along
        (set-playing-url nil)
        nil)))
  ;; After mpv finishes and closes, run-program returns and nothing is playing.
  (clear-playing-url))

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

(defun duration ()
  "Current playing song duration, in MM:SS format, as string."
  (write-to-string (round (get-property "duration"))))

(defun turn-video-on ()
  "Quit mpv and restart it in video mode, locally (i.e. not in the browser)"
  (let ((url (playing-url))
        (pos (time-pos)))
    (quit)
    ;wait for mpv to close and url to be cleared, otherwise, the clearing of the
    ;url might happen after playing the video which would result in a state
    ;where there is no playing url but mpv is running.
    (sleep 1)                           
    (make-thread
     (lambda ()
       (play url :video t :pos pos)))))

(defun switch-to-browser (&key (from-beginning nil))
  "Pause the player and open the youtube page of the current playing song in the
user default browser. If from-beginning is T, start playing from the beginning,
otherwise continue from where the player was. "
  (pause)              ;don't want mpv and youtube to both run at the same time
  (launch-program
   (concatenate
    'string  "xdg-open \"" (playing-url)
    (unless from-beginning
      (concatenate
       'string "&feature=youtu.be&t="
       (time-pos)
       ;; url for xdg-open must be surrounded by quotes
       "\"")))))

(defun quit ()
  "Stop the playback."
  (send-command "quit")
  (clear-playing-url))
