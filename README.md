# youtube
Play songs from youtube given a youtube url or a search string. Uses youtube-dl and the mpv player.

# Installation

Make sure you have [mpv](https://mpv.io/) and
[youtube-dl](https://github.com/ytdl-org/youtube-dl) installed.

```bash
# clone to local-projects for quickload access
git clone https://github.com/mihaiolteanu/youtube ~/quicklisp/local-projects/youtube
```

```common-lisp
; Register the new project
(ql:register-local-projects)
```

# Usage

```common-lisp
(ql:quickload :youtube)
```

Play a known url
```common-lisp
(play "https://www.youtube.com/watch?v=skpQ-joyuLw")
```

Or let youtube-dl search for a youtube url given an artist name and a song name and play that
```common-lisp
(play "anathema one last goodbye")
```

This will start an mpv player instance listening on the */tmp/mpv-cl-socket*
socket. By default, mpv runs with no video support, in the background.

Following this, you can control the playback by pausing the player, rewinding,
opening a youtube page or querying the playback position. See the API for
details.

# API

**play** _url-or-song &key (video nil) (pos "0")_
   
    Play the youtube url through mpv. If `video` is T, open the video with mpv
    player, if not, run mpv in the background. If position is specified, in seconds,
    start playback from there. If the youtube link is valid but the video is
    unavailable for some reason, return nil and don't play anything.

**play/pause**
  
    Toggle playing status.

**replay**
  
    Rewind the song at the beginning, effectively replaying it.
  
**seek** _seconds_
  
    Forward or backward play by seconds, if the seconds is negative.
  
**percent-pos**
  
    Current playing song position, in percent.
  
**time-pos**
  
    Current playing song position, in seconds, as string.
  
**duration**
  
    Current playing song duration, in MM:SS format, as string.
  
**switch-to-browser** _&key (from-beginning nil)_
    
    Pause the player and open the youtube page of the current playing song in the
    user default browser. If `from-beginning` is T, start playing from the beginning,
    otherwise continue from where the player was. 

**turn-video-on**
  
    Quit mpv and restart it in video mode, locally (i.e. not in the browser)
  
**quit**
  
    Stop the playback.

## Authors
Copyright (c) 2019 [Mihai Olteanu](www.mihaiolteanu.me)

Licensed under the [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html) license.

