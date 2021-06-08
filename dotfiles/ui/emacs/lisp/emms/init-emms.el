;;; Emms

;;; TODO: See if mpd is faster at building the db. Not so important.
;;; TODO: Change face from purple to white?
;;; TODO: emms-all causes some "require"d files to be loaded twice if called after, say, emms-browser was loaded.
(emms-all)
(emms-history-load)
(emms-mode-line-disable)

(defun ambrevar/emms-playing-time-toggle ()
  (if (or (derived-mode-p 'emms-browser-mode)
          (derived-mode-p 'emms-playlist-mode))
      (emms-playing-time-mode-line)
    (emms-playing-time-restore-mode-line)))
(add-hook 'window-state-change-hook 'ambrevar/emms-playing-time-toggle)

(setq emms-player-list (list emms-player-mpv)
      emms-source-file-default-directory (file-truename "~/Music")
      emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find
      ;; Cover thumbnails.
      emms-browser-covers 'emms-browser-cache-thumbnail-async)
(add-to-list 'emms-player-mpv-parameters "--no-audio-display")
(add-to-list 'emms-info-functions 'emms-info-cueinfo)

(require 'emms-info-libtag)
(if (executable-find emms-info-libtag-program-name)
    (progn
      (add-to-list 'emms-info-functions 'emms-info-libtag)
      (delete 'emms-info-ogginfo emms-info-functions)
      (delete 'emms-info-mp3info emms-info-functions))
  (add-to-list 'emms-info-functions 'emms-info-ogginfo)
  (add-to-list 'emms-info-functions 'emms-info-mp3info))

(when (require 'helm-emms nil t)
  (setq helm-emms-dired-directories (list emms-source-file-default-directory
                                          "~/humour"
                                          "~/Soundtrack"))
  (setq helm-emms-default-sources
        '(helm-source-emms-dired
          helm-source-emms-files ; Disable for a huge speed-up.
          helm-source-emms-streams)))

(defun ambrevar/emms-play-on-add (old-pos)
  "Play tracks when calling `emms-browser-add-tracks' if nothing
is currently playing."
  (interactive)
  (when (or (not emms-player-playing-p)
            emms-player-paused-p
            emms-player-stopped-p)
    (with-current-emms-playlist
      (goto-char old-pos)
      ;; if we're sitting on a group name, move forward
      (unless (emms-playlist-track-at (point))
        (emms-playlist-next))
      (emms-playlist-select (point)))
    (emms-stop)
    (emms-start)))
(add-hook 'emms-browser-tracks-added-hook 'ambrevar/emms-play-on-add)

;;; Display album in playlist
(defun ambrevar/emms-artist-album-track-and-title-format (bdata fmt)
  (concat
   "%i"
   (let ((artist (emms-browser-format-elem fmt "a")))
     (if (not artist)
         "%n"                           ; If unknown, display the filename.
       (concat
        "%a - "
        (let ((album (emms-browser-format-elem fmt "A")))
          (if album "%A - " ""))
        (let ((disc (emms-browser-format-elem fmt "D")))
          (if (and disc (not (string= disc ""))) "%D/" ""))
        (let ((track (emms-browser-format-elem fmt "T")))
          (if (and track (not (string= track "0")))
              "%T. "
            ""))
        "%t [%d]")))))
(setq emms-browser-playlist-info-title-format 'ambrevar/emms-artist-album-track-and-title-format)
;; Display disc number in browser
(defun ambrevar/emms-browser-track-artist-and-title-format (bdata fmt)
  (concat
   "%i"
   (let ((disc (emms-browser-format-elem fmt "D")))
     (if (and disc (not (string= disc "")))
         "%D/"))
   (let ((track (emms-browser-format-elem fmt "T")))
     (if (and track (not (string= track "0")))
         "%T. "
       ""))
   "%n"))
(setq emms-browser-info-title-format 'ambrevar/emms-browser-track-artist-and-title-format)

(defun ambrevar/emms-time-for-display (track)
  "Inspired by `emms-playing-time-display'."
  (let* ((total-playing-time
          (or (emms-track-get
               track
               'info-playing-time)
              0))
         (total-min-only (/ total-playing-time 60))
         (total-sec-only (% total-playing-time 60)))
    (format "%02d:%02d" total-min-only total-sec-only)))

(defun ambrevar/emms-track-description-with-album (track)
  "Simple function to give a user-readable description of a track.
If it's a file track, just return the file name.  Otherwise,
return the type and the name with a colon in between.
Hex-encoded characters in URLs are replaced by the decoded
character."
  (let ((type (emms-track-type track)))
    (cond ((eq 'file type)
           (cl-flet ((fmt (string &optional suffix prefix)
                          (if string
                              (concat prefix string suffix)
                            "")))
             (concat
              (fmt (emms-track-get track 'info-artist) " - ")
              (fmt (emms-track-get track 'info-album) " - ")
              (fmt (emms-track-get track 'info-discnumber) "/")
              (if (emms-track-get track 'info-tracknumber)
                  (format "%02d. " (string-to-number (emms-track-get track 'info-tracknumber)))
                "")
              (emms-track-get track 'info-title)
              (fmt (ambrevar/emms-time-for-display track) "]" " ["))))
          ((eq 'url type)
           (emms-format-url-track-name (emms-track-name track)))
          (t (concat (symbol-name type)
                     ": " (emms-track-name track))))))
(setq emms-track-description-function 'ambrevar/emms-track-description-with-album)

(defun ambrevar/emms-tracks-duration (begin end)
  "Display play time of tracks in region."
  (interactive "r")
  (cl-flet ((track-at-point ()
                            (if (eq major-mode 'emms-playlist-mode)
                                (emms-playlist-track-at)
                              (car (emms-browser-tracks-at-point)))))
    (if (not (use-region-p))
        (message "%s" (emms-browser-track-duration
                       (track-at-point)))
      (let* ((total-time)
             (first-line (line-number-at-pos begin))
             (last-line (line-number-at-pos end))
             (count 0))
        (setq total-time
              (save-mark-and-excursion
                (cl-loop for line from first-line to last-line
                         do (goto-line line)
                         for time = (emms-track-get (track-at-point)
                                                    'info-playing-time)
                         when time
                         do (cl-incf count)
                         when time
                         sum time)))
        (message "%02d:%02d for %d tracks" (/ total-time 60) (% total-time 60)
                 count)))))

(provide 'init-emms)
