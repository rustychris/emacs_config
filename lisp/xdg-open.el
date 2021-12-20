(load-library "dired")

(defun rcd/dired-view ()
  "View files, either as HTML or media"
  (interactive)
  (let* ((files (dired-get-marked-files))
     (how-many (length files))
     (extensions (mapcar 'file-name-extension files))
     (extensions (mapcar 'downcase extensions)))
(cond ((member "html" extensions) (eww-open-file (car files)))
      ((member "mp4" extensions) (emms-play-dired))
      ((member "mp3" extensions) (emms-play-dired))
      ((member "ogg" extensions) (emms-play-dired))
      (t (if (> how-many 1) (xdg-open-files files)
	   (xdg-open (car files) t))))))

(defun xdg-open (file &optional async)
  "Opens file with xdg-open. Without optional argument ASYNC, it will wait for the file to finish playing or review."
  (let ((command (format "xdg-open '%s'" file)))
(if async
    (async-shell-command command)
  (shell-command command))))

(defun xdg-open-files (files)
  "Opens list of files with xdg-open one by one, waiting for each to finish."
  (dolist (file files)
(xdg-open file)))

;; Finally mapping the key V to dired-mode-map
(define-key dired-mode-map "V" 'rcd/dired-view)
