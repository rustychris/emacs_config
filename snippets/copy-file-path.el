(defun xah-copy-file-path (&optional dir-path-only-p)
    "Copy the current buffer's file path or dired path to `kill-ring'.
If `universal-argument' is called first, copy only the dir path.
URL `http://ergoemacs.org/emacs/emacs_copy_file_path.html'
Version 2015-08-08
RH: modified to copy path of file at point when in dired"
    (interactive)
    (let ((fpath
           (if (equal major-mode 'dired-mode)
               ;default-directory
               (dired-copy-filename-as-kill 0)
             (if (null (buffer-file-name))
                 (user-error "Current buffer is not associated with a file.")
               (buffer-file-name)))))
      (kill-new
       (if (null dir-path-only-p)
           (progn
             (message "File path copied: %s" fpath)
             fpath
             )
         (progn
           (message "Directory path copied: %s" (file-name-directory fpath))
           (file-name-directory fpath))))))
(global-set-key (kbd "C-M-f") 'xah-copy-file-path)


