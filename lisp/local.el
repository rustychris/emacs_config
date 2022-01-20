(setq python-shell-interpreter "ipython")

(if (display-graphic-p)
    (progn
      (setq python-shell-interpreter-args "--simple-prompt --matplotlib=qt")
      ;; laptop, this is about right
      (set-face-attribute 'default nil :height 140))
  (setq python-shell-interpreter-args "--simple-prompt --matplotlib=agg"))

(if (string-match "linux" system-configuration)
    (progn
      (setq path "/home/rusty/miniconda3/envs/general/bin:/home/rusty/miniconda3/condabin:/home/rusty/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin")
      (setenv "PATH" path)
      (setenv "PROJ_LIB" "/home/rusty/miniconda3/envs/general/share/proj")
      (setq exec-path (append exec-path '("/home/rusty/miniconda3/envs/general/bin")))
      (setq exec-path (append exec-path '("/usr/local/bin")))
      ))

(if (string-match "w64" system-configuration)
    (setq python-shell-interpreter "C:\\Users\\rusty\\src\\ipython-general.bat"
          python-shell-interpreter-args "")

;; (setq python-shell-interpreter "C:\\Users\\rusty\\Miniconda3\\envs\\general\\Scripts\\jupyter"
;;       python-shell-interpreter-args "console --simple-prompt")

(defun run-python-cws ()
  "Run a python shell on cws-linuxmodeling.  Currently this
   only works when invoked from a remote python buffer."
  (interactive)
  (let ((python-shell-interpreter "/opt/anaconda3/bin/ipython")
        (python-shell-interpreter-args "--simple-prompt --matplotlib=agg"))
    (run-python)))



