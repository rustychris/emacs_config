(setq path "/home/rusty/miniconda3/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin")
(setq path "/home/rusty/miniconda3/envs/general/bin:/home/rusty/miniconda3/condabin:home/rusty/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin")
(setenv "PATH" path)

(setenv "PROJ_LIB" "/home/rusty/miniconda3/envs/general/share/proj")

; (setenv "PYTHONPATH" "/Users/rusty/python")
(setq exec-path (append exec-path '("/home/rusty/miniconda3/envs/general/bin")))
(setq exec-path (append exec-path '("/usr/local/bin")))

; (add-to-list 'load-path "/Users/rusty/.emacs.d/elpa/python-cell-20131029.1616")

; python.el settings:
; (setq python-shell-interpreter "/home/rusty/anaconda/bin/ipython")
; (setq python-shell-interpreter-args "--pylab=tk") ; This had been commented out - why?

; or on linux:
(setq python-shell-interpreter "ipython")
(setq python-shell-interpreter-args "--simple-prompt --matplotlib=qt")

(defun run-python-cws ()
  "Run a python shell on cws-linuxmodeling.  Currently this
   only works when invoked from a remote python buffer."
  (interactive)
  (let ((python-shell-interpreter "/opt/anaconda3/bin/ipython")
        (python-shell-interpreter-args "--simple-prompt --matplotlib=agg"))
    (run-python)))
     
