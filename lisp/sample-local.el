(setq path "/Users/rusty/anaconda/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/texbin")
(setenv "PATH" path)
(setenv "PYTHONPATH" "/Users/rusty/python")
(setq exec-path (append exec-path '("/Users/rusty/anaconda/bin")))
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (append exec-path '("/usr/texbin")))

(add-to-list 'load-path "/usr/local/share/maxima/5.28.0/emacs")
(add-to-list 'load-path "/Users/rusty/.emacs.d/elpa/python-cell-20131029.1616")

; python.el settings:
(setq python-shell-interpreter "/Users/rusty/anaconda/bin/ipython")
(setq python-shell-interpreter-args "--pylab=tk") ; This had been commented out - why?

; or on linux:
; (setq python-shell-interpreter "ipython")
; (setq python-shell-interpreter-args "--pylab")

