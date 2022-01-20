(setq path "/home/rusty/miniconda3/envs/general/bin:/home/rusty/miniconda3/condabin:home/rusty/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin")
(setenv "PATH" path)

(setenv "PROJ_LIB" "/home/rusty/miniconda3/envs/general/share/proj")

; (setenv "PYTHONPATH" "/Users/rusty/python")
(setq exec-path (append exec-path '("/home/rusty/miniconda3/envs/general/bin")))
(setq exec-path (append exec-path '("/usr/local/bin")))

; python.el settings:
(setq python-shell-interpreter "ipython")
(setq python-shell-interpreter-args "--simple-prompt --matplotlib=agg")

