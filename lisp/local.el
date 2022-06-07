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
    (progn
      (setq ; python-shell-interpreter "C:\\Users\\rusty\\src\\ipython-general.bat"
                                        ; python-shell-interpreter-args ""
       python-shell-interpreter "jupyter"
       python-shell-interpreter-args "console --simple-prompt"
       magit-git-executable "C:\\Users\\rusty\\Miniconda3\\Library\\bin\\git.exe"
       grep-command "\"C:\\Program Files (x86)\\GnuWin32\\bin\\grep.exe\" -nH -e "
       exec-path (cons "C:/Program Files (x86)/GnuWin32/bin" exec-path)
       ;ediff-cmp-program "\"C:\\Program Files (x86)\\GnuWin32\\bin\\cmp.exe\""
       ;ediff-diff-program "\"C:\\Program Files (x86)\\GnuWin32\\bin\\diff.exe\""
       diff-command "\"C:\\Program Files (x86)\\GnuWin32\\bin\\diff.exe\"")
      (setenv "PYTHONPATH" "C:\\Users\\rusty\\src\\stompy")))
  
;; (setq python-shell-interpreter "C:\\Users\\rusty\\Miniconda3\\envs\\general\\Scripts\\jupyter"
;;       python-shell-interpreter-args "console --simple-prompt")

(defun run-python-ipython ()
  "Run a python shell on cws-linuxmodeling.  Currently this
   only works when invoked from a remote python buffer."
  (interactive)
  (let (;(python-shell-interpreter "C:\\Users\\rusty\\Miniconda3\\envs\\general\\Scripts\\ipython.exe")
        (python-shell-interpreter "C:\\Users\\rusty\\src\\ipython-wx.bat")
        (python-shell-interpreter-args ""))
    (run-python)))



