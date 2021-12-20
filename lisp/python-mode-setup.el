(add-to-list 'load-path "/home/rusty/.emacs.d/python-mode")
(setq py-install-directory "/home/rusty/.emacs.d/python-mode")


(message "about to load python mode")
(require 'python-mode)
(message "loaded python mode")

; These are fine for python buffer, but doesn't help for ipython shell.
; (autoload 'jedi:setup "jedi" nil t)
; (add-hook 'python-mode-hook 'jedi:setup)
; (setq jedi:setup-keys t)
; (setq jedi:complete-on-dot t)

; tab completion is not as smooth as it should be - slow, and tends to popup the messages
; window.
; TAB currently bound to (py-indent-or-complete)
; C-h-k TAB says: in (I)Python shell-modes `py-shell-complete'
; (defalias 'ipython-complete 'py-shell-complete)
; calls   (py--complete-prepare shell debug beg end word nil))

; ipython's tab completion is way better - is it possible to just use that?
; maybe the issue is that this:
;   (string-match "ipython" (py-choose-shell))
; return false, beacuse py-choose-shell


; nope - calling py-shell-complete directly does the same thing.
; seems to eventually call: (py--complete-prepare shell debug beg end word nil)

; still same behavior...
; (py--complete-prepare (process-name (get-buffer-process (current-buffer))))

; but py-ipython-shell-mode has the bits where it sends completion setup
; code to the shell...

; (process-name (get-buffer-process buffer))

; (string-match "[Ii][Pp]ython*" (py-choose-shell))

; Does py-set-ipython-completion-command-string get called in py--complete-base?
; yes!


; code ends up:
; "print(';'.join(get_ipython().Completer.all_completions('%s'))) #PYTHON-MODE SILENT
; "
; manually feeding:
; print(';'.join(get_ipython().Completer.all_completions('res'))) #PYTHON-MODE SILENT
; to ipython, gives research/;%reset;%reset_selective;reshape;resize;restoredot;result_type
; which is the same as what emacs gets, less the magic commands.

; presumably the python code is actually getting called, and emacs is dropping the magics?
; the next call is (py--shell-do-completion-at-point proc imports word pos py-exception-buffer code)

; first question - is imports passed in?? nope.
; next question - why is (sit-for 0.1 t) called? don't know.
; there is another pause - 0.2 s, in py--shell-completion-get-completions

; what completions come back?
; ("research/" "%reset" "%reset_selective" "reshape" "resize" "restoredot" "result_type")
; just like asking ipython directly

; presumably this list gets passed on to py--try-completion

; maybe time to look at the python side, or at jedi?
; site-packages/IPython/core/completer.py
; IPCompleter.all_completions(text)
; returns IPCompleter.complete(text)[0]

; Aha - the key difference is here:
; get_ipython().Completer.complete('res','cd res')[1] => 'research/'
;  - the completer takes an optional second parameter giving the whole line.

; to fix this, or use a better autocompleter?

; jedi?
; pymacs?

;;

; What about going back to the good parts of ipython.el?
; it's not calling ipython-shell-hook
; manually calling ipython-shell-hook works [M-: (ipython-shell-hook)], 
; which enables dir tracking, and also breaks completion.
; (require 'ipython)

; python-mode has py-dirstack-hook, though.  Had to add a line to get 
; py-ipython-shell-mode to call the hooks, but dir tracking now works.
(add-hook 'py-python-shell-mode-hook 'py-dirstack-hook)


; (require 'python-mode-cell)

; python mode cell mode:
; Invalid face reference: python-cell [13 times]

;; have to set this in the mode map, which overrides global-set-key
(define-key python-mode-map [(control c)(|)] 'py-execute-region-no-switch)
; completion is screwy inside code buffer
(define-key python-mode-map [(tab)] 'py-indent-line)

(load-library "python-mode-cell.el")
(add-hook 'python-mode-hook #'python-mode-cell-mode 1)

; cell highlighting flickers a lot - disable it.
(setq python-cell-highlight-cell nil)

(setq py-shell-name "/home/rusty/anaconda/bin/ipython")
(setq py-python-command-args "--pylab --colors LightBG")
;(setq py-shell-name "ipython")
;(setq py-python-command-args (quote ("--pylab" "--colors" "LightBG")))
