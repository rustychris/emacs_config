(require 'python)

; old
;;(add-to-list 'load-path "/home/rusty/.emacs.d/elpa/python-cell-20131029.1616")

;; note that machine-specific settings should go in local.el
(setq
  python-shell-prompt-regexp "In \\[[0-9]+\\]: "
  python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
  python-shell-completion-setup-code
    "from IPython.core.completerlib import module_completion"
  python-shell-completion-module-string-code
    "';'.join(module_completion('''%s'''))\n"
  python-shell-completion-string-code
    "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

; so for pythonic completions, it needs to be broken on word boundaries
; like arithmetic operations, but for ipython magic like ls, chdir, etc.
; we need to break on space.  Seems like a job for a smarter python
; function, and pass the whole line in.

(require 'python-cell)

(add-hook 'python-mode-hook #'python-cell-mode 1)

; by default, it takes over control up and control down
; actually, that's fine - move the paragraph jump keys to meta up / down
(define-key python-cell-mode-map [(control up)] 'python-backward-cell) 
(define-key python-cell-mode-map [(control down)] 'python-forward-cell) 
(define-key python-cell-mode-map [(meta down)] 'forward-paragraph)
(define-key python-cell-mode-map [(meta up)] 'backward-paragraph)



; Attempt some customization of cell mode
;  - add a comment '# sent' at the command line to indicate work in progress
;  - popup the shell window below current window, with relatively few lines  
; still to do: don't resize shell window if it already is displayed.
(defun python-shell-send-cell ()
  "Send the cell the cursor is in to the inferior Python process."
  (interactive)
  (let* ((start (save-excursion (python-beginning-of-cell)
                                (point)))
         (end (save-excursion (python-end-of-cell)
                              (point)))
         (current (current-buffer))
         (process (python-shell-get-or-create-process))
         (py-buffer (process-buffer process)))
    (set-buffer py-buffer)
    ;; (switch-to-buffer-other-window py-buffer)
    (message (format "py-buffer is %s" py-buffer))
    (if (get-buffer-window py-buffer "visible")
        nil
      (display-buffer py-buffer '( (display-buffer-reuse-window 
                                    display-buffer-below-selected)
                                   (window-height . 10 ))) )
    ;; move point to the process mark before insert
    (goto-char (process-mark process))
    (insert "# sent\n")
    (set-marker (process-mark process) (point))
    (set-buffer current)
    ;; calls python-shell-send-string in turn.
    (python-shell-send-region start end)))


;; keybindings for multiple-cursors
; (require 'multiple-cursors)
; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


(defun run-conda-python (env_name)
  "start ipython in a particular conda environment"
  (interactive (list (read-string "Environment: " "root")))
  (let* ((subpath (if (string= env_name "root")
                      "" (concat "envs/" env_name "/"))))
    ;; the 4 is necessary to get emacs to Show the buffer
    (run-python (concat "/opt/anaconda3/" subpath "bin/ipython"
                        (if (getenv "DISPLAY")
                            " --pylab=tk"
                          ""))
                nil 4)))


(require 'shell)
(add-hook 'inferior-python-mode-hook 'shell-dirtrack-mode)

;; shell completion gets an error: 
; completion--some: Symbol's value as variable is void: python-shell--prompt-calculated-input-regexp

;; issues:
;;  debugger makes buffers go crazy
;;  completion can't do multiple directories in the shell
;;  run-python, when it's already running, fails to switch to the buffer.
;;  sometimes sending a cell causes a bunch of blank lines to be sent - maybe fixed in 24.4?
;;  on occasion, it forgets the ending newline for a cell
;;  dir tracking is now fixed!

;; C-return is bound to python-shell-send-cell
;; that figures out the region, jumbles 
;; the buffers around so that the python shell is visible,
;; and calls (python-shell-send-region start end)

;; calling python-shell-send-region directly does the same thing.
;; why am I using emacs 24.3?
;; brew has installed 24.4
;; after sorting out the links, I'm running 24.4, and it's
;; possible that the blank lines issue is sorted out.

;; dir tracking: nothing in python.el about dir tracking.

;; see python-mode-setup.el for a more verbose way to do
;; this, which may come in handy if ipython departs too much
;; from  a regular shell.


;; completion:
;; TAB is bound to python-shell-completion-complete-or-indent

;; which then calls (completion-at-point)
;; which I think is then calling #'python-completion-complete-at-point

;; which then calls (python-shell-completion-complete-at-point process)
;; there's a fair bit of logic in that.

;; the end point is this:
;; ';'.join(get_ipython().Completer.all_completions('''res'''))

;; all_completions is for emacs -
;; the more complete function is:
;; ip.Completer.complete(text=None, line_buffer=None, cursor_pos=None)


;; had to copy this in from python.el.gz in order to get
;; python-shell-completion-get-completions
;; to parse
(eval-when-compile
  (defconst python-rx-constituents
    `((block-start          . ,(rx symbol-start
                                   (or "def" "class" "if" "elif" "else" "try"
                                       "except" "finally" "for" "while" "with")
                                   symbol-end))
      (dedenter            . ,(rx symbol-start
                                   (or "elif" "else" "except" "finally")
                                   symbol-end))
      (block-ender         . ,(rx symbol-start
                                  (or
                                   "break" "continue" "pass" "raise" "return")
                                  symbol-end))
      (decorator            . ,(rx line-start (* space) ?@ (any letter ?_)
                                   (* (any word ?_))))
      (defun                . ,(rx symbol-start (or "def" "class") symbol-end))
      (if-name-main         . ,(rx line-start "if" (+ space) "__name__"
                                   (+ space) "==" (+ space)
                                   (any ?' ?\") "__main__" (any ?' ?\")
                                   (* space) ?:))
      (symbol-name          . ,(rx (any letter ?_) (* (any word ?_))))
      (open-paren           . ,(rx (or "{" "[" "(")))
      (close-paren          . ,(rx (or "}" "]" ")")))
      (simple-operator      . ,(rx (any ?+ ?- ?/ ?& ?^ ?~ ?| ?* ?< ?> ?= ?%)))
      ;; FIXME: rx should support (not simple-operator).
      (not-simple-operator  . ,(rx
                                (not
                                 (any ?+ ?- ?/ ?& ?^ ?~ ?| ?* ?< ?> ?= ?%))))
      ;; FIXME: Use regexp-opt.
      (operator             . ,(rx (or "+" "-" "/" "&" "^" "~" "|" "*" "<" ">"
                                       "=" "%" "**" "//" "<<" ">>" "<=" "!="
                                       "==" ">=" "is" "not")))
      ;; FIXME: Use regexp-opt.
      (assignment-operator  . ,(rx (or "=" "+=" "-=" "*=" "/=" "//=" "%=" "**="
                                       ">>=" "<<=" "&=" "^=" "|=")))
      (string-delimiter . ,(rx (and
                                ;; Match even number of backslashes.
                                (or (not (any ?\\ ?\' ?\")) point
                                    ;; Quotes might be preceded by a escaped quote.
                                    (and (or (not (any ?\\)) point) ?\\
                                         (* ?\\ ?\\) (any ?\' ?\")))
                                (* ?\\ ?\\)
                                ;; Match single or triple quotes of any kind.
                                (group (or  "\"" "\"\"\"" "'" "'''"))))))
    "Additional Python specific sexps for `python-rx'")

  (defmacro python-rx (&rest regexps)
    "Python mode specialized rx macro.
This variant of `rx' supports common Python named REGEXPS."
    (let ((rx-constituents (append python-rx-constituents rx-constituents)))
      (cond ((null regexps)
             (error "No regexp"))
            ((cdr regexps)
             (rx-to-string `(and ,@regexps) t))
            (t
             (rx-to-string (car regexps) t))))))
                                       

;; line is 'cd res', input is 'res'
;; or 'cd research/ct' and 'ct'
;; the ipython completer is reasonably smart - we can just pass the whole line
;; to it, and it will figure out how to split the line, and figure out completions.


;; this isn't working - disable for now, try using system version.

; (defun python-shell-completion-get-completions (process line input)
;   "Do completion at point for PROCESS.
; LINE is used to detect the context on how to complete given INPUT."
;   (message (format "calling my completion on %s %s" line input)) ;; working!
;   (with-current-buffer (process-buffer process)
;     (let* ((prompt
;             ;; Get last prompt of the inferior process buffer (this
;             ;; intentionally avoids using `comint-last-prompt' because
;             ;; of incompatibilities with Emacs 24.x).
;             (save-excursion
;               (buffer-substring-no-properties
;                (line-beginning-position) ;End of prompt.
;                (re-search-backward "^"))))
;            (completion-code
;             ;; Check whether a prompt matches a pdb string, an import
;             ;; statement or just the standard prompt and use the
;             ;; correct python-shell-completion-*-code string
;             (cond ((and (> (length python-shell-completion-pdb-string-code) 0)
;                         (string-match
;                          (concat "^" python-shell-prompt-pdb-regexp) prompt))
;                    python-shell-completion-pdb-string-code)
;                   ((string-match
;                     python-shell--prompt-calculated-input-regexp prompt)
;                    python-shell-completion-string-code)
;                   (t nil)))
;            (input
;             (if (string-match
;                  (python-rx line-start (* space) (or "from" "import") space)
;                  line)
;                 line
;               input)))
;       (and completion-code
;            (> (length input) 0)
;            (let ((completions
;                   (python-util-strip-string
;                    (python-shell-send-string-no-output
;                     (format completion-code input) process))))
;              (and (> (length completions) 2)
;                   (split-string completions
;                                 "^'\\|^\"\\|;\\|'$\\|\"$" t)))))))



; The stock install attempts to pull up buffers from PDB status lines,
                                        ; and delete them at will.
;

(defun python-pdbtrack-comint-output-filter-function (output)
  "Move overlay arrow to current pdb line in tracked buffer.
Argument OUTPUT is a string with the output from the comint process."
  (when (and python-pdbtrack-activate (not (string= output "")))
    (let* ((full-output (ansi-color-filter-apply
                         (buffer-substring comint-last-input-end (point-max))))
           (line-number)
           (file-name
            (with-temp-buffer
              (insert full-output)
              ;; When the debugger encounters a pdb.set_trace()
              ;; command, it prints a single stack frame.  Sometimes
              ;; it prints a bit of extra information about the
              ;; arguments of the present function.  When ipdb
              ;; encounters an exception, it prints the _entire_ stack
              ;; trace.  To handle all of these cases, we want to find
              ;; the _last_ stack frame printed in the most recent
              ;; batch of output, then jump to the corresponding
              ;; file/line number.
              (goto-char (point-max))
              (when (re-search-backward python-pdbtrack-stacktrace-info-regexp nil t)
                (setq line-number (string-to-number
                                   (match-string-no-properties 2)))
                (match-string-no-properties 1)))))
      (if (and file-name line-number)
          (let* ((tracked-buffer
                  (python-pdbtrack-set-tracked-buffer file-name))
                 (shell-buffer (current-buffer))
                 (tracked-buffer-window (get-buffer-window tracked-buffer))
                 (tracked-buffer-line-pos))
            (with-current-buffer tracked-buffer
              (set (make-local-variable 'overlay-arrow-string) "=>")
              (set (make-local-variable 'overlay-arrow-position) (make-marker))
              (setq tracked-buffer-line-pos (progn
                                              (goto-char (point-min))
                                              (forward-line (1- line-number))
                                              (point-marker)))
              (when tracked-buffer-window
                (set-window-point
                 tracked-buffer-window tracked-buffer-line-pos))
              (set-marker overlay-arrow-position tracked-buffer-line-pos))
            (pop-to-buffer tracked-buffer)
            (switch-to-buffer-other-window shell-buffer))
        (when python-pdbtrack-tracked-buffer
          (with-current-buffer python-pdbtrack-tracked-buffer
            (set-marker overlay-arrow-position nil))
          ; (mapc #'(lambda (buffer)
          ;           (ignore-errors (kill-buffer buffer)))
          ;       python-pdbtrack-buffers-to-kill)
          (setq python-pdbtrack-tracked-buffer nil
                python-pdbtrack-buffers-to-kill nil)))))
  output)
