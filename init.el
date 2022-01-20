; -*-Lisp-*-

(add-to-list 'load-path (substitute-in-file-name "$HOME/.emacs.d/lisp"))

(load-library "local.el")

(line-number-mode 1)

(setq c-electric-flag nil)
(setq inhibit-startup-message t)
(setq display-time-echo-area t)
(setq dired-use-ls-dired nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
	
(setq-default ispell-program-name "aspell")

;; keep splits the old way, where the split is a horizontal line.
(setq split-height-threshold 0) 
(setq split-width-threshold nil)

(setq compilation-window-height 15)

(autoload 'perl-mode  "perl-mode" "Perl Editing Mode" t)
(autoload 'c++-mode "cc-mode" "C++ Editing Mode" t)
(autoload 'c-mode   "cc-mode" "C Editing Mode" t)
(autoload 'css-mode "css-mode")

(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
(autoload 'maxima "maxima" "Maxima interaction" t)
(autoload 'imath-mode "imath" "Imath mode for math formula input" t)
(setq imaxima-use-maxima-mode-flag t)


(setq auto-mode-alist
      (append '(("\\.C$"  . c++-mode)
		("\\.cc$" . c++-mode)
		("\\.h$"  . c++-mode)
		("\\.y$"  . indented-text-mode)
		("\\.l$"  . indented-text-mode)
		("\\.c$"  . c-mode)
		("\\.S$"  . c-mode)
		("\\.scm$" . scheme-mode)
		("\\.ss$" . scheme-mode)
		("\\.pl$" . perl-mode)
		("\\.tex$" . latex-mode)
		("\\.html$" . html-mode)
		("\\.rhtml$" . html-mode)
		("\\.adp$" . html-mode)
		("\\.psp$" . html-mode)
		("\\.tpl$" . html-mode)
		("\\.java$" . java-mode)
		("\\.rb$" . ruby-mode)
                ("\\.mc$" . maxima-mode)
                ("\\.m$" . octave-mode) 
                ("\\.css\\'" . css-mode)
                ("\\.rhtml$" . html-mode)
		) auto-mode-alist))

; avoid doing extra work since I don't use builtin vc
(setq vc-handled-backends nil)

; (setq gc-cons-threshold 524288)         ;512K
(setq default-case-fold-search nil)     ;distingush upper/lower cases
; superseded in better defaults? (setq require-final-newline t)          ;ask
(setq enable-recursive-minibuffers t)   ;use recursive minibuffers...
(setq auto-save-interval 100)           ;save this often
(setq default-major-mode 'text-mode)    ;some hooks...

(setq shell-prompt-pattern "^[^#$%>)]*[#$%>)]")

(setq scroll-step 1)

(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-cq" 'query-replace)
(global-set-key "\C-u" 'scroll-down)
(global-set-key "\M-\C-h" 'backward-kill-word)
(global-set-key "\C-c\C-r" 'comment-region)
(setq-default indent-tabs-mode nil) ; no tabs - everything is a space (might screw up Makefiles...)

(global-set-key (kbd "<C-tab>") 'next-buffer)
; no idea why it has to be so different for shift-control-tab
(global-set-key (kbd "<C-S-iso-lefttab>") 'previous-buffer)

(global-set-key (kbd "<C-S-up>") 'scroll-up-line)
(global-set-key (kbd "<C-S-down>") 'scroll-down-line)

(global-set-key (kbd "C-x O") 'previous-multiframe-window)
; these are typically bound to backward/forward-page, 
; but more commonly I want to go to the top/end of 
; the buffer
(global-set-key (kbd "C-x [") 'beginning-of-buffer)
(global-set-key (kbd "C-x ]") 'end-of-buffer)

;; Reasonable f-key bindings
(global-set-key [M-left] 'backward-word)
(global-set-key [M-right] 'forward-word)
(global-set-key [f33] 'end-of-buffer)           ; R13
(global-set-key [f35] 'scroll-up)               ; R15

(global-set-key [f1] 'next-error)               ; F8
(global-set-key [f2] 'compile)                  ; F5
(global-set-key [f3] 'enlarge-window)           ; F1
(global-set-key [f4] 'shrink-window)            ; F2
(global-set-key [f5] 'call-last-kbd-macro)      ; F3
(global-set-key [f6] 'goto-line)                ; F4
(global-set-key [f7] 'c++-indent-defun)         ; F7
(global-set-key [f8] 'query-replace)            ; F10
(global-set-key [f9] 'grep)                     ; F6
(global-set-key [f10] 'gdb)                     ; F9
(global-set-key [f11] 'keyboard-quit)           ; L1, F11

(setq c++-mode-hook 'font-lock-mode)
(setq c-mode-hook 'font-lock-mode)

(setq initial-major-mode 'text-mode)
(setq default-major-mode 'text-mode)

;; save myself from the horror of set-fill-column...
(global-unset-key "\C-xf")

;; compile command
(setq compile-command "make ")

;;;;;;;;;;;;;;;;;; COLOR STUFF ;;;;;;;;;;;;

;;   (set-background-color "white")
;;   (set-foreground-color "black")
;;   (set-cursor-color "green")
;;   (set-mouse-color "green")
;;   (set-face-foreground 'default "black")
;;   (set-face-background 'default "white")
;;   
;;     ;; function names
;;   (set-face-foreground 'bold-italic "turquoise")
;;     ;; comments
;;   (set-face-foreground 'italic "orange")
;;     ;; strings  (used to be salmon)
;;   (set-face-foreground 'underline "SpringGreen")
;;     ;; keywords
;;   (set-face-foreground 'bold "DarkOrange1")
;;     ;; types
;;   (set-face-foreground 'highlight "plum")
;;   (set-face-background 'highlight "black")
;;     ;; this is a region highlighted by selection
;;   (set-face-foreground 'region "yellow")
;;   (set-face-background 'region "purple3")
;;   
;;     ;; turn of annoying underlining
;;   (set-face-underline-p 'bold nil)
;;   (set-face-underline-p 'underline nil)

(setq truncate-partial-width-windows nil)
(setq global-font-lock-mode 't)
(set-scroll-bar-mode nil)

(setq revert-without-query '(".*"))
(global-set-key [f12] 'revert-buffer)
 
;; Trying to make the mac keyboard feel more emacs-ish
(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
  )

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

;; Matlab stuff
(defun my-matlab-mode-hook ()
  (setq matlab-indent-function t))       ; if you want function bodies indented
(setq matlab-mode-hook 'my-matlab-mode-hook)
(autoload 'matlab-shell "matlab" "Interactive Matlab mode." t)
(defun my-matlab-shell-mode-hook ()
  '())
(setq matlab-mode-hook 'my-matlab-mode-hook)


(defun xah-copy-file-path (&optional dir-path-only-p)
  "Copy the current buffer's file path or dired path to `kill-ring'.
If `universal-argument' is called first, copy only the dir path.
URL `http://ergoemacs.org/emacs/emacs_copy_file_path.html'
Version 2015-08-08"
  (interactive "P")
  (let ((fpath
         (if (equal major-mode 'dired-mode)
             default-directory
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

(global-set-key (kbd "C-$") 'hs-show-block)
(global-set-key (kbd "C-#") 'hs-hide-block)

; really, the detractors of python.el are
; (1) didn't have dir tracking - fixable.
; (2) completion is buggy
; (3) sending a cell sometimes prints out a long string of blank lines.
; (4) debugger goes nuts with windows

; python-mode.el:
; (1) not sure about the debugger - python.el retains line numbers, which is nice.
; (2) doesn't have cell mode, quite yet.. FIXED
; (3) completion also buggy
; (4) execute-region switches to *scratch* FIXED

(load-library "python-setup.el") ; this one works reasonably well.

;; too old to learn how to use ido-mode.
(ido-mode -1) 

;;
;; shot in the dark to make cells less annoying.
(setq python-cell-cellbreak-regexp
      (rx line-start (* space)
          (group (and "#" (or (and "#" (* (not (any "\n"))))
                              (and " <" (or "codecell" "markdowncell") ">"))
                      line-end))) )
(setq visible-bell nil)

;; 
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(better-defaults
    material-theme
    lush-theme))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)


;;

(load-theme 'lush t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ess magit better-defaults zenburn-theme zen-and-art-theme yapfify ws-butler winum white-sand-theme which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme toc-org tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit sunny-day-theme sublime-themes subatomic256-theme subatomic-theme sql-indent spaceline spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle slim-mode seti-theme scss-mode sass-mode reverse-theme restart-emacs rebecca-theme rainbow-delimiters railscasts-theme pyvenv python-cell pytest pyenv-mode py-isort purple-haze-theme pug-mode professional-theme popwin planet-theme pip-requirements phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode pcre2el paradox orgit organic-green-theme org-bullets open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme neotree naquadah-theme mustang-theme move-text monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minimal-theme material-theme markdown-toc majapahit-theme magit-gitflow madhat2r-theme macrostep lush-theme lorem-ipsum livid-mode live-py-mode linum-relative link-hint light-soap-theme less-css-mode json-mode js2-refactor js-doc jbeans-theme jazz-theme ir-black-theme inkpot-theme indent-guide hy-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation heroku-theme hemisu-theme helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-css-scss helm-ag hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme google-translate golden-ratio gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md gandalf-theme flx-ido flatui-theme flatland-theme fill-column-indicator farmhouse-theme fancy-battery eyebrowse expand-region exotica-theme exec-path-from-shell eval-sexp-fu espresso-theme emmet-mode elisp-slime-nav dumb-jump dracula-theme django-theme diminish define-word darktooth-theme darkokai-theme darkmine-theme darkburn-theme dakrone-theme cython-mode cyberpunk-theme csv-mode column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized coffee-mode clues-theme clean-aindent-mode cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme auto-highlight-symbol auto-compile apropospriate-theme anti-zenburn-theme anaconda-mode ample-zen-theme ample-theme alect-themes aggressive-indent afternoon-theme adaptive-wrap ace-window ace-link ace-jump-helm-line)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(global-eldoc-mode -1)

;; Running a remote python?
; (python-shell-calculate-command)
; that returns "ipython --simple-prompt --matplotlib=qt"
; (run-python "/opt/anaconda3/bin/ipython --simple-prompt")
(load-library "xdg-open.el")

