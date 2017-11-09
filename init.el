;; EMACS INIT

;; open backtrace buffer when something goes wrong
;; (set 'debug-on-error t)

;; dont load outdated byte code
(setq load-prefer-newer t)

;; initial messages
(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;; this line must exist; do not remove
(package-initialize)

;; configure auto file backups
;; set convenience variables
(defvar dir-file-backups (concat user-emacs-directory "file_backups/"))
(defvar dir-file-autosaves (concat dir-file-backups "autosaves/"))

;; create file backup directories if they dont exist
(unless (file-exists-p dir-file-backups) (make-directory dir-file-backups))
(unless (file-exists-p dir-file-autosaves) (make-directory dir-file-autosaves))

;; file to store all the active auto save file names
(setq auto-save-list-file-name (concat dir-file-autosaves "auto-save-list"))

;; directory to save autosaves - these are temp files for edits that have not yet
;; been committed to the source file ( #filename.ext# )
(setq auto-save-file-name-transforms `((".*" ,(concat dir-file-autosaves "\\1") t)))

;; directory for file backups ( filename.ext~ )
(setq backup-directory-alist `(("." . ,dir-file-backups)))

;; misc backup configuration
(setq
 backup-by-copying t
 delete-old-versions t
 kept-new-versions 3
 kept-old-versions 1
 version-control nil)

;; configure custom file
;; this is where emacs will place all of its auto-saved config
;; create file if it doesnt exist
(defvar custom-file-path (concat user-emacs-directory "auto_custom.el"))
(unless (file-exists-p custom-file-path) (write-region "" nil custom-file-path))

;; use own custom file path
(setq custom-file custom-file-path)
(load custom-file)

;; frame titles should show filename by default (even if only one frame exists)
(setq frame-title-format "%b")

;; require trailing newline on file load AND save
(setq require-final-newline 'visit-save)

;; tabs (and evil mode shifts) 4 spaces wide
(setq-default tab-width 4)
(setq-default evil-shift-width 4)

;; use spaces instead of tabs by default
;; use helpers/gp-indent-infer-spaces-or-tabs or helpers/gp indent-use-tabs and gp-indent-use-spaces to
;; switch modes if needed
(setq-default indent-tabs-mode nil)

;; match braces, parens, quotes etc
(electric-pair-mode)
; and highlight them
(show-paren-mode)

;; stop dired creating new buffers when entering directories
(require 'dired)
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
(define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))
(put 'dired-find-alternate-file 'disabled nil)

;; scroll settings
(setq mouse-wheel-scroll-amount '(3)) ;; three lines at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
;; (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;; (setq scroll-conservatively 101) ;; move minimum when cursor exits view, instead of recentering
;; (setq scroll-step 1) ;; keyboard scroll one line at a time

;; enable line numbers
;; (global-linum-mode)
;; add a bit of spacing around line numbers
(setq linum-format " %d ")

;; enable line highlight
;; (global-hl-line-mode)

;; disable fringes by default (use set-fringe-style command to change it within a session)
(set-fringe-style '(1 . 1))

;; disable cursor blinking by default
(blink-cursor-mode 0)

;; expose gp/ init files
(add-to-list 'load-path (concat user-emacs-directory "gp"))

;; load helper functions
;; do this before loading other init files, as they might depend on helper functions
(require 'init_helpers)

;; use 2 spaces in el files
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (setq tab-width 2)
                                  (setq evil-shift-width 2)
                                  (gp-indent-use-spaces)))

;; infer whether to use spaces or tabs in java
(add-hook 'java-mode-hook 'gp-indent-infer-spaces-or-tabs)

;; load themes, set mode line
(require 'init_themes)
;; set mode line
(gp-set-mode-line)
;; disable mode line by default
;; we set up the mode line content and faces first so if we enable it
;; later it'll have the correct settings
;; (hidden-mode-line-mode)

;; load machine specific configuration
;; this should overwrite certain functions and variables, so
;; make sure to load it at an appropriate time
(gp-determine-machine)

;; expose gp/plugins files
(add-to-list 'load-path (concat user-emacs-directory "gp/plugins"))

;; load session plugin
(require 'session)

;; set up packages
(require 'init_packages)

;; set up themes and ui options
(gp-init-themes)

;; end
