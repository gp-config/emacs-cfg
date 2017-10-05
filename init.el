;; EMACS INIT

;; open backtrace buffer when something goes wrong
(set 'debug-on-error t)

;; dont load outdated byte code
(setq load-prefer-newer t)

;; this line must exist; do not remove
(package-initialize)
;; configure auto file backups
;; set a variable for convenience
(defvar dir-file-backups (concat user-emacs-directory "file_backups"))

;; create directory if it doesnt exist
(unless (file-exists-p dir-file-backups) (make-directory dir-file-backups))
;; set configuration
(setq auto-save-list-file-name (concat dir-file-backups "/auto-save-list"))
(setq
 backup-directory-alist `(("." . ,dir-file-backups))
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

;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; match braces, parens, quotes etc
(electric-pair-mode)
; and highlight them
(show-paren-mode)

; highlight current line
(hl-line-mode)

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
(global-linum-mode)
;; add a bit of spacing around line numbers
(setq linum-format " %d ")

;; enable line highlight
(global-hl-line-mode)

;; disable fringes by default (use set-fringe-style command to change it within a session)
(set-fringe-style 0)

;; disable cursor blinking by default
(blink-cursor-mode)

;; expose gp/ init files
(add-to-list 'load-path (concat user-emacs-directory "gp"))

;; load helper functions
;; do this before loading other init files, as they might depend on helper functions
(require 'init_helpers)

;; set up packages
(require 'init_packages)

;; expose gp/plugins files
(add-to-list 'load-path (concat user-emacs-directory "gp/plugins"))
(require 'session)


;; set up themes and ui options once we're done starting up
(require 'init_themes)
(add-hook 'emacs-startup-hook 'gp_init_themes)

;; end
