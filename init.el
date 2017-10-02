;; EMACS INIT

;; open backtrace buffer when something goes wrong
; (set 'debug-on-error t)

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
(setq auto-save-list-file-name (concat dir-file-backups "auto-save-list"))
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

;; expose gp/ init files
(add-to-list 'load-path (concat user-emacs-directory "gp"))

;; set up packages
(require 'init_packages)

;; set up themes and ui options once we're done starting up
(require 'init_themes)
(add-hook 'emacs-startup-hook 'gp_init_themes)

;; end

