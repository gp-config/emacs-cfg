;; helper functions, etc

;; TODO: no longer used, remove? keep in a gist?
(defun gp-load-directory (directory)
  "Recursively open all `.el' files in DIRECTORY in their own buffer
Ignores directories 'elpa', 'themes', '.', '..'"
  (dolist (element (directory-files-and-attributes directory nil nil nil))
    (let* ((path (car element))
           (fullpath (concat directory "/" path))
           (isdir (car (cdr element)))
           (ignore-dir (or
                         (string= path "elpa")
                         (string= path "themes")
                         (string= path ".")
                         (string= path ".."))))
      (cond
       ((and (eq isdir t) (not ignore-dir))
        (load-directory fullpath))
       ((and (eq isdir nil) (string= (substring path -3) ".el"))
        (find-file fullpath))))))

(defun gp-session-load-config () (interactive)
       (gp-session-load "config"))

(defun gp-indent-infer-spaces-or-tabs () (interactive)
  "Infer whether to use tabs or spaces based on the count of
each character type present in the current file"
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))

(defun gp-indent-use-tabs () (interactive)
    "Use tabs for indentation"
    (setq indent-tabs-mode t)
    (setq-default indent-tabs-mode t))

(defun gp-indent-use-spaces () (interactive)
    "Use spaces for indentation"
    (setq indent-tabs-mode nil)
    (setq-default indent-tabs-mode nil))

;; ====================== machine-specific configuration ====================== ;;
;; figure out which machine we're on and call the appropriate setup function
;; if we don't recognize the name, call unrecognized to set up defaults for
;; otherwise machine-dependant settings
(defun gp-determine-machine ()
  (cond
   ;; macbook pro
   ((string-equal system-name "Geordies-MacBook-Pro.local") (gp-setup-machine-macbook))

   ;; work laptop
   ((string-equal system-name "gp-toshiba") (gp-setup-machine-toshiba))

   ;; default case - unrecognized
   (t (gp-setup-machine-unrecognized))
   ))


;; ///////////// machine config: unrecognized
;; when we don't find a machine name we recognize,
;; use some defaults.
(defun gp-setup-machine-unrecognized ()
  (defun gp-launch-terminal () (interactive)
         (term "/bin/zsh"))
  )

;; ///////////// machine config: macbook pro
;; includes macOS specific settings
(defun gp-setup-machine-macbook ()
(when (eq system-type 'darwin)
  ;; keep menu bar enabled on mac as it's not annoying
  (menu-bar-mode -1)
    ;; fix colors in powerline separators
    ;; (macOS SRGB issue with certain versions of emacs)
    ;; two fixes here:
    ;;
    ;; disable srgb color space
    ; (setq ns-use-srgb-colorspace nil)
    ;; or
    ;; use built-in powerline patch (recommended):
    ; https://github.com/milkypostman/powerline/issues/54#issuecomment-310867163
      (setq powerline-image-apple-rgb t))

  ;; override font function in themes
  (defun gp-set-font () (interactive)
    (set-face-attribute 'default nil :font "Monaco-16" :weight 'Regular))

  ;; create function to launch terminal (default in unrecognized-machine)
  (defun gp-launch-terminal () (interactive)
         (term "/bin/zsh")))

;; ///////////// machine config: work laptop
(defun gp-setup-machine-toshiba ()
  ;; override font function in themes
  (defun gp-set-font () (interactive)
    (set-face-attribute 'default nil :font "Droid Sans Mono Dotted for Powerline-11" :weight 'Regular))

  (defun gp-launch-terminal () (interactive)
         (term "/usr/bin/zsh")))


(provide 'init_helpers)
