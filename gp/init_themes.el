;; set up themes dir
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "gp/themes/"))

(defvar-local hidden-mode-line-mode nil)
(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global t
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
    (progn
      ;; set both -default and not, for current buffer AND global
        (setq-default hide-mode-line mode-line-format mode-line-format nil)
        (setq hide-mode-line mode-line-format mode-line-format nil))
    ;; else
    (progn
      (setq-default mode-line-format hide-mode-line hide-mode-line nil)
      (setq mode-line-format hide-mode-line hide-mode-line nil)))
  (force-mode-line-update)
  (redraw-display))

(defun gp-set-mode-line () (interactive)
    (column-number-mode)
    (setq
        evil-normal-state-tag "  "
        evil-insert-state-tag " I"
        evil-visual-state-tag " V")

    (setq mode-line-position '((line-number-mode ("%l" (column-number-mode ":%c")))))
    (setq evil-mode-line-format '(before . mode-line-front-space))



    (setq-default mode-line-format '("%e"
        mode-line-front-space
        evil-mode-line-tag
        " "
        mode-line-modified
        "  "
        mode-line-position
        "  "
        mode-line-buffer-identification
        mode-line-end-spaces))
    (setq gp-mode-line-enabled t))


;; default font
;; may be overridden by machine-specific functions in init_helpers
(defun gp-set-font () (interactive)
  (set-face-attribute 'default nil :font "Monospace-10" :weight 'Regular))

(defun gp-set-faces () (interactive)
  ;; remove 1px border around mode line
  (custom-set-faces '(
    mode-line ((t (:box nil :overline nil :underline nil :weight bold)))))

  ;; change color of window split
  (set-face-foreground 'vertical-border "#363636")

  ;; change line number color
  ; (set-face-foreground 'linum "#575757"))
  )


;; called at emacs-startup-hook
(defun gp-init-themes ()
  ;; disable bits of the interface
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)

  ;; load preferred theme
  ;; (load-theme 'nimbostratus-purp t)
  ;; (load-theme 'nimbostratus-lime t)
  (load-theme 'brown t)

  ;; set colors of borders etc
  (gp-set-faces)

  ;; set font face
  ;; either calls the function defined in this file, or
  ;; a function defined by a machine-specific configuration function
  ;; called by init_helpers/gp-determine-machine
  (gp-set-font))

(provide 'init_themes)
