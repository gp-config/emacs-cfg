;; set up themes dir
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "gp/themes/"))

(defun gp-set-mode-line ()
  (column-number-mode)
  (setq
    evil-normal-state-tag " N"
    evil-insert-state-tag " I"
    evil-visual-state-tag " V")
  (setq mode-line-position '((line-number-mode ("%l" (column-number-mode ":%c")))))
  (setq evil-mode-line-format '(before . mode-line-front-space))

    (setq-default mode-line-format '("%e"
        mode-line-front-space
        evil-mode-line-tag
        "/ "
        mode-line-position
        " / "
        mode-line-buffer-identification
        mode-line-end-spaces)))


;; called at emacs-startup-hook
(defun gp-init-themes ()
  ;; disable bits of the interface
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)

  ;; macOS
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

  ;; set font face
  (set-face-attribute 'default nil :font "Monaco-16")

  ;; remove 1px border around mode line
  (custom-set-faces '(
    mode-line ((t (:box nil :overline nil :underline nil :weight bold)))))

  ;; load theme
  (load-theme 'nimbostratus-purp t))

(provide 'init_themes)
