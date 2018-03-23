;;; gmacs-theme.el - an Emacs color theme by Geordie Powers // GeordieP @ github
;; Package-Requires: ((autothemer "0.2"))

;;; Code:
(eval-when-compile
  (require 'cl-lib))

(require 'autothemer)

;; assumed based on gruvbox
(unless (>= emacs-major-version 24)
  (error "Requires Emacs 24 or later"))
;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(autothemer-deftheme
 gmacs
 "by gp"

 ;; Color Palette:
 ((((class color) (min-colors #xFFFFFF))
   ((class color) (min-colors #xFF)))

  ;; purple
  (gmacs-accent "#6E6887" nil)
  (gmacs-accent-dark "#3A238B" nil)
  (gmacs-accent-light "#A48FFF" nil)
  (gmacs-accent-alt "#AFD870" nil)
  (gmacs-accent-alt-dark "#788768" nil)
  (gmacs-accent-alt-light "#D3FF8F" nil)

  ;; bright green
  ;; (gmacs-accent "#AFD870" nil)
  ;; (gmacs-accent-dark "#788768" nil)
  ;; (gmacs-accent-light "#D3FF8F" nil)
  ;; (gmacs-accent-alt "#6E6887" nil)
  ;; (gmacs-accent-alt-light "#A48FFF" nil)
  ;; (gmacs-accent-alt-dark "#3A238B" nil)

  ;; green
  ;; (gmacs-accent "#7da050" nil)
  ;; (gmacs-accent-dark "#788768" nil)
  ;; (gmacs-accent-light "#D3FF8F" nil)
  ;; (gmacs-accent-alt "#6E6887" nil)
  ;; (gmacs-accent-alt-light "#A48FFF" nil)
  ;; (gmacs-accent-alt-dark "#3A238B" nil)

  ;; blue
  ;; (gmacs-accent "#6f859f" nil)
  ;; (gmacs-accent-dark "#687888" nil)
  ;; (gmacs-accent-light "#58d6ff" nil)
  ;; (gmacs-accent-alt "#a0f8fF" nil)
  ;; (gmacs-accent-alt-dark "#687788" nil)
  ;; (gmacs-accent-alt-light "#8FDFF3" nil)

  ;; gold
  ;; (gmacs-accent "#9C8C5F" nil)
  ;; (gmacs-accent-dark "#806E1B" nil)
  ;; (gmacs-accent-light "#C5B567" nil)
  ;; (gmacs-accent-alt "#717e8e" nil)
  ;; (gmacs-accent-alt-alt-dark "#434c56" nil)
  ;; (gmacs-accent-alt-alt-light "#8191a7" nil)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (gmacs-bg-primary "#282828" nil)
  (gmacs-bg-primary-dark "#252525")
  (gmacs-bg-primary-light "#373737")
  (gmacs-bg-primary-verydark "#222222" nil)

  (gmacs-med-gray "#444444")

  (gmacs-fg-primary "#777777" nil)
  (gmacs-fg-primary-darker "#646466" nil)
  (gmacs-fg-primary-lighter "#9e9e9e" nil)

  (gmacs-cursor gmacs-accent)
  
  ;;;;;;;;;;;;;;;;;;;;;;;;
  
  (gmacs-light gmacs-fg-primary nil)
  (gmacs-white "#F5F5F5" nil)
  (gmacs-grey "#616161" nil)

  (generic-red "#F06292" nil)
  (generic-green "#81C784" nil)
  (generic-yellow "#FFF176" nil)
  (generic-blue "#7986CB" nil)
  (generic-purple "#BA68C8" nil)
  (generic-cyan "#4DB6AC" nil)
  (generic-orange "#FFB74D" nil)
  (generic-brown "#957265" nil)

  (generic-red-darker "#C2185B" nil)
  (generic-green-darker "#388E3C" nil)
  (generic-yellow-darker "#FBC02D" nil)
  (generic-blue-darker "#303F9F" nil)
  (generic-purple-darker "#7B1FA2" nil)
  (generic-cyan-darker "#0097A7" nil)
  (generic-orange-darker "#F57C00" nil)
  (generic-brown-darker "#5D4037" nil))

 ;; Specifications:
 ((default (:background gmacs-bg-primary :foreground gmacs-fg-primary))
  (cursor (:background gmacs-cursor))

  (mode-line (:box nil :foreground gmacs-accent-light :background gmacs-bg-primary-verydark))
  (mode-line-inactive (:box nil :foreground gmacs-med-gray :background gmacs-bg-primary-dark))
  (mode-line-buffer-id (:foreground gmacs-fg-primary))

  (fringe (:background gmacs-bg-primary))
  (hl-line (:background gmacs-bg-primary-verydark))
  (region (:background gmacs-bg-primary-light :foreground gmacs-fg-primary-lighter))
  (secondary-selection (:background gmacs-bg-primary-verydark :foreground gmacs-accent))
  (minibuffer-prompt (:background gmacs-bg-primary :foreground gmacs-accent :bold t))
  (vertical-border (:foreground gmacs-bg-primary-verydark))
  (window-divider (:foreground gmacs-bg-primary-verydark))
  (link (:foreground generic-blue :underline t))
  (shadow (:foreground gmacs-grey))

  (font-lock-builtin-face (:foreground gmacs-fg-primary))
  (font-lock-constant-face (:foreground gmacs-fg-primary :bold t))
  (font-lock-string-face (:foreground gmacs-accent))
  (font-lock-comment-face (:foreground gmacs-med-gray))
  (font-lock-doc-face (:foreground gmacs-med-gray))
  (font-lock-function-name-face (:foreground gmacs-fg-primary-lighter :italic t))
  (font-lock-variable-name-face (:foreground gmacs-accent :italic t))
  (font-lock-keyword-face (:foreground gmacs-fg-primary-lighter))
  (font-lock-type-face (:foreground gmacs-accent :italic t))
  (font-lock-warning-face (:foreground generic-red-darker :bold t))
  (show-paren-match (:foreground gmacs-accent-dark :background gmacs-accent-light))

  (hl-todo (:foreground gmacs-accent-light :bold t))

  ;; Basic Faces
  (error (:foreground generic-red :bold t))
  (success (:foreground generic-green :bold t))
  (warning (:foreground gmacs-accent-alt :bold t))
  (trailing-whitespace (:background gmacs-light))
  (escape-glyph (:foreground generic-cyan))
  (header-line (:background gmacs-bg-primary :foreground gmacs-light :box nil :inherit nil))
  (highlight (:background gmacs-bg-primary-dark :foreground gmacs-accent-light))
  (homoglyph (:foreground generic-yellow))
  (match (:background gmacs-med-gray))

  ;; isearch
  (isearch (:foreground gmacs-accent-dark :background gmacs-accent-light))
  (lazy-highlight (:foreground gmacs-accent-light :underline t))
  (isearch-lazy-highlight (:foreground gmacs-accent-light :underline t))

  ;; evil ex
  (evil-ex-commands (:foreground gmacs-light))
  (evil-ex-info (:foreground generic-red))
  (evil-ex-lazy-highlight (:background gmacs-med-gray))
  (evil-ex-search (:background gmacs-med-gray))
  (evil-ex-substitute-matches (:foreground gmacs-accent-light :underline t))
  (evil-ex-substitute-replacement (:foreground gmacs-accent-alt))

  ;; line numbers
  (line-number (:foreground gmacs-med-gray))
  (line-number-current-line (:foreground gmacs-light :background gmacs-bg-primary))
  (linum (:foreground gmacs-med-gray))
  (linum-highlight-face (:foreground gmacs-light :background gmacs-bg-primary))
  (linum-relative-current-face (:foreground gmacs-light :background gmacs-bg-primary))

  ;; highlight indentation
  (highlight-indentation-current-column-face (:background gmacs-grey))
  (highlight-indentation-face (:background gmacs-med-gray))

  ;; smartparens
  (sp-pair-overlay-face (:background gmacs-grey))
  (sp-show-pair-match-face (:background gmacs-med-gray))
  (sp-show-pair-mismatch-face (:background gmacs-light))

  ;; diff
  (diff-changed (:background nil :foreground gmacs-light))
  (diff-added (:background nil :foreground generic-green))
  (diff-removed (:background nil :foreground generic-red))
  (diff-indicator-changed (:inherit 'diff-changed))
  (diff-indicator-added (:inherit 'diff-added))
  (diff-indicator-removed (:inherit 'diff-removed))

  ;; ediff
  (ediff-current-diff-A (:background generic-orange :foreground gmacs-white))
  (ediff-current-diff-Ancestor (:background generic-red :foreground gmacs-white))
  (ediff-current-diff-B (:background generic-green :foreground gmacs-white))
  (ediff-current-diff-C (:background generic-yellow :foreground gmacs-white))
  (ediff-even-diff-A (:background generic-orange :foreground gmacs-white))
  (ediff-even-diff-Ancestor (:background generic-red :foreground gmacs-white))
  (ediff-even-diff-B (:background generic-green :foreground gmacs-white))
  (ediff-even-diff-C (:background generic-yellow :foreground gmacs-white))
  (ediff-fine-diff-A (:background generic-orange :foreground gmacs-white))
  (ediff-fine-diff-Ancestor (:background generic-red :foreground gmacs-white))
  (ediff-fine-diff-B (:background generic-green :foreground gmacs-white))
  (ediff-fine-diff-C (:background generic-yellow :foreground gmacs-white))
  (ediff-odd-diff-A (:background generic-orange :foreground gmacs-white))
  (ediff-odd-diff-Ancestor (:background generic-red :foreground gmacs-white))
  (ediff-odd-diff-B (:background generic-green :foreground gmacs-white))
  (ediff-odd-diff-C (:background generic-yellow :foreground gmacs-white))

  ;; company-mode
  (company-scrollbar-bg (:background gmacs-bg-primary-light))
  (company-scrollbar-fg (:background gmacs-fg-primary-lighter))
  (company-tooltip (:background gmacs-bg-primary-verydark :foreground gmacs-fg-primary-darker))
  (company-tooltip-annotation (:background gmacs-bg-primary-verydark :foreground generic-purple :italic t))
  (company-tooltip-annotation-selection (:background gmacs-bg-primary :foreground generic-purple :italic t))
  (company-tooltip-selection (:foreground gmacs-accent :background gmacs-bg-primary-light))
  (company-tooltip-common (:foreground gmacs-fg-primary-lighter))
  (company-tooltip-common-selection (:foreground gmacs-accent-light :bold t))
  (company-preview (:background gmacs-bg-primary-light))
  (company-preview-search (:background gmacs-bg-primary-verydark))
  (company-preview-common (:foreground gmacs-accent-light))

  ;; ivy
  (ivy-current-match (:foreground gmacs-accent-light :bold t :background gmacs-bg-primary-verydark))
  (ivy-minibuffer-match-face-1 (:foreground generic-cyan))
  (ivy-minibuffer-match-face-2 (:foreground gmacs-accent-alt))
  (ivy-minibuffer-match-face-3 (:foreground generic-cyan))
  (ivy-minibuffer-match-face-4 (:foreground gmacs-accent-alt))

  ;; powerline
  (powerline-active0 (:foreground gmacs-light :background gmacs-bg-primary))
  (powerline-active1 (:foreground gmacs-light :background gmacs-med-gray))
  (powerline-active2 (:foreground gmacs-light :background gmacs-grey))
  (powerline-inactive0 (:foreground gmacs-light :background gmacs-bg-primary))
  (powerline-inactive1 (:foreground gmacs-light :background gmacs-med-gray))
  (powerline-inactive2 (:foreground gmacs-light :background gmacs-grey))
  (spaceline-python-venv (:foreground generic-purple)))

 ;; Evaluated Forms:
 (custom-theme-set-variables 'gmacs
                             `(hl-paren-colors
                               '(,generic-green
                                 ,generic-red
                                 ,generic-red
                                 ,generic-orange))
                             `(ansi-color-names-vector
                               [,gmacs-bg-primary
                                ,generic-red
                                ,generic-green
                                ,generic-yellow
                                ,generic-blue
                                ,generic-purple
                                ,generic-cyan
                                ,gmacs-light])))

(provide-theme 'gmacs)
(provide 'gmacs-theme)
