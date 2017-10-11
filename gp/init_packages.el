;; define package repos
(defconst gnu '("gnu" . "https://elpa.gnu.org/packages/"))
(defconst melpa '("melpa" . "https://melpa.org/packages/"))
(defconst melpa-stable '("melpa-stable" . "https://stable.melpa.org/packages/"))

;; add package repos to archives list
(setq package-archives nil)
(add-to-list 'package-archives melpa-stable t)
(add-to-list 'package-archives melpa t)
(add-to-list 'package-archives gnu t)

(unless (and (file-exists-p "~/.emacs.d/elpa/archives/gnu")
  (file-exists-p "~/.emacs.d/elpa/archives/melpa")
  (file-exists-p "~/.emacs.d/elpa/archives/melpa-stable"))
	(package-refresh-contents))
;; evaluate the package list and install missing packages
(defun packages-install (&rest packages)
  ; (message "running packages-install")
  (mapc (lambda (package)
	  (let ((name (car package))
		(repo (cdr package)))
	    (when (not (package-installed-p name))
	      (let ((package-archives (list repo)))
		(package-initialize)
		(package-install name)))))
	packages)
  (package-initialize)
  (delete-other-windows))

;; install any packages if they're missing
(defun init--install-packages ()
  ; (message "installing packages")
  (packages-install (cons 'use-package melpa)))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

;; ====================== install/load packages ====================== ;;

;; use-package :init - run before package is loaded
;; use-package :config - run after package is loaded

(use-package general
  :ensure t
  :config
        ;; KEY BINDS
        ;; different states get different general-define-key blocks
        ;; eg, we dont want the , leader key to be active in insert mode
        ;; ============= GENERAL KEYS - MISC =============
        (general-define-key
          :states '(normal motion emacs insert)
          "C-h" 'evil-window-left
          "C-j" 'evil-window-down
          "C-k" 'evil-window-up
          "C-l" 'evil-window-right
          "C-u" 'evil-scroll-up
          ;; ctrl+shift+enter to insert line above
          "C-S-<return>" '(lambda () (interactive)
                        (previous-line)
                        (end-of-line)
                        (newline-and-indent))
          "C-<return>" '(lambda () (interactive)
                          (end-of-line)
                          (newline-and-indent)))
        ;; ============= GENERAL KEYS - MISC - NO INSERT MODE =============
        (general-define-key
         :states '(normal motion emacs)
          ;; "C-p" 'switch-to-buffer)
          "C-p" 'counsel-projectile)
        ;; ============= GENERAL KEYS - VIM =============
        ;; COMMA LEADER
        (general-define-key
            :states '(normal motion emacs)
            :prefix ","
            ;; SHORTCUTS (misc keys, not inside a "menu")
            "v" 'evil-window-vsplit
            "c" 'kill-this-buffer
            "q" 'next-buffer
            "z" 'previous-buffer
            "x" 'execute-extended-command
            ;; MENUS - <leader><menu key> enters a "menu"
            ;; b - BUFFERS
            "bd" 'kill-buffer
            "bb" 'switch-to-buffer
            "bn" 'next-buffer
            "bp" 'previous-buffer
            "bl" 'list-buffers
            ;; s - SPLITS
            "sv" 'evil-window-vsplit
            "sh" 'evil-window-split
            ;; f - FILES
            "ff" 'find-file
            "fo" 'find-file
            "fc" 'gp-session-load-config
            "fed" 'gp-session-load-config
            ;; w - WINDOW
            "wd" 'evil-window-delete
            "wc" 'evil-window-delete
            "wv" 'evil-window-vnew
            "wh" 'evil-window-new
            ;; t - UI TOGGLES
            "tn" 'global-linum-mode
            "th" 'hl-line-mode
            "tw" 'toggle-truncate-lines
            "tm" 'hidden-mode-line-mode
            "ts" 'whitespace-mode
            "tis" 'gp-indent-use-spaces
            "tit" 'gp-indent-use-tabs
            "tii" 'gp-indent-infer-spaces-or-tabs
            ;; e - EXECUTE
            "et" 'gp-launch-terminal
            "ec" 'execute-extended-command
            "ee" 'eval-expression
            ;; s - SESSION
            "ss" 'gp-session-save
            "so" 'gp-session-load
            ;; "sa" ;; TODO: toggle session auto-save
            ;; h - HELP
	    ;; h d - HELP > DESCRIBE
            "hdv" 'describe-variable
            "hdf" 'describe-function
            "hdk" 'describe-key
            ))

(use-package evil-escape
  :ensure t
  :defer
  :init (evil-escape-mode)
  :config (setq-default evil-escape-key-sequence "kj"))

(use-package evil
  :ensure t
  :init (evil-mode 1)
  :config (define-key evil-normal-state-map "," nil))

(use-package evil-commentary
  :ensure t
  :defer t
  :init (evil-commentary-mode))

(defun gp-powerline-enable () (interactive)
    (use-package powerline
    :ensure t
    :init
            ;; (my-powerline-theme)
            ; previews of separators: http://spacemacs.org/doc/DOCUMENTATION.html#mode-line
            ;; (setq powerline-default-separator 'alternate)
            ;; (setq powerline-default-separator 'arrow)
            ;; (setq powerline-default-separator 'arrow-fade)
            ;; (setq powerline-default-separator 'bar)
            ;; (setq powerline-default-separator 'box)
            ;; (setq powerline-default-separator 'brace)
            ;; (setq powerline-default-separator 'butt)
            ;; (setq powerline-default-separator 'chamfer)
            ;; (setq powerline-default-separator 'contour)
            ;; (setq powerline-default-separator 'curve)
            ;; (setq powerline-default-separator 'rounded)
            ;; (setq powerline-default-separator 'roundstub)
            (setq powerline-default-separator 'slant)
            ;; (setq powerline-default-separator 'wave)
            ;; (setq powerline-default-separator 'zigzag)
            ;; (setq powerline-default-separator 'nil)
                ;; this package adds a lot to emacs boot time
                ;; commenting it out for now
                (use-package airline-themes
                :ensure t
                :config
                        (powerline-default-theme)
                        (load-theme 'airline-wombat t)
                        (force-mode-line-update)
                        (redraw-display))
                            ;; (load-theme 'airline-papercolor t)))
            ))

(use-package ivy
  :ensure t
  :defer t
  :init
        (ivy-mode 1)
        (setq ivy-use-virtual-buffers t)
        (setq enable-recursive-minibuffers t))

(use-package counsel
  :ensure t
  :defer t)

(use-package projectile
  :ensure t
  :defer t
  :init
  (use-package counsel-projectile
    :ensure t)
  :config
    (projectile-global-mode)
    (counsel-projectile-on))

(use-package which-key
  :ensure t
  :defer t
  :init
        (which-key-mode)
        (which-key-add-key-based-replacements ",b" "Buffers...")
        (which-key-add-key-based-replacements ",s" "Splits...")
        (which-key-add-key-based-replacements ",f" "Files...")
        (which-key-add-key-based-replacements ",fc" "Edit Emacs configuration files")
        (which-key-add-key-based-replacements ",w" "Window...")
        (which-key-add-key-based-replacements ",t" "UI/Visual Toggles...")
        (which-key-add-key-based-replacements ",tn" "Line Numbers (Toggle)")
        (which-key-add-key-based-replacements ",th" "Highlight Current Line (Toggle)")
        (which-key-add-key-based-replacements ",tw" "Word Wrap (Toggle)")
        (which-key-add-key-based-replacements ",e" "Execute...")
        (which-key-add-key-based-replacements ",et" "Terminal (zsh)")
        (which-key-add-key-based-replacements ",ec" "Command")
        (which-key-add-key-based-replacements ",ee" "Evaluate Expression")
        (which-key-add-key-based-replacements ",h" "Help...")
        (which-key-add-key-based-replacements ",hd" "Describe..."))

(use-package hl-todo
  :ensure t
  :defer t
  :init (global-hl-todo-mode))

(use-package highlight-indent-guides
  :ensure t
  :defer t
  :init (setq highlight-indent-guides-method 'character))

;; function to load rainbow delimiters
(defun enable-rainbow-delims ()
    (use-package rainbow-delimiters
    :ensure t
    :init (rainbow-delimiters-mode)))
;; don't load the rainbow delims package until we open a lisp or elisp file
(add-hook 'lisp-mode-hook 'enable-rainbow-delims)
(add-hook 'emacs-lisp-mode-hook 'enable-rainbow-delims)

(provide 'init_packages)
