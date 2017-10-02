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

(use-package evil
  :ensure t
  :init (evil-mode 1)
  :config (define-key evil-normal-state-map "," nil))

(use-package general
  :ensure t
  :config (progn
            ; (general-evil-setup t) ; creates vim-like key binding functions (eg imap)
            ;; familiar shortcuts to comment things out
            (global-set-key (kbd "C-\/") 'evil-commentary-line)
            (global-set-key (kbd "C-S-\/") 'evil-commentary)
            ;; some movement keys
            (global-set-key (kbd "C-h") 'evil-window-left)
            (global-set-key (kbd "C-j") 'evil-window-down)
            (global-set-key (kbd "C-k") 'evil-window-up)
            (global-set-key (kbd "C-l") 'evil-window-right)
            ;; general keys
            (general-define-key
            :states '(normal motion insert emacs)
            :prefix ","
            ;; HELP
            "hdv" 'counsel-describe-variable
            "hdf" 'counsel-describe-function
            "hdk" 'describe-key
            
            ;; FILES
            "ff" 'counsel-find-file
            
            ;; SPLITS (with prefix)
            "sv" 'evil-window-vsplit
            "sh" 'evil-window-split
            ;; SPLITS (shortcuts)
            "v" 'evil-window-vsplit
            
            ; BUFFERS (with prefix)
            "bd" 'kill-buffer
            "bb" 'switch-to-buffer
            "bn" 'next-buffer
            "bp" 'previous-buffer)
            ; BUFFERS (shortcuts)
            "c" 'kill-this-buffer
            "q" 'next-buffer
            "z" 'previous-buffer
            ))

(use-package evil-escape
  :ensure t
  :defer
  :init (evil-escape-mode)
  :config (setq-default evil-escape-key-sequence "kj"))

(use-package powerline
  :ensure t
  :init (progn
          (powerline-default-theme)
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
          ))

(use-package airline-themes
  :ensure t
  :config (progn
	    (load-theme 'airline-wombat t)))

(use-package evil-commentary
  :ensure t
  :defer t
  :init (evil-commentary-mode))

(use-package counsel
  :ensure t
  :defer t)

(use-package ivy
  :ensure t
  :defer t
  :init (progn
	  (ivy-mode 1)
          (setq ivy-use-virtual-buffers t)
          (setq enable-recursive-minibuffers t)))

(use-package which-key
  :ensure t
  :defer t
  :init (progn
	  (which-key-mode)
          (which-key-add-key-based-replacements ",f" "Files")
          (which-key-add-key-based-replacements ",h" "Help")
          (which-key-add-key-based-replacements ",hd" "Help - Describe")))

(use-package hl-todo
  :ensure t
  :defer t
  :init (global-hl-todo-mode))

(provide 'init_packages)
