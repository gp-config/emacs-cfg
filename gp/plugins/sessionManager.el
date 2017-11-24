;; session desktop saving location (session management)
(defconst dir-session-save (concat user-emacs-directory "/sessions/"))

;; make dir if doesnt exist
(unless (file-exists-p dir-session-save) (make-directory dir-session-save))

;; variable for history of sessions
(defvar session-name-history nil)

(defun gp-session-save (&optional name) (interactive)
       (unless name
         (setq name (gp-session-prompt-for-name "Save session as: ")))
       (make-directory (concat dir-session-save name) t)
       (desktop-save (concat dir-session-save name) t))

(defun gp-session-load (&optional name) (interactive)
       (unless name
         (setq name (gp-session-prompt-for-name "Load session: ")))
       (desktop-read (concat dir-session-save name)))

(defun gp-session-prompt-for-name (prompt)
  (completing-read prompt (and (file-exists-p dir-session-save)
                              (directory-files dir-session-save))
  nil nil nil session-name-history))

(provide 'sessionManager)
