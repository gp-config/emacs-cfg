;; HELPER FUNCTIONS

(defun load-directory (directory)
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

(defun indent-infer-spaces-or-tabs () (interactive)
  "Infer whether to use tabs or spaces based on the count of
each character type present in the current file"
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))

(defun indent-use-tabs () (interactive)
    "Use tabs for indentation"
    (setq indent-tabs-mode t)
    (setq-default indent-tabs-mode t))

(defun indent-use-spaces () (interactive)
    "Use spaces for indentation"
    (setq indent-tabs-mode nil)
    (setq-default indent-tabs-mode nil))

(provide 'init_helpers)
