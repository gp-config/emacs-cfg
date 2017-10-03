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

(provide 'init_helpers)
