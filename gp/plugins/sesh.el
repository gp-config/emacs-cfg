;; sesh - simple session manager
;; using PMDM as a starting point
;; https://inigo.katxi.org/blog/2015/08/09/pmdm_a_poor_s_man_desktop_mode_replacement_for_emacs.html

;; session desktop saving location (session management)
(defconst sesh-root-dir (concat user-emacs-directory "/sessions/"))
;; make dir if doesnt exist
(unless (file-exists-p sesh-root-dir) (make-directory sesh-root-dir))

;; variables
(defvar sesh-file-name (expand-file-name (concat sesh-root-dir ".sesh-files") user-emacs-directory)
  "Location of file to write in opened files.")

;;; INTERNAL FUNCTIONS
(defun sesh--read-files-list ()
  ;; only if sesh-files file exists
  (when (file-exists-p sesh-file-name)
    (with-temp-buffer
      ;; bring contents of file into the temp buffer
      (insert-file-contents sesh-file-name)
      ;; delete commented lines
      (delete-matching-lines "^;; ")
      ;; read contents of buffer to convert it from text to valid lisp
      (read (buffer-substring-no-properties (point-min) (point-max))))))

;;; PUBLIC INTERFACE
(defun sesh-write-opened-files ()
  "write a list of currently opened files to the file defined in `sesh-file-name'"
  (interactive)
  (let ((files (delq nil (mapcar 'buffer-file-name (buffer-list)))))
    (write-region (format ";; sesh file - do not edit manually\n%s"
                          (prin1-to-string files))
                  nil
                  sesh-file-name)))

(defun sesh-load-files ()
  "load the files found in the sesh file"
  (interactive)
  (let ((opened-files (delq nil (mapcar 'buffer-file-name (buffer-list))))
        (files (sesh--read-files-list)))
    (dolist (file files)
      (unless (member file opened-files)
        (find-file file)))))

(provide 'sesh)
