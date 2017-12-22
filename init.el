;; initial load path setup

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path (concat user-emacs-directory "gp/plugins"))
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "gp/themes/"))

(defun gp-byte-compile-on-save ()
  "byte compile an .el file at save time, if
  it's in the user emacs directory"
  (when (and(string= (file-name-directory (buffer-file-name)) (expand-file-name user-emacs-directory))
            (string= (file-name-extension (buffer-file-name)) "el"))
    (byte-compile-file (buffer-file-name) nil)))

;; when we open an elisp file, add byte compile function to save hook
(add-hook 'emacs-lisp-mode-hook '(lambda ()
                                   (add-hook 'after-save-hook #'gp-byte-compile-on-save)))

(defun gp-tangle-section-cancelled ()
  "checks if previous section header was OFF"
  (save-excursion
    (if (re-search-backward "^\\*+\\s-+\\(.*?\\)?\\s-*$" nil t)
        (progn
          ;; message "FOUND '%s'" (match-string 1))
          (string-prefix-p "OFF" (match-string 1)))
      nil)))

(defun gp-tangle-config-org (orgfile elfile)
  "this function will write all source blocks from =config.org= into
=config.el= that are ...
- not marked as :tangle no
- have a source code of =emacs-lisp=
- do not have a todo marker OFF"
  (let* (;; list where we cobble together body parts
         (body-list ())
         ;; disable special file headers when loading .org files
         (file-name-handler-alist nil)
         ;; monster regexp to extract pieces out of an org file
         (org-babel-src-block-regexp (concat
                                      ;; (1) indentation                 (2) lang
                                      "^\\([ \t]*\\)#\\+begin_src[ \t]+\\([^ \f\t\n\r\v]+\\)[ \t]*"
                                      ;; (3) switches
                                      "\\([^\":\n]*\"[^\"\n*]*\"[^\":\n]*\\|[^\":\n]*\\)"
                                      ;; (4) header arguments
                                      "\\([^\n]*\\)\n"
                                      ;; (5) body
                                      "\\([^\000]*?\n\\)??[ \t]*#\\+end_src")))
    (with-temp-buffer
      (insert-file-contents orgfile)
      (goto-char (point-min))
      (while (re-search-forward org-babel-src-block-regexp nil t)
        (let ((lang (match-string 2))
              (args (match-string 4))
              (body (match-string 5))
              (canc (gp-tangle-section-cancelled)))
          (when (and (string= lang "emacs-lisp")
                     (not (string-match-p ":tangle\\s-+no" args))
                     (not canc))
            (setq body (replace-regexp-in-string "\\(?:^\s+;.+\\)" "" body))
            (setq body (replace-regexp-in-string "\\(?:^\s*\n\\)" "" body))
            (add-to-list 'body-list body)))))
            ;; (add-to-list 'body-list (replace-regexp-in-string "\\(?:^\s+;.+\\|^\s*$\\)" "" body))))))










    (with-temp-file elfile
      (insert ";; *- lexical-binding: t; -*-\n")
      (insert (format ";; Don't edit this file, edit %s instead ...\n\n" orgfile))
      ;; (insert (apply 'concat (reverse body-list)))
      (apply 'insert (reverse body-list)))))

(defun gp-load-file (fname)
  "This loads an elisp configuration file. If an .org file exists,
it will be first untangled. If an byte-compiled file does NOT exist,
it will be created. After this, the normal loading logic happens."
  (let* (;; disable garbage collection while we do heavy string work
         (gc-cons-threshold most-positive-fixnum)
         ;; fname with various extensions
         (sansfile (expand-file-name (file-name-sans-extension fname) user-emacs-directory))
         (orgfile (concat sansfile ".org"))
         (elfile  (concat sansfile ".el"))
         (elcfile (concat sansfile ".elc")))
    (when (file-exists-p orgfile)
      ;; when el file does not exist or org file is newer than el file
      (when (or (not (file-exists-p elfile))
                (file-newer-than-file-p orgfile elfile))
        ;; tangle the org file
        (gp-tangle-config-org orgfile elfile)))

    ;; when a compiled file doesnt exist or the el file is newer than compiled version
    (when (or (not (file-exists-p elcfile))
              (file-newer-than-file-p elfile elcfile))
      ;; byte compile the el file
      (byte-compile-file elfile))
    ;; load the compiled version
    (load elfile nil 'nomessage)))

(gp-load-file "config")
