;;; oauth2-auto-test.el --- Tests for oauth2-auto -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Adrià Garriga-Alonso, Robert Irelan

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'oauth2-auto)

(defmacro oauth2-auto-test--with-temp-directory (directory &rest body)
  "Bind DIRECTORY to a temporary directory while evaluating BODY."
  (declare (indent 1) (debug t))
  `(let ((,directory (make-temp-file "oauth2-auto-test-" t)))
     (unwind-protect
         (progn ,@body)
       (delete-directory ,directory t))))

(defun oauth2-auto-test--insert-secret-entries-in-store (store)
  "Insert the plstore secret marker into the buffer for STORE."
  (with-current-buffer (plstore--get-buffer store)
    (insert ";;; secret entries\n")))

(ert-deftest oauth2-auto-same-file-p-recognizes-missing-file ()
  (oauth2-auto-test--with-temp-directory directory
    (let ((file (expand-file-name "oauth2-auto.plist" directory)))
      (should-not (file-exists-p file))
      (should (oauth2-auto--same-file-p file file)))))

(ert-deftest oauth2-auto-same-file-p-recognizes-missing-file-through-symlink ()
  (oauth2-auto-test--with-temp-directory directory
    (let ((real-directory (expand-file-name "real" directory))
          (link-directory (expand-file-name "link" directory)))
      (make-directory real-directory)
      (condition-case error-data
          (make-symbolic-link real-directory link-directory)
        (file-error
         (ert-skip (format "Cannot create symbolic link: %S" error-data))))
      (let ((real-file (expand-file-name "oauth2-auto.plist" real-directory))
            (link-file (expand-file-name "oauth2-auto.plist" link-directory)))
        (should-not (file-exists-p real-file))
        (should (oauth2-auto--same-file-p real-file link-file))))))

(ert-deftest oauth2-auto-same-file-p-recognizes-hard-links ()
  (oauth2-auto-test--with-temp-directory directory
    (let ((original (expand-file-name "original.plist" directory))
          (alias (expand-file-name "alias.plist" directory)))
      (with-temp-file original
        (insert "plstore"))
      (condition-case error-data
          (add-name-to-file original alias)
        (file-error
         (ert-skip (format "Cannot create hard link: %S" error-data))))
      (should (oauth2-auto--same-file-p original alias)))))

(ert-deftest oauth2-auto-same-file-p-rejects-different-missing-files ()
  (oauth2-auto-test--with-temp-directory directory
    (should-not
     (oauth2-auto--same-file-p
      (expand-file-name "first.plist" directory)
      (expand-file-name "second.plist" directory)))))

(ert-deftest oauth2-auto-plstore-write-allows-new-plstore ()
  (oauth2-auto-test--with-temp-directory directory
    (let ((oauth2-auto-plstore
           (expand-file-name "oauth2-auto.plist" directory))
          (plist '(:access-token "secret")))
      (cl-letf (((symbol-function 'plstore-put) #'ignore)
                ((symbol-function 'plstore-save)
                 #'oauth2-auto-test--insert-secret-entries-in-store))
        (should (equal plist
                       (oauth2-auto--plstore-write
                        "user@example.com" 'google plist))))
      (should-not (file-exists-p oauth2-auto-plstore)))))

(ert-deftest oauth2-auto-plstore-write-rejects-wrong-buffer ()
  (oauth2-auto-test--with-temp-directory directory
    (let ((oauth2-auto-plstore
           (expand-file-name "oauth2-auto.plist" directory))
          (wrong-file (expand-file-name "calendar.org" directory))
          (wrong-buffer (generate-new-buffer " *oauth2-auto-wrong-buffer*")))
      (unwind-protect
          (progn
            (with-current-buffer wrong-buffer
              (setq buffer-file-name wrong-file))
            (cl-letf (((symbol-function 'plstore-put) #'ignore)
                      ((symbol-function 'plstore-save)
                       (lambda (_store)
                         (with-current-buffer wrong-buffer
                           (insert ";;; secret entries\n")))))
              (let ((error-data
                     (should-error
                      (oauth2-auto--plstore-write
                       "user@example.com" 'google
                       '(:access-token "secret")))))
                (should
                 (string-match-p
                  "Attempted to write.*keys"
                  (error-message-string error-data))))))
        (when (buffer-live-p wrong-buffer)
          (kill-buffer wrong-buffer))))))

(provide 'oauth2-auto-test)

;;; oauth2-auto-test.el ends here
