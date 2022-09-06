;;; test-oauth2-auto.el --- Tests for oauth2-auto -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Adrià Garriga-Alonso
;;
;; Author: Adrià Garriga-Alonso <adria@rdwrs.com>
;; Maintainer: Adrià Garriga-Alonso <adria@rdwrs.com>
;; Created: September 06, 2022
;; Version: 0.0.1
;; Keywords: comm extensions mail news processes
;; Homepage: https://github.com/rhaps0dy/emacs-oauth2-auto
;; Package-Requires: ((emacs "26.1") (aio "1.0") (alert "1.2") (dash "2.19"))
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;;  Tests oauth2-auto
;;
;;; Code:
(require 'oauth2-auto)

(ert-deftest test-oauth2-auto--provider-info ()
  (setq test-oauth2-auto-additional-providers-alist oauth2-auto-additional-providers-alist)
  (unwind-protect
      (progn
        ;; unknown provider
        (should-error (oauth2-auto--provider-info 'surely-an-unknown-provider))

        ;; missing client_secret
        (setq oauth2-auto-additional-providers-alist '((surely-a-novel-provider (client_id . "id"))))
        (should-error (oauth2-auto--provider-info 'surely-a-novel-provider))

        ;; blank client_secret
        (setq oauth2-auto-additional-providers-alist
              '((surely-a-novel-provider (client_id . "id") (client_secret . ""))))
        (should-error (oauth2-auto--provider-info 'surely-a-novel-provider))

        ;; gets correctly
        (let ((provider-info '((client_id . "id") (client_secret . "secret"))))
          (setq oauth2-auto-additional-providers-alist `((surely-a-novel-provider ,@provider-info)))
          (should (eq (oauth2-auto--provider-info 'surely-a-novel-provider) provider-info))))

    ;; restore settings
    (setq oauth2-auto-additional-providers-alist test-oauth2-auto-additional-providers-alist)))


(ert-deftest test-oauth2-auto--provider-scopes ()
  (should-error (oauth2-auto--provider-scopes 'google ()))
  (should-error (oauth2-auto--provider-scopes 'google '(bad)))
  (should-error (oauth2-auto--provider-scopes 'google '(email bad)))
  (should (equal "https://mail.google.com/" (oauth2-auto--provider-scopes 'google '(email))))
  (should (equal "https://www.googleapis.com/auth/calendar.events https://mail.google.com/"
                 (oauth2-auto--provider-scopes 'google '(calendar email)))))

(ert-deftest test-oauth2-auto--make-token-plist ()
  (let ((plist '(:refresh-token "ref-plist"))
        (refresh-token '(refresh_token . "ref-alist"))
        (other-alist '((access_token . "access")
                       (expires_in . 10)))
        (expiry (+ 10 (oauth2-auto--now))))
    (should (equal `(:access-token "access" :refresh-token "ref-plist" :expiration ,expiry)
                   (oauth2-auto--make-token-plist other-alist plist)))

    (should (equal `(:access-token "access" :refresh-token "ref-alist" :expiration ,expiry)
                   (oauth2-auto--make-token-plist (cons refresh-token other-alist) plist)))
    (should-error (oauth2-auto--make-token-plist other-alist nil))
    (should-error (oauth2-auto--make-token-plist nil plist))
    (should-error (oauth2-auto--make-token-plist (list refresh-token) plist))))

(ert-deftest test-oauth2-auto--make-auth-request-alist ()
  (should (equal '((username . "a")
                   (provider . google)
                   (scope . "https://mail.google.com/ https://www.googleapis.com/auth/calendar.events"))
                 (oauth2-auto--make-auth-request-alist "a" 'google '(email calendar)))))

(ert-deftest test-oauth2-auto--compute-id ()
  "Test that IDs are different if auth-requests are a bit different."
  (should-not (equal (oauth2-auto--compute-id (oauth2-auto--make-auth-request-alist "a" 'google '(email)))
                     (oauth2-auto--compute-id (oauth2-auto--make-auth-request-alist "b" 'google '(email)))))

  (should-not (equal (oauth2-auto--compute-id (oauth2-auto--make-auth-request-alist "a" 'google '(email)))
                     (oauth2-auto--compute-id (oauth2-auto--make-auth-request-alist "b" 'microsoft '(email)))))

  (should-not (equal (oauth2-auto--compute-id (oauth2-auto--make-auth-request-alist "a" 'google '(email)))
                     (oauth2-auto--compute-id (oauth2-auto--make-auth-request-alist "b" 'google '(calendar)))))

  (should-not (equal (oauth2-auto--compute-id (oauth2-auto--make-auth-request-alist "a" 'google '(calendar email)))
                     (oauth2-auto--compute-id (oauth2-auto--make-auth-request-alist "b" 'google '(calendar))))))


(ert-deftest test-oauth2-auto--plstore-roundtrip ()
  (setq test-oauth2-auto-plstore oauth2-auto-plstore)
  (setq oauth2-auto-plstore "/tmp/test-oauth2-auto--tmp-for-testing.plist")

  (unwind-protect
      (let ((auth-request (oauth2-auto--make-auth-request-alist "a" 'google '(calendar)))
            (auth-request-2 (oauth2-auto--make-auth-request-alist "a" 'google '(email))))

        (should-not (oauth2-auto--plstore-read auth-request))

        ; Check it does not retrieve whatever
        (oauth2-auto--plstore-write auth-request '(:hi "bye"))
        (should-not (oauth2-auto--plstore-read auth-request-2))


        ; Write something and retrieve it
        (oauth2-auto--plstore-write auth-request-2 '(:bar "foo"))
        (should (equal (oauth2-auto--plstore-read auth-request) '(:hi "bye")))
        (should (equal (oauth2-auto--plstore-read auth-request-2) '(:bar "foo"))))

    (progn
      (delete-file oauth2-auto-plstore)
      ;; restore settings
      (setq oauth2-auto-plstore test-oauth2-auto-plstore))))


(provide 'test-oauth2-auto)
;;; test-oauth2-auto.el ends here
