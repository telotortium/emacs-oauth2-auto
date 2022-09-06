;;; oauth2-auto.el --- Automatically refreshing OAuth 2.0 tokens -*- lexical-binding: t; -*-

;; Copyright (C) 2011-2021 Free Software Foundation, Inc

;; Author: Adrià Garriga-Alonso <adria.garriga@gmail.com>
;; Maintainer: Adrià Garriga-Alonso <adria@rdwrs.com>
;; Version: 0.0.1
;; Keywords: comm extensions mail news processes
;; Homepage: https://github.com/rhaps0dy/emacs-oauth2-auto
;; Package-Requires: ((emacs "26.1") (aio "1.0") (alert "1.2") (dash "2.19"))

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

;; State machine to fetch OAuth 2.0 tokens for various email accounts.
;;
;; Based on ~mutt_oauth2.py~, which is Copyright (C) 2020 Alexander Perlis,
;; licensed under the GPLv3 or later.
;;
;; The entry points are `oauth2-auto-plist' and the convenience function
;; `oauth2-auto-access-token'.

;;; Code:
(require 'plstore)
(require 'aio)     ; promises
(eval-when-compile (require 'dash))    ; `--map'
(require 'alert)   ; `alert' to give the user a heads up to go to their browser and log in
(require 'url-auth)

(defgroup oauth2-auto nil
  "Automatically refreshing OAuth 2.0 tokens."
  :group 'comm)


;; Endpoints and client secret/id used for various OAuth2 providers.

(defcustom oauth2-auto-microsoft-default-tenant "common"
  "Default tenant ID for Microsoft OAuth2.
If using an institutional email, change to the organization ID."
  :group 'oauth2-auto
  :type 'string)

(defcustom oauth2-auto-microsoft-client-id ""
  "Default client ID for Microsoft OAuth2."
  :group 'oauth2-auto
  :type 'string)

(defcustom oauth2-auto-microsoft-client-secret ""
  "Default client secret for Microsoft OAuth2."
  :group 'oauth2-auto
  :type 'string)

(defcustom oauth2-auto-google-client-id "739925368137-g1723jt0hrsck33tqnrhhdtr7bfri6gj.apps.googleusercontent.com"
  "Default client ID for Google OAuth2."
  :group 'oauth2-auto
  :type 'string)

(defcustom oauth2-auto-google-client-secret "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"
  "Default client secret for Google OAuth2."
  :group 'oauth2-auto
  :type 'string)

(defcustom oauth2-auto-additional-providers-alist '()
  "Additional OAuth2 providers following the format of `oauth2-auto--default-providers'."
  :group 'oauth2-auto
  :type 'alist)

(defun oauth2-auto--default-providers ()
  "Default OAuth2 providers."
  (let ((ms-oauth2-url (concat "https://login.microsoftonline.com/"
                               oauth2-auto-microsoft-default-tenant
                               "/oauth2/v2.0/")))
    `((google
       (authorize_url . "https://accounts.google.com/o/oauth2/auth")
       (token_url . "https://oauth2.googleapis.com/token")
       (available_scopes . ((email . "https://mail.google.com/")
                            (calendar . "https://www.googleapis.com/auth/calendar.events")))
       (client_id . ,oauth2-auto-google-client-id)
       (client_secret . ,oauth2-auto-google-client-secret))
      (microsoft
       (authorize_url . ,(concat ms-oauth2-url "authorize"))
       (token_url . ,(concat ms-oauth2-url "token"))
       (tenant . ,oauth2-auto-microsoft-default-tenant)
       (available_scopes . ((email . "offline_access https://outlook.office.com/IMAP.AccessAsUser.All https://outlook.office.com/POP.AccessAsUser.All https://outlook.office.com/SMTP.Send")))
       (client_id . ,oauth2-auto-microsoft-client-id)
       (client_secret . ,oauth2-auto-microsoft-client-secret)))))


(defun oauth2-auto-providers-alist ()
  "Return all available OAuth2 providers.
This combines the providers specified in `oauth2-auto--default-providers' and
`oauth2-auto-additional-providers-alist'."
  (append oauth2-auto-additional-providers-alist
          (oauth2-auto--default-providers)))

;; Query the provider alist

(defun oauth2-auto-assoc (key alist)
  "Get KEY from ALIST. Error if the result is nil."
  (let ((out (cdr (assoc key alist))))
    (unless out
      (error "Could not find key %s in alist %s" key alist))
    out))


(defun oauth2-auto--provider-info (provider)
  "Get data for PROVIDER from `oauth2-auto-providers-alist'."
  (let ((provider-info (oauth2-auto-assoc provider (oauth2-auto-providers-alist))))
    (dolist (key '(client_id client_secret))
      (when (equal "" (oauth2-auto-assoc key provider-info))
        (error "oauth2-auto: Provider %s was requested but has no `%s' specified" provider key)))
    provider-info))

(defun oauth2-auto--provider-scopes (provider scopes)
  "Return the concatenated SCOPES from PROVIDER.
Signals error if the SCOPES are not present."
  (unless scopes
    (error "SCOPES is nil (empty list)"))
  (let* ((provider-info (oauth2-auto--provider-info provider))
         (available-scopes (cdr (assoc 'available_scopes provider-info)))
         (get-scope (lambda (s) (let ((scope-text (oauth2-auto-assoc s available-scopes)))
                                  (unless scope-text
                                    (error "Provider %s does not have scope %s" provider s))
                                  scope-text))))
    (mapconcat get-scope scopes " ")))


;; Main data structure

(defun oauth2-auto--make-token-plist (response plist)
  "Make the token storage data structure.

This data structure is a plist containing an :access-token, :refresh-token, and
:expiration.  This function takes RESPONSE from ‘oauth2-auto--request’, and data
from PLIST if non-nil.  The return value is intended to be stored in plstore."
  (let ((refresh-token (or (cdr (assoc 'refresh_token response))
                           (plist-get plist :refresh-token))))
    (unless refresh-token
      (error "No refresh token in response %s or plist %s"
             (pp-to-string response) (pp-to-string plist)))
    `(:access-token ,(oauth2-auto-assoc 'access_token response)
      :refresh-token ,refresh-token
      :expiration ,(+ (oauth2-auto--now)
                      (oauth2-auto-assoc 'expires_in response)))))

(defun oauth2-auto--make-auth-request-alist (username provider scopes)
  "Struct representing an authentication request.

Authenticate user USERNAME with PROVIDER, and request use of the
resources in SCOPES."
    `((username . ,username)
      (provider . ,provider)
      (scopes . ,(funcall #'oauth2-auto--provider-scopes provider scopes))))

;; Checking token expiration

(defun oauth2-auto--now ()
  "Current epoch in seconds."
  (seconds-to-time nil 'integer))

(defun oauth2-auto--plist-needs-refreshing (plist)
  "Return non-nil if the authentication-token in PLIST needs refreshing."
  (or (not (plist-get plist :expiration))
      (> (oauth2-auto--now)
         (plist-get plist :expiration))))


;; Cache and plstore read/write

(defcustom oauth2-auto-plstore (concat user-emacs-directory "oauth2-auto.plist")
  "File to store the authenticated accounts to."
  :group 'oauth2-auto
  :type 'file)

(defun oauth2-auto--compute-id (auth-request)
  "Unique ID for an AUTH-REQUEST."
  (url-hexify-string
   (pp-to-string (list (alist-get 'username auth-request)
                       (alist-get 'provider auth-request)
                       (alist-get 'scope auth-request)))))

(defun oauth2-auto--plstore-write (auth-request plist)
  "Save the response PLIST for an AUTH-REQUEST to the plstore."
  (let ((id (oauth2-auto--compute-id auth-request))
        (plstore (plstore-open oauth2-auto-plstore)))
    (unwind-protect
        (prog1 plist
          (plstore-put plstore id nil plist)
          ;; Seems like we occasionally end up with a killed buffer in PLSTORE - reinitialize it in that case.
          (unless (buffer-live-p (plstore--get-buffer plstore))
           (setq plstore (plstore-open oauth2-auto-plstore))
           (plstore-put plstore id nil plist))
          (plstore-save plstore))
      (plstore-close plstore))))

(defun oauth2-auto--plstore-read (auth-request)
  "Read data for AUTH-REQUEST from plstore if it exists, else from the plstore.
Cache data if a miss occurs."
  (let ((id (oauth2-auto--compute-id auth-request)))
    (let ((plstore (plstore-open oauth2-auto-plstore)))
      (unwind-protect
          (cdr (plstore-get plstore id))
        (plstore-close plstore)))))



;; Main entry point

(aio-defun oauth2-auto-plist (username provider scopes)
  "Return a promise for a `oauth2-token' structure for USERNAME and PROVIDER, with access to SCOPES."
  ; Check the plstore for the requested username, provider and scope
  (let* ((auth-request (oauth2-auto--make-auth-request-alist username provider scopes))
        (plist (oauth2-auto--plstore-read auth-request)))
    (if (not (oauth2-auto--plist-needs-refreshing plist))
        ; If expiration time is found and hasn't happened yet
        plist
      ; Otherwise refresh or authenticate the user, and write the result to
      ; plstore.
      (oauth2-auto--plstore-write
       auth-request
       (aio-await
        (oauth2-auto-refresh-or-authenticate auth-request plist))))))

(aio-defun oauth2-auto-force-reauth (username provider scopes)
  "Authenticate USERNAME with PROVIDER again, requesting SCOPES, and save result to `oauth2-auto-plstore'."
  (let ((auth-request (oauth2-auto--make-auth-request-alist username provider scopes)))
    (oauth2-auto--plstore-write
     auth-request
     (aio-await
      (oauth2-auto-authenticate auth-request)))))

;; (defun oauth2-auto-poll-promise (promise &optional seconds)
;;   "Synchronously wait for PROMISE, polling every SECONDS seconds."
;;   (let ((seconds-really (or seconds 3)))
;;    (while (null (aio-result promise))
;;      (sleep-for seconds-really))
;;    (funcall (aio-result promise))))

;;;###autoload
(defun oauth2-auto-plist-sync (username provider scopes)
  "Return a `oauth2-token' structure for USERNAME and PROVIDER, with access to SCOPES."
  (aio-wait-for (oauth2-auto-plist username provider scopes)))

(aio-defun oauth2-auto-access-token (username provider scopes)
  "Return promise for an access-token string used to authenticate USERNAME to PROVIDER, requesting SCOPES."
  (plist-get (aio-await (oauth2-auto-plist username provider scopes))
             :access-token))

;;;###autoload
(defun oauth2-auto-access-token-sync (username provider scopes)
  "Return an access-token string used to authenticate USERNAME to PROVIDER, requesting SCOPES."
  (aio-wait-for (oauth2-auto-access-token username provider scopes)))


;; Making and encoding requests

(defun oauth2-auto--urlify-request (alist)
  "Make ALIST of (symbol . string) into URL-formatted request."
  (mapconcat (lambda (s) (concat (url-hexify-string (symbol-name (car s)))
                                 "=" (url-hexify-string (cdr s))))
             alist "&"))

(defun oauth2-auto--craft-request-alist (provider-info data-keys extra-alist)
  "Make request for PROVIDER-INFO using the info in DATA-KEYS and EXTRA-ALIST."
  (append (--filter (memq (car it) data-keys) provider-info) extra-alist))

(defun oauth2-auto--request-access-parse ()
  "Parse the result of an OAuth request.

Code from `oauth2.el', licensed under GPLv3+.
See https://github.com/emacsmirror/oauth2."
  (goto-char (point-min))
  (when (search-forward-regexp "^$" nil t)
    (json-read)))

(aio-defun oauth2-auto--request (provider url-key data-keys extra-alist)
  "Asynchronously send a POST request to OAuth2 PROVIDER.
PROVIDER uses the url and data specified under URL-KEY and
DATA-KEYS in the provider info (see `oauth2-auto-providers-alist').
Also send data in EXTRA-ALIST."
  (let* (; Craft the request first
         (provider-info (oauth2-auto--provider-info provider))
         (url (cdr (assoc url-key provider-info)))
         (data-alist (oauth2-auto--craft-request-alist
                      provider-info data-keys extra-alist))
         (data (oauth2-auto--urlify-request data-alist)))
    ; Parameters for `url-retrieve' inside `aio-url-retrieve'
    ; Cannot use `let' because they are dynamically bound variables
    (setq url-registered-auth-schemes nil
          url-request-method "POST"
          url-request-data data
          url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
    ; TODO: using `aio-url-retrieve' sometimes results in a hung connection
    (let* ((response-buffer (aio-await (url-retrieve-synchronously url)))
           (response (with-current-buffer response-buffer
                       (prog1 (oauth2-auto--request-access-parse)
                         (kill-buffer (current-buffer))))))
      (cond
       ((assoc 'error response)
        (error "OAuth error.  Request: %s.  Response: %s"
               (pp-to-string data-alist) (pp-to-string response)))
       (t response)))))


;; Barebones HTTP server to receive the tokens

(defun oauth2-auto--httpd-respond (process response)
  "Send response for OAuth2 challenge-response.
PROCESS is the server process created in ‘oauth2-auto--browser-request’.
RESPONSE is the HTTP response body to send."
  (process-send-string
   process (concat "HTTP/1.0 200 OK\n"
                   "Content-Type: text/plain; charset=utf-8\n"
                   (format "Content-Length: %i\n\n" (length response))
                   response
                   "\n\n"))
  (process-send-eof process))

(defmacro oauth2-auto--query-case (&rest cases)
  "Handle HTTP queries based on the keys present in ‘query-alist’.
‘query-alist’ is a free variable, bound by the caller of this macro.  Each
element of CASES has the format ‘(symbols msg body)'.  For each element of
CASES:

- ‘symbols' is a list of at least one symbol, which should be keys in
  ‘query-alist’.
- Extract and bind keys `symbols' from `query-alist'.
- If all of them are present, respond with `msg' and runs `body'.

For example of usage see ‘oauth2-auto--httpd-filter’."
  (declare
   (debug (&rest ((symbolp &rest symbolp) form &rest form))))
  `(cond
    ,@(mapcar (lambda (case)
                (let ((symbols (car case))
                      (msg (cadr case))
                      (body (cddr case)))
                  `((and ,@(--map `(cdr (assoc ',it query-alist)) symbols))
                    (let* (,@(--map `(,it (cdr (assoc ',it query-alist))) symbols)
                           (msg ,msg))
                      ;; ‘ignore’ suppresses byte compiler warnings if the macro
                      ;; caller doesn’t use the variables declared in the ‘let*’
                      ;; above.
                      (ignore msg ,@symbols)
                      (oauth2-auto--httpd-respond process msg)
                      ,@body))))
              cases)))

(defun oauth2-auto--httpd-filter (process input)
  "The HTTP handler for the OAuth2 challenge-response server.
PROCESS is the server process created in ‘oauth2-auto--browser-request’.
INPUT is the raw HTTP request."
  (let ((query-alist
         (with-temp-buffer
           (insert input)
           (goto-char (point-min))
           (re-search-forward
            "^[[:space:]]*GET[[:space:]]+[/?]+\\([[:graph:]]*\\)[[:space:]]+HTTP/[0-9.]+[[:space:]]*$")
           (mapcar
            (lambda (it) (cons (intern (car it)) (cadr it)))
            (url-parse-query-string (match-string 1))))))
    (oauth2-auto--query-case
     ((error error_description)
      (format "Error %s: %s" error error_description)
      (error msg)
      nil)
     ((code state)
      "Authentication token successfully obtained by Emacs! You may close this page now."
      query-alist)
     ((favicon.ico)
      ""
      nil)  ; just return empty list if favicon.ico is requested
     (()
      (format "Could not parse query string %s" (pp-to-string query-alist))
      (error msg)
      nil))))


(aio-defun oauth2-auto--browser-request (provider url-key data-keys extra-alist &optional quiet)
  "Open browser for the OAuth2 PROVIDER.
Browser is opened at url and parameters given by taking URL-KEY and DATA-KEYS
from the data of the PROVIDER, and adding EXTRA-ALIST.  Then we listen to the
redirect response and return it.

If QUIET is non-nil, suppress alerts."
  (let* (; First open listener to some port in localhost
         (server-proc-filter (aio-make-callback))
         (server-proc (make-network-process
                       :name    "oauth2-auto--httpd"
                       :service t
                       :server  t
                       :host    'local
                       :family  'ipv4
                       :filter  (car server-proc-filter)
                       :coding  'binary)))
    (unwind-protect
        (let* ((server-promise (cdr server-proc-filter))
               (server-url (format "http://localhost:%i/"
                                   (cadr (process-contact server-proc))))
               (redirect-uri-elt (cons 'redirect_uri server-url))

               ; Craft a request
               (very-extra-alist (cons redirect-uri-elt extra-alist))
               ;; (very-extra-alist extra-alist)
               ;; almost same code as beginning of `oauth2-auto--request'
               (provider-info (oauth2-auto--provider-info provider))
               (data-alist (oauth2-auto--craft-request-alist
                            provider-info data-keys very-extra-alist))
               (data (oauth2-auto--urlify-request data-alist))

               (url (cdr (assoc url-key provider-info)))
               ; open authorization URL in browser
               (response-alist nil))
          (browse-url (concat url "?" data))

          (unless quiet
            (alert "Log in to your account for Emacs in your browser window"
                   :title "Emacs OAuth2 login"
                   :category 'oauth2-auto))
          ; Wait until we get a reply containing 'code and 'state.
          (while (not response-alist)
            (setq response-alist
                  (apply #'oauth2-auto--httpd-filter (aio-chain server-promise))))

          ; return the response, with the 'redirect_uri
          (cons redirect-uri-elt response-alist))
      ; Always kill server-proc
      (delete-process server-proc))))

(defconst oauth2-auto--url-unreserved
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXY0123456789-_"
  "List of valid non-padding characters in Base64 URL encoded string.")

(defun oauth2-auto--random-string (len)
  "Return a random string of length LEN.
Uses only characters valid in the output of `base64url-encode-string'."
  ; inspired by http://xahlee.info/emacs/emacs/elisp_insert_random_number_string.html
  (let ((rand-len (length oauth2-auto--url-unreserved)))
   (with-temp-buffer
     (dotimes (_l len)
       (insert (elt oauth2-auto--url-unreserved (random rand-len))))
     (buffer-string))))

(defun oauth2-auto--base64url-encode-string (string &optional no-pad)
  "Package-local version of ‘base64url-encode-string’.

Base64url-encode STRING and return the result.

Optional second argument NO-PAD means do not add padding char =.

This produces the URL variant of base 64 encoding defined in RFC 4648.

Exists because this package is compatible with Emacs 26.1, but
‘base64url-encode-string’ was only added in Emacs 27.1."
  (if (fboundp 'base64url-encode-string)
      ;; Use funcall to silence flycheck.
      (funcall 'base64url-encode-string string no-pad)
    (let* ((enc (base64-encode-string string t))
           (enc (replace-regexp-in-string "+" "-" enc))
           (enc (replace-regexp-in-string "/" "_" enc)))
      (if no-pad
          (replace-regexp-in-string "=" "" enc)
        enc))))

;; Control flow to authenticate client to the OAuth2 providers


(aio-defun oauth2-auto-refresh-or-authenticate (auth-request plist)
  "Try to refresh, and if refreshing fails, authenticate.
For USERNAME, PROVIDER, and PLIST see ‘oauth2-auto-refresh’."
  (let* ((promise (oauth2-auto-refresh (oauth2-auto-assoc 'provider auth-request) plist))
         (result (aio-await (aio-catch promise))))
    (if (eq (car result) :success)
        ; If succeeded, return the result
        (cdr result)
      ; If failed, authenticate instead
      (aio-await (oauth2-auto-authenticate auth-request)))))

(aio-defun oauth2-auto-authenticate (auth-request)
  "Authenticate AUTH-REQUEST and return a plist from `oauth2-auto--make-token-plist'."
  (let* ((provider (oauth2-auto-assoc 'provider auth-request))
         (state (oauth2-auto--random-string 8))
         (code-verifier (oauth2-auto--random-string 43))
         (binary-code-challenge (secure-hash 'sha256 code-verifier nil nil t))
         (response (aio-await
                    (oauth2-auto--browser-request
                     provider 'authorize_url
                     '(client_id tenant)
                     `((login_hint . ,(oauth2-auto-assoc 'username auth-request))
                       (scope . ,(oauth2-auto-assoc 'scopes auth-request))
                       (response_type . "code")
                       (response_mode . "query") ;; microsoft-only
                       (access_type . "offline") ;; google-only
                       (state . ,state)
                       (code_challenge . ,(oauth2-auto--base64url-encode-string binary-code-challenge t))
                       (code_challenge_method . "S256")))))
         (response-state (oauth2-auto-assoc 'state response))
         (redirect-uri (oauth2-auto-assoc 'redirect_uri response))
         (code (oauth2-auto-assoc 'code response)))

    ; Verify that the return state matches.
    ; https://docs.microsoft.com/en-us/azure/active-directory/develop/v2-oauth2-auth-code-flow#successful-response
    (unless (equal state response-state)
      (error
       "State sent and returned do not match - security risk: state=%s response_state=%s"
       state response-state))

    (oauth2-auto--make-token-plist
     (aio-await
      (oauth2-auto--request
       provider 'token_url
       '(client_id tenant client_secret)
       `((redirect_uri . ,redirect-uri)
         (code . ,code)
         (grant_type . "authorization_code")
         (code_verifier . ,code-verifier))))
     nil)))

(aio-defun oauth2-auto-refresh (provider plist)
  "Refresh access on PROVIDER using the refresh-token in PLIST.
Return the refreshed plist."
  (let ((refresh-token (plist-get plist :refresh-token)))
    (unless refresh-token
      (error "Refresh token is nil in plist=%s" (pp-to-string plist)))

    ; Refresh an oauth2-token
    (oauth2-auto--make-token-plist
     (aio-await (oauth2-auto--request
                 provider 'token_url
                 '(client_id tenant client_secret)
                 `((refresh_token . ,refresh-token)
                   (grant_type . "refresh_token"))))
     plist)))

(provide 'oauth2-auto)

;;; oauth2-auto.el ends here
