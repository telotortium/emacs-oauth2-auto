# Emacs oauth2-auto

Authenticate yourself to an OAuth2 provider from inside Emacs.

For example, running

```emacs-lisp
(oauth2-auto-access-token-sync "your-email@example.org" 'google '(email))
```

requests an access token for user `your-email@example.org` from Google's
servers. This access token can then be used by other software to fetch and send
email, for example using [mbsync](https://git.code.sf.net/p/isync/isync) and
[msmtp](https://git.marlam.de/git/msmtp.git).

## OAuth scopes and providers

When calling `oauth2-auto-access-token` or similar, you need to request an
authentication provider, and which data you would like to access to (or the
*scope* of the request). The library has support for different scopes from
different providers. You may also add providers by modifying
`oauth2-auto-additional-providers-alist` (see [Configuration](#Configuration)).

You may request multiple scopes in one authentication call. For example, all of these are valid:

```emacs-lisp
(oauth2-auto-access-token-sync "your-email@example.org" 'google '(calendar))
(oauth2-auto-access-token-sync "your-email@example.org" 'google '(calendar email))
(oauth2-auto-access-token-sync "your-email@example.org" 'microsoft '(email))
```

### Google

Emacs `oauth2-auto` is in the process of being verified as an OAuth app, so no
configuration is needed to authenticate yourself to Google servers.

The following scopes are supported by default:
- `email`: fetch and send user mail, for example using
  [mbsync](https://git.code.sf.net/p/isync/isync) and
  [msmtp](https://git.marlam.de/git/msmtp.git).
- `calendar`: read and write the user's calendar events, for example using [org-gcal](https://github.com/kidd/org-gcal.el).


### Microsoft
To authenticate yourself to Microsoft, you need to create your own app and add
the client ID and secret to your config. For example,

```emacs-lisp
(setq oauth2-auto-microsoft-default-tenant "common"
      oauth2-auto-microsoft-client-id "my-ms-app-client-id"
      oauth2-auto-microsoft-client-secret "my-ms-app-client-secret")
```

The following scopes are supported by default:
- `email`: fetch and send user mail, for example using
  [mbsync](https://git.code.sf.net/p/isync/isync) and
  [msmtp](https://git.marlam.de/git/msmtp.git).

## Configuration

### Customizable variables

The following customizable variables let you change the behaviour of `oauth2-auto`:

- `oauth2-auto-plstore`: where to store the access tokens.
  - These will be encrypted if `plstore-encrypt-to` (included with Emacs) is configured
- `oauth2-auto-additional-providers-alist`: extra providers that `oauth2-auto`
  doesn't include by default, who also follow the OAuth2 protocol. This alist
  should follow the format from the value in `oauth2-auto--default-providers`.

## Alerts

When you request an OAuth token from Emacs, it opens a link in your browser
which asks you to log in to your authentication provider. `oauth2-auto` uses
[alert](https://github.com/jwiegley/alert/) to make sure the user knows to go to
their browser and log in.

To modify how these notifications are displayed, edit the `"oauth2-auto"`
category of alerts. For example,

```emacs-lisp
(alert-add-rule :category "oauth2-auto" :style 'notifications)
```

### Fetching the token for email authentication

If you would like to read your email authenticated with `XOAUTH2`, you need to
pass the access token to your email reader.
[Mbsync](https://git.code.sf.net/p/isync/isync) and
[msmtp](https://git.marlam.de/git/msmtp.git) let you give them a password
command instead of writing the password to your config (see [tecosaur's very
well-documented
config](https://tecosaur.github.io/emacs-config/config.html#mail)). This library
plus the script below allow you to use `emacs --batch --script
oauth2-token-fetch.el` to refresh the access token if necessary and send it to
stdout.

```emacs-lisp
;;; oauth2-token-fetch.el -*- lexical-binding: t; -*-

(let ((username (nth 3 command-line-args))
      (provider (nth 4 command-line-args))
      (repo-dir
       (concat straight-base-dir ".local/straight/repos/")))

  ; very basic parsing of command line arguments
  (unless (and username provider)
    (princ "Could not find username or provider.\n")
    (princ "emacs --script fetch-oauth-token.el USERNAME PROVIDER\n")
    (kill-emacs 1))

  ; Load and configure the GPG encryption by `plstore'
  (require 'plstore)
  (setq plstore-encrypt-to "my-email@example.com")

  ; Load dependencies
  (require 'aio (concat repo-dir "emacs-aio/aio.el"))
  (require 'dash (concat repo-dir "dash.el/dash.el"))
  (require 'alert (concat repo-dir "alert/alert.el"))
  (require 'oauth2-auto (concat repo-dir "emacs-oauth2-auto-oauth2-auto.el"))

  ; Load client ID, secrets and tenants
  (load-file "/path/to/oauth2-auto-config.el")

  ; Authenticate user and print access token
  (princ (oauth2-auto-access-token-sync username (intern provider)))
  (princ "\n"))
```

## Privacy policy

In short, Emacs `oauth2-auto` runs in your local machine and does not send away
any of your data. We developers have no way to access it. For more explanation
see our [Privacy Policy](./privacy-policy).
