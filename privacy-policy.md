## Privacy policy

In short: Emacs `oauth2-auto` only stores the authentication token in your local
machine, and does not transmit it. We, the authors of this code, have **no
access to your data**.

Emacs `oauth2-auto` itself does not access or store any sensitive user data,
other than the OAuth token required for authentication. In particular, it does
not store the username or password, or any email or calendar events from the
user.

The authentication token fetched by Emacs `oauth2-auto` is stored in the user's
hard drive, encrypted with their own GPG key using the `plstore-encrypt-to`
variable from the Emacs environment.

The resulting token may then be used by other software in the user's computer,
mainly (but not exclusively) in their Emacs installation. What the user does
with their own token is the user's responsibility.

The prototypical example uses of Emacs `oauth2-auto` are:

- Reading and writing mail using [mu4e](https://github.com/djcb/mu) or other
  Emacs email clients
- Reading and writing calendar events using
  [org-gcal](https://github.com/kidd/org-gcal.el)

Both of these applications store the user's data in their hard drive and do not
share it over the network.

## Copyright notice

The copyright for this license is

Copyright (C) 2022 Adrià Garriga Alonso

Licensed under GPLv3 or later, see the project page
<https://github.com/rhaps0dy/emacs-oauth2-auto>
