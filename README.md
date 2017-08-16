README
======

Description
-----------
Emacs configuration files, everything starts from init.el.


Install
-------

    git clone git@github.com:Sarcasm/.emacs.d.git ~/.emacs.d

Configuration
-------------

You can optionally create an identity file where mail address, user
full name, etc, are recommended to be set.

An example of the content of this file can be:

    $ cat init.d/identity.el
    (setq user-mail-address "firstname.lastname@gmail.com"
          user-full-name    "Firstname Lastname")
    $

Troubleshooting
---------------

Buggy cursor color on KDE
-------------------------

Comment the Emacs*Foreground key of `/usr/share/kdisplay/app-defaults/Emacs.ad`:

```diff
-Emacs*Foreground:		WINDOW_FOREGROUND
+! Emacs*Foreground:		WINDOW_FOREGROUND
```

See:
- https://lists.gnu.org/archive/html/help-gnu-emacs/2002-04/msg00116.html
- https://emacs.stackexchange.com/a/32764/901
