README
======

Description
-----------
Emacs configuration files, everything starts from init.el.


Install
-------

    git clone git@github.com:Sarcasm/.emacs.d.git ~/.config/emacs
    emacs --eval "(package-install-selected-packages)"

Configuration
-------------

You can optionally create an identity file where mail address, user
full name, etc, are recommended to be set.

An example of the content of this file can be:

    $ cat identity.el
    (setq user-mail-address "firstname.lastname@mailprovider.foo"
          user-full-name    "Firstname Lastname")

Startup time performance
------------------------

Care should be taken to not `require` unnecessarily.
Prefer add-hook, keybindings, ...

Some commands of things I'd like to be fast:

    $ perf stat emacs -l ~/.config/emacs/init.el -batch --eval '(message "Hello, world!")'
    $ for i in {1..10}; { perf stat emacs -l ~/.emacs.d/init.el -batch --eval '(message "Hello, world!")' |& grep "seconds time elapsed" ; } | sort -n
    $ EDITOR='emacs -nw' git commit

    $ for file in init.el init.d/*.el lisp/*.el; emacs -batch -L lisp -f batch-byte-compile $file
    $ perf stat emacs -l ~/.emacs.d/init.elc -batch --eval '(message "Hello, world!")'
    $ find . -name "*.elc"

Also check time reported by `M-x emacs-init-time RET`,
to account for UI elements.

Troubleshooting
---------------

Package Quickstart
------------------

If quickstarts aren't up-to-date, call:

    M-x package-quickstart-refresh RET
