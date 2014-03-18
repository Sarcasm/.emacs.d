README
======

Description
-----------
Emacs configuration files, everything stats from init.el.


Install
-------

    git clone git@github.com:Sarcasm/.emacs.d.git ~/.emacs.d

The config uses *el-get* (https://github.com/dimitri/el-get). Please refer to
the project page to install it. It is recommended to install it using in a
non-customized Emacs instance, with `emacs -Q`, otherwise the loading the
configuration will probably results in failures as el-get won't be installed.

Configuration
-------------
Changes to `custom-file` should probably be ignored by git, one can do that with
the following command:

    git update-index --assume-unchanged /path/to/sarcasm-custom.el

You can optionally create an identity file where mail address, user
full name, etc, are recommended to be set.

An example of the content of this file can be:

    $ cat sarcasm-identity.el
    (setq user-mail-address "firstname.lastname@gmail.com"
          user-full-name    "Firstname Lastname")
    (provide 'sarcasm-identity)
    $
