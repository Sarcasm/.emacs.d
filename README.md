README
======

Description
-----------
Emacs configuration files, starting from init.el.

Requirements
------------
The configuration depend on git, subversion and rake.

On debian the following command should install the required packages:

    sudo aptitude install git subversion ruby rake cvs


Install
-------
The config use *el-get* (https://github.com/dimitri/el-get).

In the console:

    git clone git@github.com:Sarcasm/.emacs.d.git ~/.emacs.d
    emacs -Q

In the *scratch* buffer:

    ;; So the idea is that you copy/paste this code into your *scratch* buffer,
    ;; hit C-j, and you have a working el-get.
    (url-retrieve
     "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
     (lambda (s)
       (end-of-buffer)
       (eval-print-last-sexp)))


Configuration
-------------
You can optionally create an identity file where mail address, user
full name, etc, are recommended to be set.

An example of the content of this file can be:

    $ cat sarcasm-identity.el
    (setq user-mail-address "firstname.lastname@gmail.com"
          user-full-name    "Firstname Lastname")
    (provide 'sarcasm-identity)
    $

Changes to `custom-file` should probably be ignored by git, one can do that with
the following command:

    git update-index --assume-unchanged /path/to/sarcasm-custom.el
