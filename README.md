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

```lisp
;; So the idea is that you copy/paste this code into your *scratch* buffer,
;; hit C-j, and you have a working el-get.
(url-retrieve
 "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
 (lambda (s)
   (end-of-buffer)
   (eval-print-last-sexp)))
```

Contact
-------
Guillaume Papin - guillaume.papin@epitech.eu
