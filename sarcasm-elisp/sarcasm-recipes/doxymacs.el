(:name doxymacs
       :website "http://doxymacs.sourceforge.net/"
       :description "Doxymacs is Doxygen + {X}Emacs."
       :type git
       :url "git://github.com/Sarcasm/doxymacs.git"
       :load-path ("./lisp")
       :build ("./bootstrap" "./configure" "make")
       :features doxymacs)
