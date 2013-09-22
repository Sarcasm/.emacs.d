;; Org-Mode LaTeX export config -- Guillaume Papin
;; usage:
;; (require 'sarcasm-org-latex)
;;
;; WWW that help:
;; http://orgmode.org/worg/org-faq.html#using-xelatex-for-pdf-export
;; http://orgmode.org/worg/org-contrib/babel/examples/article-class.html
;; http://orgmode.org/worg/org-tutorials/org-latex-export.html
;; http://emacs-fu.blogspot.com/2011/04/nice-looking-pdfs-with-org-mode-and.html
;; http://nitens.org/taraborelli/cvtex
;; http://www.kieranhealy.org/esk/kjhealy.html
;; http://lists.gnu.org/archive/html/emacs-orgmode/2011-07/msg01056.html

(require 'org-latex)
(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))

(add-to-list 'auto-mode-alist '("\\.lco\\'" . latex-mode))

(defvar sarcasm-tex2pdf-engine "xelatex"
  "The engine use to convert TeX file to PDF.")

;;
;; Syntax highlighting
;;
;; 2 possibilities:
;; - listing :: default package in LaTeX
;; - minted  :: Use the Pygments a syntax highlighter written in Python
;;
;; If prefer to use minted that I find more "relevant".
;;
;; For the sake of consistency, in case Pygments couldn't be installed
;; here is the brief description for the use of the listing package.
;;
;;
;; Listing code:
;;
;; (setq org-export-latex-listings t)
;; (add-to-list 'org-export-latex-packages-alist '("" "listings"))
;; (add-to-list 'org-export-latex-packages-alist '("" "color"))
;;
;; ;; note (Org-Mode 7.7 changes):
;; ;; New variables `org-export-latex-listings-options' allow package
;; ;; options to be controlled
;; (setq org-export-latex-listings-options
;;       '(("basicstyle" "\small")
;;         ("commentstyle" "\color{red}")
;;         ("stringstyle" "\color{green}")
;;         ("keywordstyle" "\color{blue}")))
;;

;;
;; Minted - http://code.google.com/p/minted/
;;
;; Installation (on Debian):
;; sudo easy_install Pygments
;; sudo mkdir -p /usr/share/texmf/tex/minted
;; sudo wget -O /usr/share/texmf/tex/minted/minted.sty \
;; http://minted.googlecode.com/files/minted.sty
;; sudo texhash
;;
;; Note (Org-Mode 7.7 changes):
;; New variables `org-export-latex-custom-lang-environments' allows
;; arbitrary configuration on a per-language basis.
;;
;; Configuration:
;; /!\ See `org-export-latex-listings' (a variable) documentation for
;; the settings /!\
(setq org-export-latex-listings 'minted)
(add-to-list 'org-export-latex-packages-alist '("" "minted"))

;; Add the "-shell-escape" option to the command line as required for
;; minted.
(setq sarcasm-tex2pdf-engine (concat sarcasm-tex2pdf-engine " -shell-escape"))

;; Note: the mintedbg color need to be define with a \definecolor{}
;; Example:
;; \definecolor{mintedbg}{rgb}{0.95,0.95,0.95}
(setq org-export-latex-minted-options
      '(("frame" "lines")
        ("bgcolor" "mintedbg")
        ("linenos" "")))

;; See: http://orgmode.org/worg/org-contrib/babel/examples/article-class.html
(setq org-export-latex-default-packages-alist
      ;; ( "options" "package" snippet-flag)
      '((""             "graphicx"    t)
        (""             "longtable"   nil)
        (""             "float"       nil)
        (""             "fontspec"    t)
        (""             "soul"        t)
        (""             "xunicode"    t)
        (""             "xltxtra"     t)
        (""             "url"         t)
        (""             "rotating"    t)
        ("american"     "babel"       t) ;correct ?
        ("babel"        "csquotes"    t)
        ;; For French try the following
        ;; see: http://perso.enstimac.fr/~gaborit/latex/latex-in-french.html
        ;; ("francais"     "babel"       t)
        ;; (""             "xspace"      t)
        ;; ("babel,french=guillements*"        "csquotes"    t)
        ("usenames,dvipsnames" "color" nil) ;colored links
        ;; ("xetex" "hyperref" nil)
        ;; - urlcolor for external links
        ;; - linkcolor for internal links (i.e. the TOC)
        ("xetex,colorlinks=true,urlcolor=BlueViolet,linkcolor=BlueViolet,unicode=true" "hyperref" nil)))

(setq org-export-latex-classes
      (cons '("sarcasm-article"
              "\\documentclass[12pt,a4paper]{report}
 [DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]
\\definecolor{mintedbg}{rgb}{0.95,0.95,0.95}

\\usepackage[a4paper,includeall,bindingoffset=0cm,margin=2cm,
            marginparsep=0cm,marginparwidth=0cm]{geometry}

\\defaultfontfeatures{Scale=MatchLowercase,Mapping=tex-text}
\\setromanfont[Mapping={tex-text},Numbers={OldStyle},Ligatures={Common}]{Linux Libertine O}
\\setsansfont[Mapping=tex-text,Colour=AA0000]{Linux Biolinum O}
% \\setsansfont[Mapping=tex-text,Colour=AA0000]{DejaVU Sans Mono}
\\setmonofont[Mapping=tex-text,Scale=0.9]{Inconsolata}

% Gentium style
% \\setromanfont{Gentium}
% \\setromanfont [BoldFont={Gentium Basic Bold},
%                 ItalicFont={Gentium Basic Italic}]{Gentium Basic}
% \\setsansfont{Charis SIL}
% \\setmonofont[Scale=0.8]{Inconsolata}"

              ("\\section{%s}"       . "\\section*{%s}")
              ("\\subsection{%s}"    . "\\subsection*{%s}")
              ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
              ("\\paragraph{%s}"     . "\\paragraph*{%s}")
              ("\\subparagraph{%s}"  . "\\subparagraph*{%s}"))
            org-export-latex-classes))

(setq org-export-latex-default-class "sarcasm-article")

;; Construct the exporting command (3 runs like the default).
(let ((process-pdf-cmd (concat sarcasm-tex2pdf-engine
                               " -interaction nonstopmode -output-directory "
                               (if (string-match-p "^6\\." org-version)
                                   "%b %s"   ;Org-Mode 6.33x
                                 "%o %f")))) ;Org-Mode 7.7
  (setq org-latex-to-pdf-process nil)
  (dotimes (_ 3)
    (push process-pdf-cmd org-latex-to-pdf-process)))

(provide 'sarcasm-org-latex)
