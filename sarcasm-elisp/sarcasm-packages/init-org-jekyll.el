
(defun sarcasm-init-org-jekyll ()
  ;; `(require 'org-publish)' Need to be delayed before after the
  ;; loading of org-mode to get the latest version installed with
  ;; el-get. Otherwise org-publish will load the one on the system,
  ;; and when el-get initialize org-mode afterwards it becomes messy
  ;; since the system wide installation has already been loaded.
  (require 'org-publish)
  (require 'org-jekyll))

(add-hook 'org-mode-hook 'sarcasm-init-org-jekyll)

;; (setq org-jekyll-lang-subdirs '(("en" . "publish-blog/blog/")
;;                                 ("es" . "publish-bitacora/bitacora/")))

;; For publishing see:
;; http://orgmode.org/worg/org-tutorials/org-publish-html-tutorial.html

;; Generate htmlize css with `org-export-htmlize-generate-css'
;; And use 'css with htmlize with `org-export-htmlize-output-type'

(setq org-export-htmlize-output-type 'css)

(setq sarcasm-org-directory (expand-file-name "~/Org/")
      sarcasm-jekyll-directory (expand-file-name
                                "~/projects/Perso/sarcasm.github.com/"))

;; Post titles should be <h1> not <h2>
;; FIXME: links are removed ?
(setq org-export-html-toplevel-hlevel 1)

;; http://orgmode.org/worg/org-tutorials/org-publish-html-tutorial.html
(setq org-publish-project-alist
      (list
       (list "sarcasm-jekyll"
             :base-directory sarcasm-org-directory
             :recursive t
             :base-extension "org"
             :publishing-directory sarcasm-jekyll-directory
             ;; `:site-root' is your blog's site name, like
             ;; "http://juanreyero.com". Used to build the entries' titles
             ;; as absolute links. If not set the links will be relative,
             ;; and will appear broken in some feeds aggregators and in
             ;; Google Buzz.
             ;;
             ;; :site-root "https://sarcasm.github.com"
             :publishing-function 'org-publish-org-to-html
             :section-numbers nil
             :table-of-contents nil
             :headline-levels 5
             :body-only t
             :auto-index nil
             :auto-preamble nil
             :auto-postamble nil
             :html-preamble nil
             :html-postamble nil

             ;; org-jekyll settings
             :jekyll-sanitize-permalinks t
             :blog-publishing-directory sarcasm-jekyll-directory
             )

       (list "sarcasm-img"
             :base-directory sarcasm-org-directory
             :recursive t
             ;; :exclude "^publish"
             :base-extension "jpg\\|gif\\|png"
             :publishing-directory sarcasm-jekyll-directory
             :publishing-function 'org-publish-attachment

             ;; org-jekyll settings
             :jekyll-sanitize-permalinks t
             :blog-publishing-directory sarcasm-jekyll-directory)

       '("sarcasm" :components ("sarcasm-jekyll" "sarcasm-img"))))
