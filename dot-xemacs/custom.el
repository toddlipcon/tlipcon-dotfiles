(load-file "~/.xemacs/darkcolor.el")

;(set-face-font 'default '("-*-fixed-medium-r-*--14-*-iso8859-1"));;

(custom-set-variables
 '(bsh-commands "/usr/share/java/bsh-commands")
 '(bsh-jar "/tmp/bsh_jar/bsh-2.0b4.jar")
 '(font-lock-maximum-decoration t)
 '(font-lock-verbose nil)
 '(gutter-buffers-tab-enabled nil)
 '(indent-tabs-mode nil)
 '(jde-ant-enable-find t)
 '(jde-ant-program "/home/todd/bin/ant_fixjvm")
 '(jde-jdk-registry (quote (("1.6.0" . "/usr/lib/jvm/java-6-sun"))))
 '(lineker-column-limit 80)
 '(package-get-download-sites (quote (("US (Main XEmacs Site)" "ftp.xemacs.org" "pub/xemacs/packages") ("Australia (aarnet.edu.au)" "mirror.aarnet.edu.au" "pub/xemacs/packages") ("Australia (au.xemacs.org)" "ftp.au.xemacs.org" "pub/xemacs/packages") ("Austria (at.xemacs.org)" "ftp.at.xemacs.org" "editors/xemacs/packages") ("Belgium (be.xemacs.org)" "ftp.be.xemacs.org" "xemacs/packages") ("Brazil (br.xemacs.org)" "ftp.br.xemacs.org" "pub/xemacs/packages") ("Canada (ca.xemacs.org)" "ftp.ca.xemacs.org" "pub/Mirror/xemacs/packages") ("Canada (crc.ca)" "ftp.crc.ca" "pub/packages/editors/xemacs/packages") ("Canada (ualberta.ca)" "sunsite.ualberta.ca" "pub/Mirror/xemacs/packages") ("Czech Republic (cz.xemacs.org)" "ftp.cz.xemacs.org" "MIRRORS/ftp.xemacs.org/pub/xemacs/packages") ("Denmark (dk.xemacs.org)" "ftp.dk.xemacs.org" "pub/emacs/xemacs/packages") ("Finland (fi.xemacs.org)" "ftp.fi.xemacs.org" "pub/mirrors/ftp.xemacs.org/pub/tux/xemacs/packages") ("France (fr.xemacs.org)" "ftp.fr.xemacs.org" "pub/xemacs/packages") ("France (mirror.cict.fr)" "mirror.cict.fr" "xemacs/packages") ("France (pasteur.fr)" "ftp.pasteur.fr" "pub/computing/xemacs/packages") ("Germany (de.xemacs.org)" "ftp.de.xemacs.org" "pub/ftp.xemacs.org/tux/xemacs/packages") ("Germany (tu-darmstadt.de)" "ftp.tu-darmstadt.de" "pub/editors/xemacs/packages") ("Ireland (ie.xemacs.org)" "ftp.ie.xemacs.org" "mirrors/ftp.xemacs.org/pub/xemacs/packages") ("Italy (it.xemacs.org)" "ftp.it.xemacs.org" "unix/packages/XEMACS/packages") ("Japan (aist.go.jp)" "ring.aist.go.jp" "pub/text/xemacs/packages") ("Japan (asahi-net.or.jp)" "ring.asahi-net.or.jp" "pub/text/xemacs/packages") ("Japan (dti.ad.jp)" "ftp.dti.ad.jp" "pub/unix/editor/xemacs/packages") ("Japan (jaist.ac.jp)" "ftp.jaist.ac.jp" "pub/GNU/xemacs/packages") ("Japan (jp.xemacs.org)" "ftp.jp.xemacs.org" "pub/GNU/xemacs/packages") ("Japan (nucba.ac.jp)" "mirror.nucba.ac.jp" "mirror/xemacs/packages") ("Japan (sut.ac.jp)" "sunsite.sut.ac.jp" "pub/archives/packages/xemacs/packages") ("Korea (kr.xemacs.org)" "ftp.kr.xemacs.org" "pub/tools/emacs/xemacs/packages") ("New Zealand (nz.xemacs.org)" "ftp.nz.xemacs.org" "mirror/ftp.xemacs.org/packages") ("Norway (no.xemacs.org)" "ftp.no.xemacs.org" "pub/xemacs/packages") ("Poland (pl.xemacs.org)" "ftp.pl.xemacs.org" "pub/unix/editors/xemacs/packages") ("Russia (ru.xemacs.org)" "ftp.ru.xemacs.org" "pub/xemacs/packages") ("Slovakia (sk.xemacs.org)" "ftp.sk.xemacs.org" "pub/mirrors/xemacs/packages") ("South Africa (za.xemacs.org)" "ftp.za.xemacs.org" "mirrorsites/ftp.xemacs.org/packages") ("Sweden (se.xemacs.org)" "ftp.se.xemacs.org" "pub/gnu/xemacs/packages") ("Switzerland (ch.xemacs.org)" "ftp.ch.xemacs.org" "mirror/xemacs/packages") ("UK (uk.xemacs.org)" "ftp.uk.xemacs.org" "sites/ftp.xemacs.org/pub/xemacs/packages") ("US (ibiblio.org)" "ibiblio.org" "pub/mirrors/xemacs/packages") ("US (stealth.net)" "ftp.stealth.net" "pub/mirrors/ftp.xemacs.org/pub/xemacs/packages") ("US (unc.edu)" "metalab.unc.edu" "pub/packages/editors/xemacs/packages") ("US (us.xemacs.org)" "ftp.us.xemacs.org" "pub/xemacs/packages") ("US (utk.edu)" "ftp.sunsite.utk.edu" "pub/xemacs/packages"))))
 '(query-user-mail-address nil)
 '(sgml-markup-faces (quote ((start-tag . default) (end-tag . default) (comment . default) (pi . default) (sgml . default) (doctype . default) (entity . default) (shortref . default) (ignored . default) (ms-start . default) (ms-end . default))))
 '(user-mail-address "todd@lipcon.org"))

(custom-set-faces
 '(default ((t (:foreground "gray90" :background "black"))) t)
 '(blue ((t (:foreground "#99f"))) t)
 '(bold ((t nil)) t)
 '(cperl-array-face ((((class color) (background light)) (:foreground "Red"))))
 '(cperl-hash-face ((((class color) (background light)) (:foreground "Red"))))
 '(custom-group-tag-face ((((class color) (background light)) (:foreground "#87cefa" :underline t))))
 '(custom-state-face ((((class color) (background light)) (:foreground "#90ee90"))))
 '(custom-variable-tag-face ((((class color) (background light)) (:foreground "#87cefa" :underline t))))
 '(font-lock-comment-face ((nil (:foreground "#66f"))))
 '(font-lock-doc-string-face ((((class color) (background light)) (:foreground "#90ee90"))))
 '(font-lock-function-name-face ((((class color) (background light)) (:foreground "#eeb4b4"))))
 '(font-lock-keyword-face ((((class color) (background light)) (:foreground "#ffb90f"))))
 '(font-lock-preprocessor-face ((((class color) (background light)) (:foreground "#00ced1"))))
 '(font-lock-reference-face ((((class color) (background light)) (:foreground "red"))))
 '(font-lock-string-face ((((class color) (background light)) (:foreground "#90ee90"))))
 '(font-lock-type-face ((((class color) (background light)) (:foreground "cyan2"))))
 '(font-lock-variable-name-face ((((class color) (background light)) (:foreground "plum1"))))
 '(font-lock-warning-face ((((class color) (background light)) (:foreground "Red" :underline nil))))
 '(highlight ((t (:background "#556b2f"))) t)
 '(html-helper-bold-face ((t nil)))
 '(isearch ((t (:background "#008b8b"))) t)
 '(list-mode-item-selected ((t (:background "gray32"))) t)
 '(primary-selection ((t (:background "gray40"))) t)
 '(secondary-selection ((t (:background "#008b8b"))) t)
 '(text-cursor ((t (:foreground "gray70" :background "red"))) t)
 '(widget-documentation-face ((((class color) (background light)) (:foreground "#90ee90"))))
 '(widget-field-face ((((class grayscale color) (background light)) (:background "gray25"))))
 '(widget-inactive-face ((((class grayscale color) (background light)) (:foreground "gray60"))))
 '(zmacs-region ((t (:background "#000077"))) t))


(defvar compilation-directory nil
  "*If non-nil, the directory in which the `\[compile]` command runs.
If nil, the value of `default-directory` is used.")

(defadvice compile (around override-default-directory activate)
  (let ((default-directory (or compilation-directory default-directory)))
    ad-do-it))

(setq load-path
      (append load-path '("/Users/tlipcon/.xemacs/")))

(setq cperl-invalid-face nil)

(setq minibuffer-max-depth nil)
(put 'narrow-to-region 'disabled nil)