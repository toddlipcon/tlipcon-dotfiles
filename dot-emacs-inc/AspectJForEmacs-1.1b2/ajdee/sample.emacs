;;; Example .emacs/_emacs file for JDEE + aspectj minor mode from a Windows
;;; configuration for GNU Emacs.
;;;
;;; This file goes in your "home" directory, which may not be an obvious
;;; place on Windows.  Check your HOME variable or try "cd ~" under bash.
;;; Parts of this are adopted from the JDEE website (sunsite.auc.dk/jde/)

;; I keep my emacs packages on c:/Emacs
(setq load-path 
   (append
    '( 
      "c:/Emacs/aspectj-mode" 	; for AJDEE
      "c:/Emacs/ajdee"
      "c:/Emacs/jde-2.2.9beta7/lisp"
      "c:/Emacs/elib-1.0"		; for JDEE
      "c:/Emacs/speedbar-0.14beta2"	; for JDEE
      "c:/Emacs/semantic-1.4beta12"	; for JDEE/speedbar
      "c:/Emacs/eieio-0.17beta3"	; for JDEE
     ) 
     load-path))

(require 'jde)

;;; Can also put this in a project's prj.el file to keep this from affecting
;;; Java projects.  See sample.prj for details.
(require 'ajdee)

;;; JDEE/speedbar settings to make it behave better for AspectJ.  You might
;;; want to do this via a prj.el file (see sample.prj) if you program in
;;; vanilla Java as well.  The sample.prj has settings for spacewar, a more
;;; complicated project.
(custom-set-variables
 '(jde-compiler '("ajc" "ajc")) ; ajc is AspectJ's compiler
 '(jde-javadoc-command-path "ajdoc")

 ;; Widen the speedbar to show more of AJ's longer tag names.
 '(speedbar-frame-parameters
   (quote ((minibuffer) (width . 30) (border-width . 0) (menu-bar-lines . 0)
           (unsplittable . t))))
 ;; Don't let speedbar split into submenus smaller than 40 items
 '(speedbar-tag-split-minimum-length 40)

 ;; Default compilation options for a simple one-directory project.
 ;; See sample.prj for a sample prj.el file for compiling, running,
 ;; and debugging spacewar.
 '(aspectj-compile-file-specification "*.java") ; default for simple projects
)

;; Sets the basic indentation for Java source files
;; to two spaces in JDEE.
(defun my-jde-mode-hook ()  (setq c-basic-offset 2))
(add-hook 'jde-mode-hook 'my-jde-mode-hook)

;; WARNING: running a shell on Windows has some interesting configuration
;; issues.  If you do not use the commands below, make sure the "sh" shell
;; is accessible at /bin/sh or you've configured emacs to use "bash" (See
;; example on JDEE web site for example code).  See the user guide for details.
(when (eq system-type 'windows-nt)
  (setq shell-file-name "cmdproxy.exe")
  (setq explicit-shell-file-name shell-file-name)
  (setenv "SHELL" shell-file-name))

;; Gets all the text highlighting you could want.
(when (not (string-match "XEmacs" emacs-version))
  (global-font-lock-mode t))

