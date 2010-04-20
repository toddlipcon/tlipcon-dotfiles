;;; Example .emacs file for java-mode + aspectj minor mode from a
;;; Windows configuration for GNU Emacs.
;;;
;;; This file goes in your "home" directory, which may not be an obvious
;;; place on Windows.  Check your HOME variable or try "cd ~" under bash.
;;; Parts of this are adopted from the JDEE website (sunsite.auc.dk/jde/)

;; I keep my emacs packages on c:/Emacs
(setq load-path (cons "c:/Emacs/aspectj-mode" load-path))

(require 'aspectj-mode)

;; WARNING: running shells on Windows has some interesting configuration
;; issues.  If you do not use the commands below, make sure the "sh" shell
;; is accessible at /bin/sh or you've configured emacs to use "bash" (See
;; example on JDEE web site for example code).  See the README for more details.
(when (eq system-type 'windows-nt)
  (setq shell-file-name "cmdproxy.exe")
  (setq explicit-shell-file-name shell-file-name)
  (setenv "SHELL" shell-file-name))

;; Gets all the text highlighting you could want.
(when (not (string-match "XEmacs" emacs-version))
  (global-font-lock-mode t))
