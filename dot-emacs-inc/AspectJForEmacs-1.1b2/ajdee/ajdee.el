;; ajdee --- AspectJ minor mode extensions to JDEE mode

;;;###autoload
(defconst ajdee-version "@build.version.short@"
 "AJDEE Mode version number.")

;; Author: Gregor Kiczales and William Griswold
;; Keywords: JDE, JDEE, tools, AspectJ
;;
;; Copyright (c) Xerox Corporation 1998, 1999, 2000, 2001.
;;
;; This file is part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;; Please send bug reports, etc. to support@aspectj.org.

;<--- edit this file with an editor window that is exactly 80 columns wide --->|
;
; ajdee.el
;

;;; -------------------------------------
;;; Requirements, Installation, and Usage
;;;
;;; ajdee for jde-mode requires a working installation of JDEE mode with
;;; speedbar.
;;;
;;; See ajdee.html and the accompanying example configuration
;;; files in this distribution for details.
;;;

;;; ------------------------------
;;; general header

(require 'aspectj-mode)
(require 'jde)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                        ;;;
;;; See Commentaries on Maintenance, XEmacs, and Declarations              ;;;
;;; Consistency and Caching in aspectj-mode.el                             ;;;
;;;                                                                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Note that jde-mode is derived from java-mode, which means that when
;;; jde-mode starts it runs the java-mode init, including hooks, before its
;;; own.
;;;
;;; NOTE: When the java-mode hook runs, java-mode is the prevailing major
;;; mode; when the  jde-mode hook runs, the jde-mode hook is the prevailing
;;; mode.
;;;
;;; WARNING - the JDE extensions don't work properly unless set up *before*
;;; jde-mode gets going, as we override some settings that are pertinent
;;; during initialization.  This may only affect the Classes menu.  This
;;; set-up is achieved by the extensions running on the java-mode hook rather
;;; than the jde-mode hook.
;;;

;;; Make jde-mode the default mode for AspectJ source code buffers.
;;; Prepend the jde-mode entry so that it shadows the java-mode
;;; entry already in the list.
;;;###autoload
(setq auto-mode-alist (append '(("\\.aj\\'" . jde-mode)) auto-mode-alist))

;;;
;;; BUG/WARNING/WGG - the initial buffer in which JDE/AspectJ mode is
;;; started will not be properly bovinated (noted in 2.2.8beta4).
;;; semantic-bovinate-toplevel will return nil and the cache will have
;;; to be cleared for it to rebovinate properly.
;;;

;;;
;;; We want pop-up menus to be consistent with JDE.
;;;
(customize-set-variable 'aspectj-classic-jump-menu nil)


;;;
;;; Tell aspectj-mode that we're running things now.
;;;
(setq aspectj-mode-setup-function 'aspectj-mode-setup-jde-mode)
(setq aspectj-mode-clear-function 'aspectj-mode-clear-jde-mode)

(defun aspectj-mode-setup-jde-mode ()
  (aspectj-mode-setup-java-mode)
  (when (aspectj-mode-in-force)
    (aspectj-check-jde-version)
    ;(aspectj-setup-local-bsh-startup-directory)
    (aspectj-setup-local-jde-menu-functions)
    ; WGG/BUG see below
    ; (setq jde-db-the-debugger (jde-db-ajdb "ajdb"))
    ))

(defun aspectj-mode-clear-jde-mode ()
  (aspectj-mode-clear-java-mode)
  ;; nothing for version check
  ;(aspectj-clear-local-bsh-startup-directory)
  ;(aspectj-clear-local-jde-menu-functions) - AJ's automatically pass through
  )

;;; This pair toggles what functions are building menus in JDEE.
(defun aspectj-setup-local-jde-menu-functions ()
  (aspectj-setup-local-classes-menu-functions)
  (aspectj-setup-local-speedbar-functions))
    
(defun aspectj-clear-local-jde-menu-functions ()
  (aspectj-clear-local-classes-menu-functions)
  (aspectj-clear-local-speedbar-functions))


;;;
;;; Put consistency funcs on JDEE- and speedbar-specific hooks.
;;;
;;; NOTE: It might appear that the visiting-file/tag hooks aren't
;;; necessary, because we already advise switch-to-buffer and display-buffer,
;;; but these hooks are invoked under complicated situations that may or may
;;; not end up invoking these low-level functions, but it may still be
;;; warranted to check the updates.  Whether this reasoning is right depends
;;; upon how the functions that call these hooks are implemented, which of
;;; course could change at any time.  Since these checks are cheap, we leave
;;; them here for now.
;;;
(defadvice aspectj-setup-annotations-and-jumps-consistency
	   (after aspectj-decls-init activate)
  (aspectj-add-hook-locally 'jde-compile-finish-hook
			    'aspectj-reset-annotations-and-jumps-on-command-h)
  (aspectj-add-hook-locally 'speedbar-visiting-file-hook
			    'aspectj-reset-annotations-and-jumps-on-change)
  (aspectj-add-hook-locally 'speedbar-visiting-tag-hook
			    'aspectj-reset-annotations-and-jumps-on-change))

;;; This wrapper is put on jde-compile-finish-hook.
(defun aspectj-reset-annotations-and-jumps-on-command-h (buffer msg)
  (aspectj-reset-annotations-and-jumps-on-change))

;;;
;;; Have to call parent and check its result for update, since this needs the
;;; up to date data and cache-fresh is automatically true after this call.
;;;
;;; ad-return-value is t if an update was performed, nil if not
;;;
(defadvice aspectj-reset-annotations-and-jumps-on-change
	   (around ajdee-decls activate)
  ad-do-it
  (when (and ad-return-value (eq major-mode 'jde-mode))
    ;; Update the Classes menu.  Could also just call imenu-update-menubar,
    ;; but that doesn't do the cleanup that this does.
    (imenu--menubar-select imenu--rescan-item)))
 
;;;
;;; Could check for speedbar, but we should work for all speedbars since
;;; it had version numbering (0.13, I think).  We also don't want to
;;; get into the game of checking whether JDEE has the right subcomponents
;;; like semantic, etc.  We're just checking the "root".
;;;
(defun aspectj-check-jde-version ()
  (aspectj-check-a-version jde-version "JDEE 2.2.9"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Customize Help functionality.
;;;

(defadvice aspectj-show-help (around aspectj-help activate)
  (let ((aspectj-helpfile-name "ajdee.html"))
    ad-do-it))    

(defadvice aspectj-find-aspectj-directory (around aspectj-help activate)
  (let ((aspectj-helpfile-dir  "ajdee"))
    ad-do-it))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Overrides JDEE compile/build behaviors when JDEE is present.
;;;

(defcustom aspectj-compile-file-specification ""
  "*Specification of file(s) to compile for JDEE extensions.
Serves as alternate to current buffer's or application class's file.
This file specification supplants the standard mechanism by which
JDEE determines which file to compile.  If this option's value is the
empty string, the standard mechanism applies.  If non-empty, it is
appended directly to the compiler invocation's argument list."
  :group 'aspectj
  :type 'string)

;;; Specializes AspectJ menu's compile option to JDEE
(defadvice aspectj-compile-spec-selected-p (around aspectj-compile activate)
  (if (eq major-mode 'jde-mode) ; else java-mode
      (setq ad-return-value
	    (string= aspectj-compile-file-specification
		     (aspectj-make-compile-spec (ad-get-arg 0))))
    ad-do-it))

;;; Specializes AspectJ menu's compile option to JDEE
(defadvice aspectj-compile-with-lst-file (around aspectj-compile activate)
  (if (eq major-mode 'jde-mode) ; else java-mode
      (let ((aspectj-mode-in-force t)) ; from menu, pretend AJ mode in force
	(customize-set-variable 'aspectj-compile-file-specification
	      (aspectj-make-compile-spec (ad-get-arg 0)))
	(setq ad-return-value (jde-compile)))
    ad-do-it))

;;;;
;;;; *Replaces* jde version to choose aspectj-compile-file-spec, if defined.
;;;; New code is commented with "WGG".
;;;;
;(defadvice jde-make-compile-command (around aspectj-compile activate)
;  "Constructs the java compile command as: jde-compiler + options + buffer file name or file-spec."
;  (cond 
;   ((not (aspectj-mode-in-force))               ad-do-it)
;   ((string= aspectj-compile-file-specification "") ad-do-it)
;   (t
;    (let ((more-args (ad-get-arg 0)))
;      (setq ad-return-value
;	    (concat jde-compiler " " 
;		    (jde-get-compile-options) 
;		    (if (not (string= more-args ""))
;			(concat " " more-args))
;		    " "
;		    (if (not (string= aspectj-compile-file-specification "")) ; WGG
;			aspectj-compile-file-specification                    ; WGG
;		      (file-name-nondirectory buffer-file-name))))))))

;;; Has no effect on JDEE's "make" mode.
(defadvice jde-java-build (around aspectj-build activate)
  "Extends jde-java-build to use aspectj-compile-file-specification."
  (cond
   ((not (aspectj-mode-in-force))               ad-do-it) 
   ((string= aspectj-compile-file-specification "") ad-do-it)
   (t             (setq ad-return-value (jde-compile)))))

;;;
;;; WARNING - We could be more aggressive here and force ajc to be the
;;; default compiler when aj-mode is in force.
;;;
(defadvice jde-compile-get-the-compiler (around aspectj-compile activate)
  (if (aspectj-mode-in-force)
      (if (string-match "ajc" (car jde-compiler))
	  (setq ad-return-value (jde-compile-get-ajc))
	ad-do-it)
    ad-do-it))

(defclass jde-compile-ajc (jde-compile-compiler)
  ()
  "Class of AspectJ compilers.")

(defmethod initialize-instance ((this jde-compile-ajc) &rest fields)
 ;; Call parent initializer.

  (call-next-method)

  ;; Set compiler name.
  (oset this :name "ajc")

  ;; Set compiler version.
  (oset this version aspectj-mode-version)) ; WARNING - best guess

(defun jde-compile-get-ajc ()
  (let ((ajc-compiler (nth 1 jde-compiler)))
    (when (or (not ajc-compiler) (string= "" ajc-compiler))
      (setq ajc-compiler "ajc"))
    (jde-compile-ajc
     "ajc"
     :use-server-p nil
     :path ajc-compiler)))

;;; WGG - cut-and-paste of jde-compile-compiler's 2.2.9beta4 :(
(defmethod jde-compile-run-exec ((this jde-compile-ajc))
  (let* ((outbuf (oref this :buffer))
         (compiler-path (oref this :path))
         (source-file (split-string aspectj-compile-file-specification)) ; !!! WGG
         (args (append
		'("-emacssym") ; !!! WGG
                (jde-compile-get-args this)
                (oref this :interactive-args)
                source-file)))

    (save-excursion
      (set-buffer outbuf)

      (insert (format "cd %s\n" default-directory))
      (insert (concat
               compiler-path
               " "
               (mapconcat (lambda (x) x) args " ")
               "\n\n"))

      (let* ((process-environment (cons "EMACS=t" process-environment))
             (proc (apply 'start-process
                          (downcase mode-name)
                          outbuf
                          compiler-path
                          args)))
        (set-process-sentinel proc 'compilation-sentinel)
        (set-process-filter proc 'compilation-filter)
        (set-marker (process-mark proc) (point) outbuf)
        (setq compilation-in-progress
              (cons proc compilation-in-progress))))))

;;; WGG/BUG: the output of ajdb doesn't match jdb, so Emacs can't
;;; recognize it.  At least I think that's the problem.
(defclass jde-db-ajdb (jde-db-jdb) ())

(defadvice jde-db-jdb (around aspectj-debug activate)
  (if (aspectj-mode-in-force)
      (setq ad-return-value (apply 'jde-db-ajdb "ajdb" (ad-get-args 1)))
    ad-do-it))

(defmethod initialize-instance ((this jde-db-ajdb) &rest fields)
  (call-next-method)
  (oset this :name "ajdb")
  (oset this :filename "ajdb"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; "Enables" defadvice on buffer-file-name (WARNING) so that we can do
;;; an AspectJ-style compilation for ajdoc javadoc generation.
;;;

(defvar aspectj-in-jde-javadoc-make nil)

(defadvice jde-javadoc-make  (around aspectj-build activate)
  (if (not (aspectj-mode-in-force))
      ad-do-it
    (let ((aspectj-in-jde-javadoc-make t))
      ad-do-it)))

;;; Turn off the buffer-file-name advice.
(defadvice compile-internal  (around aspectj-build activate)
  (let ((aspectj-in-jde-javadoc-make nil))
    ad-do-it))

;;; Turn off the buffer-file-name advice.
(defadvice save-some-buffers  (around aspectj-build activate)
  (let ((aspectj-in-jde-javadoc-make nil))
    ad-do-it))

;; WARNING - hackola.  See defadvice on jde-make-compile-command for better.
(defadvice buffer-file-name (around aspectj-build first activate)
  (if (or (not (aspectj-mode-in-force))
	  (not aspectj-in-jde-javadoc-make)
	  (string= aspectj-compile-file-specification ""))
      ad-do-it
    ;; jde-javadoc-make does some heinous stuff, so we do, too.
    (let* ((aj-cfs-list (split-string aspectj-compile-file-specification))
	   (aj-cfs-inner-quoted-str
	    (mapconcat '(lambda (x) x) aj-cfs-list "\" \"")))
      (setq ad-return-value aj-cfs-inner-quoted-str))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tweaks to JDEE's completion feature to make it behave better for AJ.
;;; WARNING - This is just a start.  This doesn't do anything special for
;;; special identifiers like thisStaticJoinPoint, completion of things
;;; like proceed within advice, or special actions on objects like hasaspect.
;;;

;;;
;;; Allows JDEE to find aspect decl when looking or class.
;;; From jde-2.2.9beta4
;;;
(defun jde-parse-get-innermost-class-at-point ()
  "Get the innermost class containing point.
If point is in a class, this function returns 
(CLASS_NAME . CLASS_POSITION), where CLASS_NAME is the 
name of the class and CLASS_POSITION is the position
of the first character of the class keyword. Otherwise,
this function returns nil."
  ;; (interactive)
  (let ((left-paren-pos (c-parse-state)))
    (if left-paren-pos
	(save-excursion
	  (catch 'class-found
	    (let ((left-paren-index 0)
		  (left-paren-count (length left-paren-pos)))
	      (while (< left-paren-index left-paren-count)
		(let ((paren-pos (nth left-paren-index left-paren-pos)))
		  (unless (consp paren-pos)
		    (goto-char paren-pos)
		    (if (looking-at "{")
			(let* ((search-end-pos
			       (if (< left-paren-index (1- left-paren-count))
				   (let ((pos (nth (1+ left-paren-index) left-paren-pos)))
				     (if (consp pos)
					 (cdr pos)
				       pos))
				 (point-min)))
                              (case-fold-search nil)
                              (class-re "^[ \t]*\\(\\(public\\|abstract\\|final\\|static\\|strictfp\\|protected\\|privileged\\)[ \t]+\\)*[ \t]*\\(aspect\\|class\\)[ \t]+\\([^ \t\n{]*\\).*") ; !!! WGG
                              (class-pos (re-search-backward class-re search-end-pos t)))      
                           (if class-pos
			      (progn
				(looking-at class-re)
				(throw
				 'class-found
				 (cons
				  (buffer-substring-no-properties
				   (match-beginning 4) ; !!! WGG 3 --> 4
				   (match-end 4)) ; !!! WGG bec added class "RE"
				  class-pos))))))))
		  (setq left-paren-index (1+ left-paren-index)))))))))


;;;;
;;;; Main method returns a list of list of declaration entries:
;;;;
;;;;   ( (...fields...) (...constructors...) (...methods...) )
;;;;
;;;; This advice filters those entries that have dollar-signs in them,
;;;; which are intended for the ajc compiler's use only.
;;;;
;;;; WARNING - there should be a java class provided by the compiler to do
;;;; this for me.
;;;;
;(defadvice jde-complete-get-classinfo (around aspectj-parse activate)
;  ad-do-it
;  (when (aspectj-mode-in-force)
;    ;(message "jde-complete-get-classinfo:\n %s" ad-return-value)
;    (setq ad-return-value
;	  (mapcar #'(lambda (lst)
;		      (remove-if #'(lambda (entry)
;				     (string-match ".*\\$" (car entry))) lst))
;		  ad-return-value))))


;;;
;;; Main method returns a list of list of declaration entries:
;;;
;;;   ( (signature . name) ... )
;;;
;;; This advice filters those entries that have ajc dollar-signs in them,
;;; which are intended for the ajc compiler's use only.
;;;
;;; WARNING: The current pattern is a guesstimate by looking at 1.0 compiler
;;;
;;; WARNING - there should be a java class provided by the compiler to do
;;; this for me.
;;;
(defadvice jde-complete-get-classinfo (around aspectj-parse activate)
  ad-do-it
  (when (aspectj-mode-in-force)
    ;(message "jde-complete-get-classinfo:\n %s" ad-return-value)
    (setq ad-return-value
	  (remove-if #'(lambda (entry)
			 (string-match ".*\\(ajc\\$\\|\\$ajc\\|access\\$\\|\\$method_call\\|this\\$\\|\\$assertionsDisabled\\|aspect\\$\\)" (cdr entry)))
		     ad-return-value))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions to make speedbar swallow our menus, etc.
;;;


;;; WARNING BUG? - navigations via the speedbar are done "outside" the current
;;; buffer, and this impacts several features, like highlighting annotations
;;; (when they appear and (oddly) what the frame-width is.  Consequently,
;;; these features should be checked for all refs to "current" buffers and
;;; frames and make sure that they are the ones we want.  This correction has
;;; been made in one or two places.


;;;
;;; Comment about (speedbar-add-supported-extension ".lst")
;;;
;;; WARNING - Just a start.  We really need a .lst-file mode that sets
;;; up imenu-generic-expression, etc., so that imenu knows about .lst files.
;;; Speedbar complains (a little) when you open one of these, otherwise.  Of
;;; course, once you have a mode, you might as well handle the @ symbols and
;;; all that, providing highlighting and navigation.  Actually, the way Mik
;;; does it is that the whole menu is built off the contents of .lst files.
;;;
;;; WARNING - this may be invasive; should only show when AJ mode on?
;;; Could do this by making the supported-extension list buffer-local.
;;;

;;; Make a speedbar "hook" local so that we can get control.  See speedbar.el.
(defun aspectj-setup-local-speedbar-functions () 
  (make-local-variable 'speedbar-dynamic-tags-function-list) ; once per buffer
  (speedbar-add-supported-extension ".lst") ; WGG - see comment above
  (setq speedbar-dynamic-tags-function-list
	(cons '(aspectj-fetch-speedbar-menu . aspectj-insert-imenu-list)
	      (default-value 'speedbar-dynamic-tags-function-list))))

;;; WARNING - Why am I restoring default-value here, rather than current?
(defun aspectj-clear-local-speedbar-functions () 
  (setq speedbar-dynamic-tags-function-list
	(default-value 'speedbar-dynamic-tags-function-list)))

;;;
;;; WARNING - To correct for speedbar-0.14beta1 change.  Bug report submitted.
;;; The set-buffer line should appear right after the save-excursion in this
;;; function so that queries are on the right buffer-local-variables.
;;;
(defadvice speedbar-fetch-dynamic-tags (around aspectj-decls activate)
  (save-excursion
    (set-buffer (find-file-noselect (ad-get-arg 0)))
    ad-do-it))

;;;
;;; Oh the humanity.  Essentially overrides speedbar-tag-hierarchy-method.
;;; I have to get access to a legit buffer to check the mode against; when
;;; this method is executed we're usually in the speedbar buffer, which is
;;; not an aspectj-mode buffer.
;;;
;;; Doing this with defadvice frees the user from having to put something
;;; in their .emacs file to override these settings.
;;;
;;; Incoming argument might appear as follows:
;;;   ((name marker button) (name marker button) ...)
;;;   (header (name marker button) ...)
;;;
(defadvice speedbar-trim-words-tag-hierarchy (around aspectj-speedbar activate)
  "Makes sure the advised method doesn't execute in aspectj-mode, because it
screws up the menus.  It makes inappropriate assumptions about what the entries
should look like and how they might reasonably be shortened."
  (let* ((arg (ad-get-arg 0))
	 (token (car-safe (cdr-safe (car-safe arg))))) ; assume 1st form
    ;; If get an entry rather than a marker, try 2nd form
    (when (listp token) (setq token (car-safe (cdr-safe token))))
    (save-excursion
      (when (markerp token) (set-buffer (marker-buffer token)))
      (if (or (not (markerp token)) (not (aspectj-mode-in-force)))
	  (progn ad-do-it)
	(setq ad-return-value arg)))))


;;;
;;; WARNING - Horrific blob of essentially copied code below, as dictated
;;; by "extensible" speedbar interface.  Sigh....
;;;

(defun aspectj-insert-imenu-list (indent lst)
  "At level INDENT, insert the imenu generated LST."
  (aspectj-insert-generic-list indent lst
				'aspectj-tag-expand
				'aspectj-tag-find))

;;; Elements of the list are one of the following, where ... is a 
;;; recursive list of the same kind.
;;;   nil
;;;   ("foo" . <int-or-mark>)
;;;   ("foo" <int-or-mark> button-type ...)  !!!  this one is new
;;;   ("foo" . ...)
;;; 

;;;
;;; Our modified version of speedbar-insert-generic-list.  (Mods marked.)
;;;
;;; WGG/WARNING - should probably be updated wrt 0.14beta1, which
;;; appears to include extensions that would make our extensions easier,
;;; if we hadn't already implemented them.
;;;
(defun aspectj-insert-generic-list (level lst expand-fun find-fun)
  "At LEVEL, insert a generic multi-level alist LST.
Associations with lists get {+} tags (to expand into more nodes) and
those with positions just get a > as the indicator.  {+} buttons will
have the function EXPAND-FUN and the token is the CDR list.  The token
name will have the function FIND-FUN and not token."
  ;; Remove imenu rescan button
  (if (string= (car (car lst)) "*Rescan*")
      (setq lst (cdr lst)))
  ;; Adjust the list.
  (setq lst (speedbar-create-tag-hierarchy lst))
  ;; insert the parts
  (while lst
    (cond ((null (car-safe lst)) nil)	;this would be a separator
	  ((or (numberp (cdr-safe (car-safe lst)))
	       (markerp (cdr-safe (car-safe lst))))
	   (aspectj-make-tag-line nil nil nil nil ;no expand button data
				   (car (car lst)) ;button name
				   find-fun        ;function
				   (cdr (car lst)) ;token is position
				   'speedbar-tag-face
				   (1+ level)))
	  ;;!!!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
 	  ((and (not (null (cdr-safe (cdr-safe (car-safe lst)))))
 		(or (numberp (car-safe (cdr-safe (car-safe lst))))
 		    (markerp (car-safe (cdr-safe (car-safe lst))))))
	   (let ((button-name (car   (car lst)))
		 (mark        (cadr  (car lst)))
		 (button-type (caddr (car lst)))
		 (subs        (cdddr (car lst))))
	     (aspectj-make-tag-line button-type
				     (if subs ?+ ? )
				     (and subs expand-fun)
				     subs
				     button-name
				     find-fun
				     mark
				     'speedbar-tag-face
				     (1+ level))))
	  ;;!!!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
	  ((listp (cdr-safe (car-safe lst)))
	   (aspectj-make-tag-line 'curly ?+ expand-fun (cdr (car lst))
				   (car (car lst)) ;button name
				   nil nil 'speedbar-tag-face
				   (1+ level)))
	  (t (speedbar-message "Ooops!")))
    (setq lst (cdr lst))))

;;;
;;; Only here to call other aspectj-... functions.
;;;
(defun aspectj-tag-expand (text token indent)
  "Expand a tag sublist.  Imenu will return sub-lists of specialized tag types.
Etags does not support this feature.  TEXT will be the button
string.  TOKEN will be the list, and INDENT is the current indentation
level."
  (cond ((string-match "+" text)	;we have to expand this file
	 (speedbar-change-expand-button-char ?-)
	 (speedbar-with-writable
	   (save-excursion
	     (end-of-line) (forward-char 1)
	     (aspectj-insert-generic-list indent token 'aspectj-tag-expand
					   'aspectj-tag-find))))
	((string-match "-" text)	;we have to contract this node
	 (speedbar-change-expand-button-char ?+)
	 (speedbar-delete-subblock indent))
	(t (error "Ooops...  not sure what to do")))
  (speedbar-center-buffer-smartly))

(defun aspectj-make-tag-line  (exp-button-type
			       exp-button-char exp-button-function
			       exp-button-data
			       tag-button tag-button-function tag-button-data
			       tag-button-face depth)
  "Create a tag line with EXP-BUTTON-TYPE for the small expansion button.
This is the button that expands or contracts a node (if applicable),
and EXP-BUTTON-CHAR the character in it (+, -, ?, etc).  EXP-BUTTON-FUNCTION
is the function to call if it's clicked on.  Button types are
'bracket, 'angle, 'curly, or nil.  EXP-BUTTON-DATA is extra data
attached to the text forming the expansion button.

Next, TAG-BUTTON is the text of the tag.  TAG-BUTTON-FUNCTION is the
function to call if clicked on, and TAG-BUTTON-DATA is the data to
attach to the text field (such a tag positioning, etc).
TAG-BUTTON-FACE is a face used for this type of tag.

Lastly, DEPTH shows the depth of expansion.

This function assumes that the cursor is in the speedbar window at the
position to insert a new item, and that the new item will end with a CR"
  (let ((start (point))
	(end (progn
	       (insert (int-to-string depth) ":")
	       (point)))
	(depthspacesize (* depth speedbar-indentation-width)))
    (put-text-property start end 'invisible t)
    (insert-char ?  depthspacesize nil)
    (put-text-property (- (point) depthspacesize) (point) 'invisible nil)
    (let* ((exp-button (cond ((eq exp-button-type 'bracket) "[%c]")
			     ((eq exp-button-type 'angle) "<%c>")
			     ((eq exp-button-type 'curly) "{%c}")
			     ((eq exp-button-type 'paren)        "(%c)") ;!!!
			     ((eq exp-button-type 'blank)        " %c ") ;!!!
			     ((eq exp-button-type 'vertical-bar) "|%c|") ;!!!
			     (t ">")))
	   (buttxt (format exp-button exp-button-char))
	   (start (point))
	   (end (progn (insert buttxt) (point)))
	   (bf (if exp-button-type 'speedbar-button-face nil))
	   (mf (if exp-button-function 'speedbar-highlight-face nil))
	   )
      (speedbar-make-button start end bf mf exp-button-function exp-button-data)
      (if speedbar-hide-button-brackets-flag
	  (progn
	    (put-text-property start (1+ start) 'invisible t)
	    (put-text-property end (1- end) 'invisible t)))
      )
    (insert-char ?  1 nil)
    (put-text-property (1- (point)) (point) 'invisible nil)
    (let ((start (point))
	  (end (progn (insert tag-button) (point))))
      (insert-char ?\n 1 nil)
      (put-text-property (1- (point)) (point) 'invisible nil)
      (speedbar-make-button start end tag-button-face
			    (if tag-button-function 'speedbar-highlight-face nil)
			    tag-button-function tag-button-data))
    ))

;;;
;;; WGG - the goto-char should have worked, as the current buffer was
;;; reported properly at the call, but...it didn't.
;;;
(defun aspectj-tag-find (text token indent)
  "For the tag TEXT in a file TOKEN, goto that position.
INDENT is the current indentation level."
  (if (consp text)                                           ;!!!
      (aspectj-tag-find (cdr text) (car text) indent)        ;!!! WGG - speedbar-tag-find OK?
  (let ((file (if (markerp token)                            ;!!!
		  (buffer-file-name (marker-buffer token))   ;!!!
		(speedbar-line-path indent))))
    (speedbar-find-file-in-frame file)
    (save-excursion (speedbar-stealthy-updates))
    ;; Reset the timer with a new timeout when cliking a file
    ;; in case the user was navigating directories, we can cancel
    ;; that other timer.
    (speedbar-set-timer dframe-update-speed)
    (aspectj-goto-point-or-mark token) ; !!! WGG - was goto-char
    (run-hooks 'speedbar-visiting-tag-hook)
    (speedbar-maybee-jump-to-attached-frame)
    ))
  )


(defun aspectj-fetch-speedbar-menu (fn)
  "Entry point from speedbar into AspectJ speedbar functionality."
  (if (not (aspectj-mode-in-force)) ; fail out to next option if mode off
      t
    (let ((aspectj-shorten-menu-width (speedbar-frame-width)))
      (aspectj-fetch-dynamic-menu fn))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Interface between aspectj-mode and speedbar, including caching
;;;

;;;
;;; Returns an approximation of an "index-alist", a data structure defined
;;; by imenu.el.  It is not quite one of these beasts, though, due to
;;; extensions to support aj weirdness.
;;;
;;; Returns "t" on failure because that's what speedbar-fetch-dynamic-tags
;;; wants to see (see speedbar.el).
;;;
(defun aspectj-fetch-dynamic-menu (fn)
  (let ((declarations (aspectj-declarations fn)))
    (if declarations
	(aspectj-convert-decls 0 declarations t nil)
      t)))

;;;
;;; The variable aspectj-buffers-declarations holds the most recently
;;; computed declarations, which could have been recomputed by the
;;; jump-menu and annotations stuff.  So these variables aid in caching
;;; the dynamic menu and determining its freshness.
;;;
(defvar aspectj-buffers-last-dynamic-menu nil)
(make-variable-buffer-local 'aspectj-buffers-last-dynamic-menu)

;;; Timestamp of the declarations that created the above.
(defvar aspectj-buffers-last-dynamic-menu-timestamp nil)
(make-variable-buffer-local 'aspectj-buffers-last-dynamic-menu-timestamp)

;;; WARNING / Hackola.  Need to cache for two consumers using different menu
;;; widths (Classes menu and speedbar).  This hack could slow down rebuilding
;;; these menus as they flush each other's results.
(defvar aspectj-buffers-last-dynamic-menu-width 0)

;;;
;;; Provides caching functionality for the above.  We could use the cache
;;; to mask compile failures, but for now we just do straight caching.
;;;
(defadvice aspectj-fetch-dynamic-menu (around aspectj-decls activate)
  (save-excursion (set-buffer (find-file-noselect (ad-get-arg 0)))
    (if (and (aspectj-derived-declarations-cache-fresh
	      fn aspectj-buffers-last-dynamic-menu-timestamp)
	     (eql aspectj-buffers-last-dynamic-menu-width
		  aspectj-shorten-menu-width))
        (setq ad-return-value aspectj-buffers-last-dynamic-menu)
      ad-do-it
      (if (not (eq ad-return-value t)) ; success
	  (progn
	    (setq aspectj-buffers-last-dynamic-menu-timestamp
		  aspectj-declarations-timestamp) ; WARNING - out-of-module ref
	    (setq aspectj-buffers-last-dynamic-menu ad-return-value))
	(setq aspectj-buffers-last-dynamic-menu-timestamp nil)
	(setq aspectj-buffers-last-dynamic-menu-width
	      aspectj-shorten-menu-width)
	(setq aspectj-buffers-last-dynamic-menu nil)))))

;;; For the no-windows jump-menu
(defadvice aspectj-jump-menu-root-dir (around aspectj-jump-menu activate)
  (if (not (string= jde-compile-option-directory ""))
      (setq ad-return-value jde-compile-option-directory)
    ad-do-it))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Support for JDEE Classes pull-down menu.
;;;

(defvar aspectj-saved-imenu-default-goto-function nil)
(make-variable-buffer-local 'aspectj-saved-imenu-default-goto-function)

;;;
;;; 1. imenu-default-goto-function is already buffer-local.
;;; 2. Jump-menu pop-up doesn't use imenu-default-goto-function, so we don't
;;;    have to set it for that.
;;; 3. User has no "option" on jde-imenu-create-index-function option.  The
;;;    way we do this (with a buffer local) slightly misleads the user
;;;    because the options menu says this option has been "set and saved";
;;;    it's not actually saved.
;;; 4. This must be called before JDE mode actually starts.
;;;
(defun aspectj-setup-local-classes-menu-functions ()
  (make-local-variable 'jde-imenu-create-index-function)  ; WARNING - defcustom
  (setq jde-imenu-create-index-function 'aspectj-create-imenu-index)
  (setq aspectj-saved-imenu-default-goto-function imenu-default-goto-function)
  (setq imenu-default-goto-function 'aspectj-imenu-goto-function))

(defun aspectj-clear-local-classes-menu-functions ()
  (setq jde-imenu-create-index-function
	(default-value 'jde-imenu-create-index-function))
  (setq imenu-default-goto-function aspectj-saved-imenu-default-goto-function))

;;; Use marks.  Name and rest ignored.  See imenu-default-goto-function.
(defun aspectj-imenu-goto-function (name position &optional rest)
  (aspectj-goto-point-or-mark position))

(defun aspectj-create-imenu-index ()
  "Creates the AspectJ version of the JDEE Classes toolbar menu.
Defaults to value of jde-imenu-create-index-function if it fails."
  (if (not (aspectj-mode-in-force))
      (aspectj-call-default-jde-create-imenu-index)
    (let* ((aspectj-shorten-menu-width imenu-max-item-length)
	   (aspectj-imenu-include-def jde-imenu-include-classdef)
	   (index-alist (aspectj-speedbar-to-imenu-translate
			 (aspectj-fetch-dynamic-menu (buffer-file-name)))))
	(if (not (eq index-alist t))
	    index-alist
	  ;; no declarations found
	  (aspectj-call-default-jde-create-imenu-index)))))

(defun aspectj-call-default-jde-create-imenu-index ()
  (let ((default-func (default-value 'jde-imenu-create-index-function)))
    (if (not (eq default-func 'aspectj-create-imenu-index)) ; no recursion
	(funcall default-func)
      (message "AspectJ Mode Warning: detected that option jde-imenu-create-index-function's
default value is set to 'aspectj-create-imenu-index.  Please change your
.emacs file to restore JDEE's default setting.")
      (semantic-create-imenu-index))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Following code turns off two undesirable features of imenu.
;;;

;;; Keeps our marks from getting bashed.
;;; WARNING - should cleanup our marks with own function when decls discarded.
(defadvice imenu--cleanup (around aspectj-decls activate)
  (if (not (aspectj-mode-in-force))
      ad-do-it
    (setq ad-return-value nil)))

;;;
;;; This part turns off this "feature" of imenu--split-menu:
;;;
;;;    (while tail                         ; push sublists to top (keep-at-top)
;;;      (if (imenu--subalist-p (car tail))
;;;	  (setq keep-at-top (cons (car tail) keep-at-top)
;;;		menulist (delq (car tail) menulist)))
;;;      (setq tail (cdr tail)))
;;;
;;; We don't want items with submenus treated specially.
;;;

;;; One per buffer, because many calls may be active, apparently.
(defvar aspectj-in-imenu--split-menu nil
  "Track whether inside imenu--split-menu.")
(make-variable-buffer-local 'aspectj-in-imenu--split-menu)

;;;
;;; Maintain flag so imenu--subalistip advice knows when to return nil.
;;; Tried doing this by enabling/disabling advice, but it was overkill.
;;;
(defadvice imenu--split-menu (around aspectj-decls activate)
  (if (not (aspectj-mode-in-force))
      ad-do-it
    (let ((aspectj-in-imenu--split-menu t))
      ad-do-it)))

;;; Returns nil (false) when in aspectj-mode and inside imenu--split-menu.
(defadvice imenu--subalist-p (around aspectj-decls activate)
  (if (or (not (aspectj-mode-in-force)) (not aspectj-in-imenu--split-menu))
      ad-do-it
    (setq ad-return-value nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; JDEE highlighting behavior for AspectJ.  NOTE: this does not use
;;; JDEE fontification customization mechanism.
;;;

;;;
;;; WARNING - hack exploiting internals of XEmacs font-lock.el; don't use for
;;; anything else.  The lists storing information for java-mode and jde-mode
;;; contain symbols referring to these variables, so writing these variables
;;; changes what is retrieved when the symbols are eval'd.  I should probably
;;; be setting the lists directly and not the variables.  So shoot me.
;;;
;;; JDEE 2.2.7 added another level of fontification that gets setup on load.
;;; Since every font level includes all the prior levels, we must oblige.
;;; Arg 1 is "spec".
;;;
(when aspectj-xemacsp
(defadvice font-lock-add-keywords (around aspectj-font-lock activate)
  (when (eq (ad-get-arg 0) 'jde-mode)
    (setq java-font-lock-keywords-4
	  (append (ad-get-arg 1) java-font-lock-keywords-4))))
)

;;; Note that this must be called even for GNU Emacs.
(font-lock-add-keywords 'jde-mode aspectj-keyword-spec)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; beanshell tweaks for JDE extensions.
;;;;
;
;;;;
;;;; At least for AspectJ, beanshell should run in the
;;;; jde-compile-option-directory.  Completion works a lot better there.
;;;; Of course, this doesn't help if you are jumping between projects. :(
;;;;
;
;(defvar aspectj-saved-bsh-startup-directory "")
;(make-variable-buffer-local 'aspectj-saved-bsh-startup-directory)
;
;;;;
;;;; The way we're setting this up (only change if ""), we technically don't
;;;; need to save the value.  I'm setting up the general solution just in
;;;; case we ever drop the empty-string condition.
;;;;
;(defun aspectj-setup-local-bsh-startup-directory ()
;  (when (and (string= "" bsh-startup-directory)
;	(not (string= "" jde-compile-option-directory)))
;    (make-local-variable 'bsh-startup-directory)
;    (setq aspectj-saved-bsh-startup-directory bsh-startup-directory)
;    (setq bsh-startup-directory jde-compile-option-directory)))
;
;(defun aspectj-clear-local-bsh-startup-directory ()
;  (setq bsh-startup-directory aspectj-saved-bsh-startup-directory))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Basic "testing" script.  This just prints out what came from SymbolManager
;;; and what goes out to speedbar and the classes menu.
;;;
;;; NOTE: Overrides a similar script in aspectj-mode.el
;;;

(defun aspectj-test ()
;  (setq debug-on-error t)
 (let* ((filename (buffer-file-name)))
   (aspectj-test-fetch-decls filename)))

(defun aspectj-test-fetch-decls (filename)
 (let* ((decls (aspectj-fetch-declarations filename))
        (aspectj-imenu-include-def jde-imenu-include-classdef)
	(out-buffer (get-buffer-create "*scratch*")))
   (pp decls out-buffer)
   (pp (aspectj-fetch-dynamic-menu filename) out-buffer)
   (pp (aspectj-speedbar-to-imenu-translate
	(aspectj-fetch-dynamic-menu filename)) out-buffer)
   nil))


(provide 'ajdee)
