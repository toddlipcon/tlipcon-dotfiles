;; aspectj-mode --- AspectJ extensions to Java mode

;;;###autoload
(defconst aspectj-mode-version "@build.version.short@"
 "AspectJ Mode version number.")

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
; aspectj-mode.el
;

;;;
;;; Provides a minor mode for AspectJ files that builds on either java-mode
;;; or jde-mode.
;;;

;;; -------------------------------------
;;; Requirements, Installation, and Usage
;;;
;;; See aspectj-mode.html and the accompanying example configuration
;;; files in this distribution for details.
;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Maintenance Commentary
;;;
;;; Beware ye all who enter.  Making AspectJ extensions to java-mode, and
;;; especially JDEE mode, requires breaking several laws of physics.  Here
;;; are some things that make this mode difficult to maintain:
;;; 
;;; 1. Crosscutting.  Navigating around in AspectJ mode cannot use mere
;;;    points, or even intra-file marks.  Marks are required to jump from
;;;    file to file.  When elaborating a buffer with its crosscutting marks,
;;;    this ends up elaborating other buffers (hopefully without building
;;;    their marks recursively).  This ends up inducing effort to manange
;;;    performance (partial elaboration of buffers), and tweaking of things like
;;;    imenu to swallow the marks and *not* destroy them too early.
;;; 
;;; 2. Declarations data management.  The semantic information about AspectJ
;;;    programs is conveyed in a file (at this writing a ".ajesym" file).  All
;;;    navigation is based on the data from these files.  This data is read
;;;    in through an interprocess communication mechanism.  This is potentially
;;;    slow and error-prone.  The information must be updated when the program
;;;    is recompiled, yet it cannot be read too often for performance reasons.
;;;    Hence, there are elaborate mechanisms to check/detect recompilation
;;;    and cache the data once inside the program.  See "Caching Commentary",
;;;    below.
;;; 
;;; 3. Extending two modes, two Emacs-en, on 2+ OS's.  Lots of conditional code
;;;    has been required to make every feature behave well regardless of what
;;;    it's building on.  This has also often required copying code from
;;;    various places (and modifying it).  Some of this code comes from
;;;    actively maintained packages, requiring continual monitoring (get on
;;;    JDEE's announcement list).  This even has to run whether in "windows"
;;;    or "no windows" mode.  Geeeeez.
;;;
;;; 4. Unintrusive extension.  Extending JDEE mode, in particular, without
;;;    modifying or redefining its code has required a significant use of
;;;    advice, hooks, dynamic scoping, and yes, function redefinitions.
;;; 
;;; 5. Toggling.  Getting this mode to behave nicely when turned off requires
;;;    (a) un-bashing global variables and hooks, and (b) and predicating
;;;    many actions with a mode-on check.
;;;
;;; 6. Bugs.  There are bugs in GNU Emacs, XEmacs, and JDEE mode.  Lots of them.
;;;    This results in additional conditionalization, redefinition, use of
;;;    advice, etc., etc.  There are no bugs in AspectJ mode itself.
;;;
;;; It goes without saying that most of these issues pertain to every feature.
;;; Good luck.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ------------------------------
;;; general header

(require 'cl)
(require 'imenu)
(require 'easy-mmode)


;;;
;;; This file provides the package named ``aspectj-mode''.  When compiled,
;;; if it contains macros, it should require itself to be loaded before the
;;; program is compiled, thus the trailing require expression.
;;;
(provide 'aspectj-mode)
(require 'aspectj-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XEmacs can barf on several things in aspectj-mode:
;;;
;;; (1) Overlays:  An adequate impl is provide by XEmacs in the fsf-compat
;;;     package using extents, but may not be installed by default.
;;; (2) Easy-mmode:  Not available in fsf-compat, but is shipped with pcl-cvs,
;;;     which is a standard package.
;;; (3) Font-lock:  Lacks the convenience function font-lock-add-keywords.
;;; (4) Process forking:  bad things can happen when launching java/javaw during
;;;     startup.
;;; (5) Key definitions: Doesn't like strings like GNU Emacs does.
;;;
;;; This flag helps to provide the needed implementations and workarounds.
;;; WARNING: XEmacs should be checked periodically to see if the above have
;;; changed.
;;;
(defvar aspectj-xemacsp (string-match "XEmacs" emacs-version)
"Non-nil if we are running in the XEmacs environment.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This file consists of several subcomponents:
;;;
;;;    - mode-managing code: hooks, hook actions
;;;    - overlay management for storing points-to/by information
;;;    - jump-menu code for displaying points-to/by
;;;    - conversion of SymbolManager data to speedbar data
;;;    - interface to the SymbolManager API provided by ajtools
;;;    - miscellaneous
;;;
;;; The core of aspectj-mode is the management of crosscutting declarations,
;;; which are read from a declarations file (.ajesym).  These decls are used
;;; in the following ways:
;;;  (1) Structure view w/navigation (speedbar)
;;;  (2) Pull-down menu w/navigation (imenu Classes menu)
;;;  (3) Code annotation (shows where 4/5 will work)
;;;  (4) Pop-up contextual menu w/navigation (imenu popup, windowing emacs)
;;;  (5) buffer contextual menu w/navigation (for buffer version of jump menu)
;;;
;;; Completion/insight currently uses the .class file and the reflection
;;; API, not the declarations.  
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Definition of aspectj-mode.

;;; Make this idiom a function.
(defun aspectj-add-hook-locally (hook func)
  (make-local-hook hook)
  (add-hook hook func nil t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mode-setup stuff
;;;
;;; Steps on opening a .java file
;;;
;;; 1. Open the file
;;;
;;; 2. Find the symbols file
;;;    * if error, just fail silently, it might not be aspectj code!
;;;
;;; 3. Check the version number of the table.  If out of date,
;;;    print a message and don't setup links
;;;
;;; 4. setup links.
;;;
;;; 5. Add menu options.
;;;

;;; Make java-mode the default mode for AspectJ source code buffers.
;;;###autoload
(setq auto-mode-alist (append '(("\\.aj\\'" . java-mode)) auto-mode-alist))

(defvar aspectj-mode-map (make-keymap)) ; WGG - GNU Emacs V21 alpha compatibility

(easy-mmode-define-minor-mode aspectj-mode
"Minor-mode extension of Java mode or JDEE mode that provides four services:
visualization and navigation of aspect crosscuts, AspectJ-style compilation
(JDEE mode) and debugging, and AspectJ keyword highlighting.

All commands are available from the AspectJ toolbar menu.  Key commands:
\\[aspectj-jump-menu]\t\tdisplays menu of advisors, advisees, & introductions
M-x aspectj-show-help\tprovides detailed help (aspectj-mode.html)
  M-x aspectj-mode\ttoggles the mode

"
			      nil
			      " AspectJ")

 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Set-up of customizations using defcustom.  See mode management, below.
;;;

;; From custom web page for compatibility between versions of custom
;; with help from ptype@dera.gov.uk (Proto Type) <via dframe.el>
(eval-and-compile
  (condition-case ()
      (require 'custom)
    (error nil))
  (if (and (featurep 'custom) (fboundp 'custom-declare-variable)
           ;; Some XEmacsen w/ custom don't have :set keyword.
           ;; This protects them against custom.
           (fboundp 'custom-initialize-set))
      nil ;; We've got what we needed
    ;; We have the old custom-library, hack around it!
    (if (boundp 'defgroup)
        nil
      (defmacro defgroup (&rest args)
        nil))
    (if (boundp 'defface)
        nil
      (defmacro defface (var values doc &rest args)
        (` (progn
             (defvar (, var) (quote (, var)))
             ;; To make colors for your faces you need to set your .Xdefaults
             ;; or set them up ahead of time in your .emacs file.
             (make-face (, var))
             ))))
    (if (boundp 'defcustom)
        nil
      (defmacro defcustom (var value doc &rest args)
        (` (defvar (, var) (, value) (, doc)))))))


;;;
;;; Sets up customization of AspectJ features.  Most defcustoms appear with
;;; the code they control.
;;;
(defgroup aspectj nil
  "AspectJ Extensions to Emacs Java Development Environments"
  :group 'tools
  :prefix "aspectj-")

;;; Sets up the customization window.
;;;###autoload
(defun aspectj-show-customizations ()
  "Show the AspectJ Mode Customizations panel."
  (interactive)
  (customize-apropos "aspectj" 'groups))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Jumps-and-Annotations "mode" management.
;;;

(defcustom aspectj-show-annotations t
  "*Show source code annotations in editor buffer.
Set this option off if you do not want annotations in the source code showing
the possible effects of aspects on program behavior.  Change of this
option will not be reflected in open buffers."
  :group 'aspectj
  :type 'boolean)

;;;
;;; Want to default to unbound, but buffer-local vars don't work that
;;; way under GNU Emacs.
;;;
(defvar aspectj-buffer-show-annotations 'unbound)
(make-variable-buffer-local 'aspectj-buffer-show-annotations)

(defun aspectj-show-annotations-p ()
  (if (not (eql aspectj-buffer-show-annotations 'unbound))
      aspectj-buffer-show-annotations
      aspectj-show-annotations))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mode management.
;;;
;;; This is complicated by the fact that aspectj-mode must work with both
;;; java-mode and jde-mode.  WARNING - there are little bits of code that
;;; have to check which mode we're in so that certain actions are dis/en-abled.
;;; This is also the sole reason for having the defcustoms above.  
;;; 
;;; A second problem is that turning on/off java-mode or jde-mode should
;;; cause reasonble things to happen for aspectj-mode.  Some of these
;;; things are hard, such as reverting global customizations that have
;;; been modified to accommodate aspectj.
;;;

(defvar aspectj-mode-setup-function 'aspectj-mode-setup-java-mode
  "This variable holds the function that is to be called when AspectJ mode is
turned on.  Nominally, it is a function that does a set-up for either pure
java-mode or jde-mode.  The function is responsible for calling all modes
that it inherits from.")

(defvar aspectj-mode-clear-function 'aspectj-mode-clear-java-mode
  "This variable holds the function that is to be called when AspectJ mode is
turned off.  Nominally, it is a function that does a set-up for either pure
java-mode or jde-mode.  It should mirror what is in
aspectj-mode-setup-function.  The function is responsible for calling all modes
that it inherits from.")

(defun aspectj-mode-setup ()
  (funcall aspectj-mode-setup-function))

(defun aspectj-mode-clear ()
  (funcall aspectj-mode-clear-function))

(add-hook 'java-mode-hook    'aspectj-mode)
(add-hook 'aspectj-mode-hook 'aspectj-mode-toggle)   ; minor modes toggle


;;;
;;; Unlike aspectj-mode, this variable is not buffer-local, hence its
;;; setting pervades all Java buffers.
;;;
(defcustom aspectj-mode-in-force t
  "*When true, overrides native mode features with AspectJ versions."
  :group 'aspectj
  :type 'boolean)

;;; NOTE - almost always call this rather than check aspectj-mode flag
(defun aspectj-mode-in-force ()
  (and aspectj-mode aspectj-mode-in-force))

;;;
;;; NOTE - although we do a "clear" on the current buffer, it is not done
;;; on other buffers visited earlier.  However, there is a -mode-in-force
;;; test on all invasive features.
;;;
(defun aspectj-mode-in-force-toggle ()
  (interactive)
  (customize-set-variable 'aspectj-mode-in-force (not aspectj-mode-in-force))
  (if (aspectj-mode-in-force)
      (aspectj-mode-setup)
    (aspectj-mode-clear)))

;; NOTE - not for direct use, use aspectj-mode-in-force-toggle instead
(defun aspectj-mode-toggle ()
  ;; minor-mode flag is set *before* hooks are run
  (if aspectj-mode
       (aspectj-mode-setup)
    (aspectj-mode-clear)))


;;;
;;; WARNING - font properties are not torn down; toolbar menu just disappears.
;;; The stuff remaining here is raw variables read by other modes.
;;;
(defun aspectj-mode-setup-java-mode ()
  (when (aspectj-mode-in-force)
    (aspectj-check-emacs-version)
    (aspectj-setup-annotations-and-jumps-consistency)
    (aspectj-setup-local-compile-command)
    (aspectj-setup-annotation-truncation)
    (aspectj-setup-menu)
    (aspectj-reset-annotations-and-jumps)
    ))

(defun aspectj-mode-clear-java-mode ()
  ;; nothing for version check
  (aspectj-clear-local-compile-command)
  ;; nothing for annotation-truncation
  ;; nothing for menu
  (aspectj-clear-annotations-and-jumps)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Manages "Compile..." command in the Tools menu on the menubar.
;;;

(defcustom aspectj-tools-compile-command "ajc -emacssym "
  "*Default compile-command accessed from the Emacs Tools menu."
  :group 'aspectj
  :type 'string)

(defvar aspectj-saved-tools-compile-command "")
(make-variable-buffer-local 'aspectj-saved-tools-compile-command)

;;; Doesn't strictly toggle because it doesn't save aspectj command.
(defun aspectj-setup-local-compile-command ()
  (make-local-variable 'compile-command)
  (when (boundp 'compile-command)
    (setq aspectj-saved-tools-compile-command compile-command))
  (setq compile-command aspectj-tools-compile-command))

(defun aspectj-clear-local-compile-command ()
  (setq compile-command aspectj-saved-tools-compile-command))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; AspectJ toolbar menu setup.
;;;
;;; The first several functions here provide support for easily compiling
;;; AJ programs based on .lst files.
;;;

;;;
;;; WARNING - I think the documentation for easy-menu-create-menu says that
;;; the first param can/should be a string, but a symbol is what actually
;;; makes it work.
;;;
(defun aspectj-make-lst-file-menu-list (&optional ignored-menu)
  (let ((menu-item-list
	 (aspectj-make-lst-file-menu-list-1 (aspectj-get-lst-files "./"))))
      (if aspectj-xemacsp
	  menu-item-list
	(easy-menu-filter-return
	 (easy-menu-create-menu 'Compile... menu-item-list)))))

(defvar aspectj-lst-file-list nil
 "List of .lst files found by previous invocations of aspectj-find-lst-files")

(defun aspectj-get-lst-files (dir)
  (setq aspectj-lst-file-list
	(sort (remove-duplicates (append (remove-if-not #'file-exists-p
					  aspectj-lst-file-list)
				  (aspectj-find-lst-files dir))
				 :test #'string=)
	      #'string<))
  aspectj-lst-file-list)

;;; Borrowed and modified from JDEE
(defun aspectj-find-lst-files (dir)
  "Finds a list of .lst files (AspectJ compilation configuration files)
relative to the current buffer."
  (append (directory-files dir t "^.+\.lst$" t)
	  (if (not (aspectj-root-dir-p dir))
	      (aspectj-find-lst-files (concat dir "../"))
	    nil)))

;;;
;;; Borrowed from jde.el unchanged (jde-root-dir-p)
;;; WARNING - check jde-root-dir-p for changes from time to time
;;;
(defun aspectj-root-dir-p (dir)
  (let ((parent (concat dir "../")))
    (cond 
     ((and
       (fboundp 'ange-ftp-ftp-name)
       (ange-ftp-ftp-name dir))
      (ange-ftp-get-file-entry parent))
     ((eq system-type 'windows-nt)
      (not (file-exists-p parent)))
     ((eq system-type 'cygwin32)
      (or (string= (file-truename dir) "/") 
	  (not (file-exists-p (file-truename dir)))))
     (t
      (or (or (not (file-readable-p dir))
	      (not (file-readable-p parent)))
	  (and 
	   (string= (file-truename dir) "/")
	   (string= (file-truename parent) "/")))))))

(defun aspectj-make-lst-file-menu-list-1 (lst-files)
  (let ((menu-list (mapcar #'aspectj-make-lst-file-entry lst-files)))
    (cons (vector
	   "last compile"
	   `(aspectj-compile ,(if (boundp 'compile-command)
				  compile-command
				aspectj-tools-compile-command))
	   t)
	  menu-list)))

(defun aspectj-make-lst-file-entry (lst-file)
  (vector lst-file `(aspectj-compile-with-lst-file ,lst-file)
	  :style 'radio
	  :selected `(aspectj-compile-spec-selected-p ,lst-file)))

;;;
;;; WARNING - because JDEE by default reloads the project file (e.g., prj.el)
;;; when a buffer is first entered/created, the default .lst file can
;;; be reset, cause this to fail to indicate what the previous compile was.
;;;
(defun aspectj-compile-spec-selected-p (lst-file)
  (string= compile-command (aspectj-make-tools-compile-command lst-file)))

(defun aspectj-compile-with-lst-file (lst-file)
  (aspectj-compile (aspectj-make-tools-compile-command lst-file)))

(defun aspectj-compile (command)
  (let ((aspectj-mode-in-force t)) ; coming from menu, pretend AJ mode in force
    (setq compile-command command)
    (call-interactively 'compile)))

(defun aspectj-make-compile-spec (lst-file) (concat "-argfile " lst-file))

(defvar aspectj-argfile-pattern "-argfile[ \t]+[^ \t]+"
  "Regexp pattern for matching an argfile option.")

;;; WARNING - this just does one substitution on the first -argfile found
(defun aspectj-make-tools-compile-command (lst-file)
  (let ((argfile-spec (aspectj-make-compile-spec lst-file)))
    (if (string-match aspectj-argfile-pattern compile-command)
	(aspectj-string-replace-match
	 aspectj-argfile-pattern compile-command argfile-spec)
      ;; make sure compile-command is reasonable before patching in
      (let ((compile-command (if (string-match
				  (concat "^" aspectj-tools-compile-command)
				  compile-command)
				 compile-command
			       aspectj-tools-compile-command)))
	(if (string-match " $" compile-command) ; avoid two spaces in command
	    (concat compile-command argfile-spec)
	  (concat compile-command " " argfile-spec))))))

(defun aspectj-get-menu-definition ()
  "Menu for AspectJ Mode."
  (list "AspectJ"
	;; xemacs knows how to update own menu, GNU's I can't figure out,
	;; so we do it outside of here.
	(cond
	 (aspectj-xemacsp
	  (list* "Compile..."
		 :filter 'aspectj-make-lst-file-menu-list
		 (aspectj-make-lst-file-menu-list-1
		  (aspectj-get-lst-files "./"))))
	 (t
	  (list* "Compile..."
		 (aspectj-make-lst-file-menu-list-1
		  (aspectj-get-lst-files "./")))))
	["Jump menu"  		aspectj-jump-menu
				(aspectj-declarations-loaded)]
	"-"
	["AspectJ mode extensions" (aspectj-mode-in-force-toggle)
				   :style toggle
				   :selected (aspectj-mode-in-force)]
	["Annotations" (if (aspectj-show-annotations-p) 
			   (aspectj-dont-show-annotations)
			 (aspectj-show-annotations))
		       :style toggle
		       :selected (aspectj-show-annotations-p)]
	;["Show annotations"  	aspectj-show-annotations
	;			(not (aspectj-show-annotations-p))]
	;["Don't show annotations"	aspectj-dont-show-annotations
	;			(aspectj-show-annotations-p)]
	;; This menu item shouldn't need to exist; it's kind of like refresh:
	;; it's there just in case there's a bug.  That's why the "enable"
	;; check is so redundant/aggressive, too.
	"-"
	["Update annotations and jumps"	aspectj-reset-annotations-and-jumps 
				(or (aspectj-declarations-loaded)
				    (aspectj-declarations-file-exists-p
				     (buffer-file-name)))]
	["Customize options"	aspectj-show-customizations t]
	["AJ Mode users guide"	aspectj-show-help t]
	"-"
	(concat "AspectJ mode "	aspectj-mode-version)))


(defun aspectj-menu-update ()
  (easy-menu-define aspectj-menu 
		    aspectj-mode-map
		    "Menu for AspectJ Mode."
		    (aspectj-get-menu-definition)))

;;; Define AspectJ mode menu for GNU Emacs and ensure its update
(when (not aspectj-xemacsp)
  (require 'easymenu)
  (aspectj-menu-update))

(defun aspectj-insert-menu-in-XEmacs-menubar ()
  "Insert AspectJ Mode menu in the XEmacs menu bar."
  (if (and (boundp 'current-menubar) current-menubar)
      (if (fboundp 'add-submenu)
	  (add-submenu nil (aspectj-get-menu-definition))
	(add-menu nil "AspectJ" (cdr aspectj-menu-definition)))))

;;; WARNING - what happens if we call this twice for the same buffer?
(defun aspectj-setup-menu ()
  (if aspectj-xemacsp
      (aspectj-insert-menu-in-XEmacs-menubar)
    (aspectj-add-hook-locally 'menu-bar-update-hook 'aspectj-menu-update)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Help functionality.
;;;

(require 'browse-url)


;;; Can be overridden in sub-mode.
(defvar aspectj-helpfile-name "aspectj-mode.html")
(defvar aspectj-helpfile-dir  "aspectj-mode")

;;;
;;; Adapted from jde.el.
;;;
;;;###autoload
(defun aspectj-show-help ()
  "Displays the AspectJ Mode User's Guide in a browser."
  (interactive)
  (let* ((aspectj-dir (aspectj-find-aspectj-directory))
         (aspectj-help
          (when aspectj-dir
             (expand-file-name aspectj-helpfile-name aspectj-dir))))
    (if (and aspectj-help (file-exists-p aspectj-help))
        (browse-url (concat "file://" (aspectj-convert-cygwin-path aspectj-help))
                    browse-url-new-window-p)
      (error "Cannot find AspectJ mode help file: %s" aspectj-help))))

;;;
;;; Adapted from jde.el.
;;;
(defun aspectj-find-aspectj-directory ()
  "Return the path of the directory of the package.
nil if the directory cannot be found."
  (let ((dir (file-name-directory (locate-library aspectj-helpfile-dir))))
    (when dir (nsubstitute ?/ ?\\ dir))
    dir))

;;;
;;; Borrowed from jde.el 2.2.6.  WARNING - The 2.2.7 version can handle
;;; mount paths, but it requires subprocess invocation to invoke cygpath.
;;;
(defun aspectj-convert-cygwin-path (path &optional separator)
  "Convert cygwin style PATH to a form acceptable to java vm.  Basiclally
converts paths of the form: '//C/dir/file' or '/cygdrive/c/dir/file' to
'c:/dir/file'.  This function will not modify standard unix style paths
unless they begin with '//[a-z]/' or '/cygdrive/[a-z]/'."
  (interactive "sPath: ")
  (let* ((path-re "/\\(cygdrive\\)?/\\([a-zA-Z]\\)/")
         (subexpr 2)
         (index1 (* 2 subexpr))
         (index2 (1+ index1)))
    (setq path 
          (if (string-match (concat "^" path-re) path)
              (concat (substring path 
                                 (nth index1 (match-data)) 
                                 (nth index2 (match-data)))
                      ":/" 
                      (substring path (match-end 0)))
            path))
    (if separator
        (while (string-match (concat separator path-re) path)
          (setq path 
                (concat (substring path 0 (match-beginning 0))
                        separator
                        (substring path 
                                 (nth index1 (match-data)) 
                                 (nth index2 (match-data)))
                        ":/" 
                        (substring path (match-end 0))))))
    path))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Version checking.
;;;

;;;
;;; Adapted from cus-edit.el.  This will only handle "." as a number
;;; separator, although a trailing "beta4" or whatever is harmless.
;;; Checks to three levels of numbering, but tolerates a missing third level.
;;; That is, it handles 1.2 and 1.2.3.
;;;
(defun aspectj-version-lessp (version1 version2)
  (let (major1 major2 minor1 minor2 sub1 sub2)
    (string-match "\\([0-9]+\\)[.]\\([0-9]+\\)\\([.]\\([0-9]+\\)\\)?" version1)
    (setq major1 (read (match-string 1 version1)))
    (setq minor1 (read (match-string 2 version1)))
    (setq sub1   (read (or (match-string 3 version1) "0")))
    (string-match "\\([0-9]+\\)[.]\\([0-9]+\\)\\([.]\\([0-9]+\\)\\)?" version2)
    (setq major2 (read (match-string 1 version2)))
    (setq minor2 (read (match-string 2 version2)))
    (setq sub2   (read (or (match-string 3 version2) "0")))
    (or (< major1 major2)
	(and (= major1 major2)
	     (< minor1 minor2))
	(and (= major1 major2)
	     (= minor1 minor2)
	     (<   sub1   sub2)))))

(defun aspectj-check-a-version (the-version min-version)
    (when (aspectj-version-lessp the-version min-version)
	  (error
	   "AspectJ Mode version check error: expected %s or higher, found '%s'"
	   min-version the-version)))

;;;
;;; Should be checked when aspectj-mode is started.
;;;
(defun aspectj-check-emacs-version ()
  (let* ((min-xemacs  (if (eq system-type 'windows-nt)
			  "XEmacs 21.4"    ; process stuff works well
			"XEmacs 21.1")) ; currently standard on Linux
         (min-gnuemacs "GNU Emacs 20.3")
	 (min-emacs (if aspectj-xemacsp min-xemacs min-gnuemacs))
	 (this-emacs (emacs-version)))
    (aspectj-check-a-version this-emacs min-emacs)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functionality for converting our enhanced speedbar menus to generic
;;; imenus.  Used for the JDEE Classes menu and our pop-up jump menu.
;;;
;;; NOTE: this is really misnamed, in that we use these menus for all sorts
;;; of things, not just speedbar.
;;;

(defvar aspectj-imenu-include-def t
 "AspectJ Mode's version of jde-imenu-include-classdef for imenu's.")

;;; Why do they make my life so miserable?
(defun aspectj-goto-point-or-mark (token)
  (when (markerp token) (switch-to-buffer (marker-buffer token)))
  (when (not aspectj-xemacsp) ; WARNING - GNU bug loses way if slow rendering buffer
    (when (markerp token) (switch-to-buffer (marker-buffer token))))
  (goto-char token))

;;;
;;; Remove the buttons on all items, the marks from non-leaf items,
;;; and leaf lists to cons's.
;;;
(defun aspectj-speedbar-to-imenu-translate (in-menu)
  (if (eq in-menu t)
      t
    (mapcar #'aspectj-speedbar-to-imenu-translate-internal in-menu)))

;;;
;;; We have to insert special entries (e.g., *Ship's decl*) for "interior"
;;; declarations because imenu doesn't permit pull-down items to have a buffer
;;; location.  We could leave these out and make folks use the jump menu.
;;;
(defun aspectj-speedbar-to-imenu-translate-internal (lst)
  (cond
   ((not (listp lst))		        ; part of an entry
    lst)
   ((and (< (length lst) 3)             ; a pair (indication mark)
	 (not (consp (cadr lst))))
    (cons (car lst) (cadr lst)))
   ((and (eql (length lst) 3)           ; a triple (indication mark button)
	 (not (consp (cadr lst)))
	 (not (consp (caddr lst))))
    (cons (car lst) (cadr lst)))
   ((and aspectj-imenu-include-def
	 (integer-or-marker-p (cadr lst)))
    (list* (car lst)	                ; > 3 long, interior item, mark
	   (cons (concat "*" (aspectj-get-signature-name (car lst)) "'s decl*")
		 (cadr lst))
	   (mapcar #'aspectj-speedbar-to-imenu-translate-internal
		   (cdddr lst))))
   (t					; > 3 long, interior item, no mark
    (cons (car lst)
	  (mapcar #'aspectj-speedbar-to-imenu-translate-internal
		  (cdddr lst))))))

;;; Advice's don't have a name, but we need something short.
(defun aspectj-get-signature-name (sig)
  (cond
   ((string-match ":" sig)
    "advice")
   ((string-match "(" sig)
    (substring sig 0 (match-beginning 0)))
   (t
    sig)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Annotation and jumps features: put marks on top of the code to show what
;;; aspects are affecting or vice versa.  Menus are available to navigate
;;; base on these effects.
;;;

;(trace-function-background 'aspectj-setup-annotations-and-jumps)
;(trace-function-background 'aspectj-fetch-declarations)
;(trace-function-background 'aspectj-convert-decls)
;(trace-function-background 'aspectj-convert-decl)
;(trace-function-background 'aspectj-add-overlays)
;(trace-function-background 'aspectj-add-overlay)
;(trace-function-background 'aspectj-add-overlay-1)

(defun aspectj-setup-annotations-and-jumps ()
  "Turn on both jumps and text annotations in the buffer."
  (let ((max-specpdl-size    (* max-specpdl-size    10))  ; recursive...
	(max-lisp-eval-depth (* max-lisp-eval-depth 10))  ; ...file visits
	(aspectj-shorten-menu-width imenu-max-item-length))
    (aspectj-clear-annotations-and-jumps)
    (aspectj-add-overlays (aspectj-declarations (buffer-file-name)))))

;;; Only for interactive purposes.  See mode hooks for internal handling.
(defun aspectj-show-annotations ()
  "Turn on text annotations in the buffer."
  (interactive)
  (setq aspectj-buffer-show-annotations t)
  (aspectj-setup-annotations-and-jumps))

(defun aspectj-dont-show-annotations ()
  "Turn off text annotations in the buffer."
  (interactive)
  (setq aspectj-buffer-show-annotations nil)
  (aspectj-clear-annotations))

(defun aspectj-clear-annotations-and-jumps ()
  (aspectj-clear-annotations)
  (aspectj-clear-jumps))


(defun aspectj-reset-annotations-and-jumps ()
  "Recompute the text annotations and jumps of the buffer as appropriate."
  (interactive)
  (aspectj-setup-annotations-and-jumps)
  (when (not (aspectj-show-annotations-p)) (aspectj-clear-annotations))
  t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Declarations Consistency and Caching Commentary.
;;;
;;; AspectJ Mode provides many of its services by reading a ".ajesym" file
;;; that contains information on the declarations in an AspectJ file, which
;;; is based on the program's last compile.  When the program is recompiled,
;;; it is desirable that all views derived from this data be updated.
;;; This is a summary of the derivation structure.  Note that there are many
;;; overlays, each derived from a *portion* of the declarations.  Also note
;;; that this is not the call structure, which is much more complicated.  This
;;; is just the basic data flow.
;;;
;;;   Classes imenu <--	dynamic menu  --> speedbar
;;;			    ^
;;;			    |
;;;			declarations  --> popup imenu --> annotation overlays
;;;			    ^
;;;			    |
;;;			Ship.ajesym (aka "declarations file")
;;;			    ^
;;;			    |
;;;			Ship.java (etc.)
;;;
;;; Keeping this all *consistent* requires detecting that the .ajesym file
;;; has been updated.  There are a couple of hooks/timers in aspectj-mode and
;;; jde-mode for providing this consistency.  This is after-change-functions
;;; which updates the overlays (annotations-and-jumps), and some hooks in
;;; JDEE (and maybe speedbar) that update the Classes menu and the speedbar.
;;;
;;; These hooks are invoked often, as in the case of after-change-functions,
;;; which is basically every buffer change.  Hence it is correct, but takes too
;;; much time to regenerate all the data on each hook invocation.
;;; Consequently, we also need a *caching* functionality.  A key concept in
;;; caching is *freshness* (or staleness) of cached data relative to the data
;;; source.  We track this solely by file timestamps.  (If you dork with the
;;; file dates, you get what you deserve.)  Thus, when a declarations file is
;;; read in, the .ajesym file's timestamp is saved with the declarations.
;;; Future checks for freshness check the file's timestamp with the saved
;;; timestamp; any change implies the data is out of date.  Derived data
;;; can save the declarations timestamp to make its own timestamp.
;;;
;;; Caching is provided in one of two ways.  Internally to aspectj-mode,
;;; we simply provide a guard that evaluates to true when the timestamp
;;; for the derived data is different than the timestamp for the declarations
;;; or declarations file.  Then the derived data is regenerated and a new
;;; derived timestamp is saved.  External hooks aren't so kind, and insist
;;; that we hand over current menu data.  Consequently, we introduce a second
;;; level of caching so that we don't have to rederive the menu on each hook
;;; invocation.
;;;
;;; Consequently, the caching structure looks as follows:
;;;
;;;	    Classes imenu   <-- dynamic menu [timestamp, cache]
;;;				    ^
;;;				    |
;;;	     pop-up imenu   <-- declarations [timestamp, cache]
;;;	 [timestamp, guard]	    ^
;;;				    |
;;;				Ship.ajesym   [file system timestamp]
;;;
;;; Of course, there are guards at the lower levels, too, which redirect
;;; the call to access the cached data rather than regenerate it.  In fact,
;;; we could cache the imenu data for the Classes menu (the
;;; annotations would be too painful, and they are internal to us anyway).
;;; However, this menu appears to be updated relatively infrequently, and
;;; its generation from the dynamic menu seems quite fast.
;;;
;;; In general, caches are implemented with two lisp mechanisms.  One, data
;;; and timestamps are stored in buffer-local variables, since each buffer is
;;; associated 1-1 with an AspectJ file.  Two, caching functionality is
;;; provided with defadvice so that the core functionality isn't littered
;;; with additional complicated logic.  This also permits pulling out the
;;; caching or changing it without messing with the core functionality.
;;;
;;; These caches are complex to maintain.  One problem has been side-effects.
;;; Somewhere in this code there is side-effect that corrupts the cached data,
;;; making it unusable in subsequent requests.  Consequently, we've added
;;; a copy-tree call which copies the data coming out of the cache, keeping
;;; the cache clean.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Overlay consistency/cache management.
;;;

;;;
;;; Note that an idle timer runs only once per idle period, so it has
;;; the effect of running the update check once after each period of
;;; activity.
;;;
(defvar aspectj-annotations-and-jumps-idle-timer nil
 "Timer that runs at idle points to keep buffer info up to date.")

;;;
;;; We're setting up TWO things here.  On the "changes" hook, we consider
;;; whether we need to redraw an annotation.  On the timer, we check whether
;;; the declarations file has been updated and needs refetching.
;;;
;;; WARNING - slight danger here: we're making defcustom hooks local and
;;; setting them.  As I understand the semantics of defcustoms and
;;; make-local-hook, I'm on safe ground.
;;;
;(defvar aj-changes 0)
(defun aspectj-setup-annotations-and-jumps-consistency ()
  (aspectj-add-hook-locally
   'after-change-functions
   'aspectj-maybe-adjust-overlays)
  (when (and aspectj-mode (not aspectj-annotations-and-jumps-idle-timer))
	(setq aspectj-annotations-and-jumps-idle-timer
	      (run-with-idle-timer
	       0.50 t 'aspectj-reset-annotations-and-jumps-on-change))))

;;;
;;; WARNING - advising non-aspectj-mode functions, because no hook available.
;;; WARNING - are these the only buffer-visit funcs that need to be advised?
;;;
;;; The truncation call should be a no-op if the first does an update.
;;;
(defadvice switch-to-buffer (after aspectj-decls activate)
  (when (aspectj-mode-in-force)
    (aspectj-reset-annotations-and-jumps-on-change)
    (aspectj-update-buffers-annotation-truncation)))

(defadvice display-buffer (after aspectj-decls activate)
  (when (aspectj-mode-in-force)
    (aspectj-reset-annotations-and-jumps-on-change)
    (aspectj-update-buffers-annotation-truncation)))


;;; Timestamp of the declarations that created the annotations and jumps.
(defvar aspectj-buffers-last-annotations-timestamp nil)
(make-variable-buffer-local 'aspectj-buffers-last-annotations-timestamp)

;;; Returns t if performed an update, nil if not
;(defvar aj-timer 0)
;(defvar aj-annotes 0)
(defun aspectj-reset-annotations-and-jumps-on-change ()
  "Recomputes the declarations information and resets the overlays if they are
stale, or adjusts an annotation positioning if necessary."
;  (message "Running timer %s" (incf aj-timer))
  (when (and (aspectj-mode-in-force)
	     (not (aspectj-derived-declarations-cache-fresh
		   (buffer-file-name (current-buffer))
		   aspectj-buffers-last-annotations-timestamp)))
;      (message "Updating annotations %s" (incf aj-annotes))
      (aspectj-reset-annotations-and-jumps)
      t))


;;;
;;; Grab the declaration file's timestamp as our own.  Note that we grab
;;; *before* the call so as to be sure we get the earliest possible timestamp.
;;; The other order permits getting a timestamp for a new file, rather than
;;; the current, in which case the new file would go undetected.
;;;
(defadvice aspectj-setup-annotations-and-jumps (around aspectj-decls last activate)
  (setq aspectj-buffers-last-annotations-timestamp
	(aspectj-declarations-file-timestamp (buffer-file-name (current-buffer))))
  ad-do-it)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Stuff for managing performance when we are flying around and visiting
;;; buffers.
;;;

;;;
;;; We prevent a transitive set-up of annotations and jumps because it's costly
;;; if there is a lot of crosscutting.  We count on this function getting
;;; called again when the programmer visits the buffer manually (via timer or
;;; hook).  I'd also like to mask fontification for performance reasons
;;; as well, but haven't figured out how to do that simply without
;;; losing fontification altogether.  Note that this runs first in the
;;; around sequence to make sure that all computations are precluded if
;;; we are in the init sequence (i.e., there is no initialization at all).
;;;
(defvar aspectj-during-jumps-init-depth 0)

;;; Since this function can visit lots of buffers, turn expensive stuff off.
(defadvice aspectj-setup-annotations-and-jumps (around aspectj-init first activate)
  (when (< aspectj-during-jumps-init-depth 1) ; up this value for more init
    (let ((aspectj-during-jumps-init-depth
	   (+ 1 aspectj-during-jumps-init-depth)))
      ad-do-it)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Hook and functions to keep track of the window width so we can re-truncate
;;; annotations.
;;;

(defvar aspectj-buffers-saved-window-width 0)
(make-variable-buffer-local 'aspectj-buffers-saved-window-width)

;;;
;;; See lazy-lock.el for what's up with window-size-change-functions.  It
;;; seems to get called more often than needed, but it's just fine.
;;; window-size-change-functions is "Not currently implemented" in XEmacs 21.1,
;;; but that's OK, because their overlays work fine.
;;;
;;; WARNING - Tried to make this hook local and barely lived to tell the tale.
;;;
(defun aspectj-setup-annotation-truncation ()
  (setq aspectj-buffers-saved-window-width (aspectj-target-window-width))
  (add-hook 'window-size-change-functions
	    'aspectj-update-annotation-truncation))

;;; This checks/updates the truncation for all windows/buffers in a frame
(defun aspectj-update-annotation-truncation (frame)
  (save-window-excursion
    (select-frame frame)
    (walk-windows
     #'(lambda (w)
	 (save-excursion
	   (select-window w)
	   (set-buffer (window-buffer))
	   (aspectj-update-buffers-annotation-truncation)))
     'nomini
     frame)))


;;; This checks/updates the truncation for the current buffer
(defun aspectj-update-buffers-annotation-truncation ()
  (when (and (aspectj-mode-in-force)
	     (aspectj-show-annotations-p)
	     (not (= aspectj-buffers-saved-window-width
		     (aspectj-target-window-width))))
    (setq aspectj-buffers-saved-window-width (aspectj-target-window-width))
    (aspectj-reset-annotations-and-jumps)))

;;;
;;; WARNING - are there other places this bug could come up?
;;;
;;; Because we might be in the speedbar frame.  What we'd like to measure
;;; is the width of the window where the buffer will eventually be shown,
;;; but we don't know where that is.  A window in the "attached frame" is
;;; likely, and we might find it we found a buffer name ending in .java
;;; and assumed its window will be the target.  Sounds complicated, so we
;;; just take any old window (I think the minibuffer window).
;;; WARNING - can't get this to work, so I just return a reasonable
;;; number.  Annotations will update automatically anyway when real number
;;; is discovered via hook activations.
;;;
(defun aspectj-target-window-width ()
  (if (and (boundp 'speedbar-frame)
  	   (eq speedbar-frame (selected-frame)))
      ;(save-window-excursion
      ; (speedbar-select-attached-frame)
      ; (window-width (selected-window)))
      80
    (window-width)))


; WGG - could modify the below to disable the truncation stuff above.
; (defun aspectj-count-open-java-buffers ()
;   "Returns non-nil if any java buffers are open."
;   (count 
;    ".java"
;    (buffer-list)
;    :test
;    (lambda (file-type buffer)
;      (let ((file-name (buffer-file-name buffer)))
;        (if file-name
; 	   (string-match file-type file-name))))))
; 
; (defun aspectj-remove-aspectj-hook ()
;   "Removes `aspectj-reset-annotations-and-jumps-on-change' when
; all Java source buffers have been closed."
;   (unless (> (aspectj-count-open-java-buffers) 1)
;     (remove-hook 'post-command-hook
; 		 'aspectj-reset-annotations-and-jumps-on-change)))
;
; (add-hook 'kill-buffer-hook 'aspectj-remove-aspectj-hook)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Overlay handling proper.
;;;
;;; This code stores points-to and pointed-to-by information in overlays
;;; to permit contextual retrieval (using the current buffer point).
;;;

(when aspectj-xemacsp
  (require 'overlay))


(defun aspectj-add-overlays (decls)
  "Add overlays to the current buffer.  Do nothing else"
  (while decls
    (let ((decl          (car decls)))
      (unless (aspectj-decl-is-introduced-p decl)
	(let* ((startline     (aspectj-decl-begin-line decl))
	       (endline       (aspectj-decl-end-line decl))
	       (kind          (aspectj-decl-kind decl))
	       (points-to     (aspectj-decl-points-to decl))
	       (pointed-to-by (aspectj-decl-pointed-to-by decl))
	       startpoint endpoint)
	  (when (or points-to pointed-to-by)
	    (save-excursion 
	      (goto-line startline)
	      (end-of-line)
	      ;;(insert-char 32 (/ (- (window-width) (current-column)) 2))
	      (setq startpoint (save-excursion 
				 (goto-line startline)
				 (beginning-of-line)
				 (point)))
	      (setq endpoint   (save-excursion
				 (goto-line endline)
				 (end-of-line)
				 (point))))
	    (aspectj-add-overlay startpoint endpoint kind points-to)
	    (aspectj-add-overlay startpoint endpoint kind pointed-to-by))
;	  (if (aspectj-decl-is-type-p decl) ...) - was a guard for below
	  (aspectj-add-overlays (aspectj-decl-declarations decl)))))
    (setq decls (cdr decls))))

;;;
;;; This has got some flexibility so that you can change
;;; the brackets in "annotations" (aka overlays) to have meaning:
;;;
;;;  <Mobility, MoveTracking<    	is advice from there
;;;
;;;  >Point, Line>                      going to there
;;;
;;; WGG - my experience with the above idea is that they blend into the
;;; text too easily.  Maybe a slight alternative would work better.
;;; Coloring the annotations would help, but see aspectj-add-overlay-1
;;; for commentary on that.
;;;
(defun aspectj-add-overlay (start-char end-char kind decls)
  (let ((points-to (remove-if #'aspectj-decl-is-introduced-p decls)))
    (when (and points-to (not (eq kind 'pointcut))) ; pc is refs relation
      (let* ((brackets&generic
	      (cond ((eq kind 'interface)    '("[" "]" "introducer"))
		    ((eq kind 'class)        '("[" "]" "introducer"))
		    ((eq kind 'field)        '("[" "]" "adviser"))
		    ((eq kind 'constructor)  '("[" "]" "adviser"))
		    ((eq kind 'method)       '("[" "]" "adviser"))
		    ((eq kind 'decbodyelement)       '("[" "]" "adviser"))
		    ;;((eq kind 'pointcut)                         )
		    ((eq kind 'introduction) '("[" "]" "introduction"))
		    ((eq kind 'declare)      '("[" "]" "declare"))
		    ((eq kind 'advice)       '("[" "]" "advisee"))
		    (t (message "AspectJ Mode Internal Warning: Unknown kind of declaration: %s" kind)
		                             '("[" "]" "unknown"))))
	     (type-names
	      (sort (remove-duplicates
		     (mapcar 'aspectj-decl-annotation
			     ; (aspectj-filter-unknown-decl-kinds decls)
			     decls)
		     :test 'equal)
		    'string<))
	     (text
	      (aspectj-make-overlay-text
	       start-char end-char type-names brackets&generic)))
	(aspectj-add-overlay-1 start-char end-char points-to text)))))

;;; 
;;; The annotation is supposed to be something "big" like a class or
;;; aspect, not a field or method.  So when it is one of the latter,
;;; grab its declaring type.  If it is a type already, just use it if
;;; it is available, otherwise declaring type.
;;;
(defun aspectj-decl-annotation (decl)
  (if (and (aspectj-decl-is-type-p decl)
	   (not (aspectj-decl-elem-null (aspectj-decl-signature decl))))
      (aspectj-decl-signature decl)
    (aspectj-decl-annotation-typename decl)))

;;;
;;; Adds robustness to getting a name.  WARNING: will fail for inner
;;; anonymous classes containing anonymous classes and the like.
;;;
(defun aspectj-decl-annotation-typename (decl)
  (let ((declaring-type (aspectj-decl-declaring-type decl)))
    (if (not (aspectj-decl-elem-null declaring-type))
	declaring-type
      (progn
	(message "AspectJ Mode Internal Warning: null declaring type for non-class declaration: %s" decl)
	(aspectj-decl-signature decl)))))


;;;
;;; Managing annotation appearance (this and next function)
;;;
;;; WGG - Originally this function clipped the type-names list if it is more
;;; than 2 long.  This really isn't necessary, since the overlays just run
;;; harmlessly off the screen.  If you widen the screen, more of the
;;; overlay is shown.  (Actually, the shortening now happens in
;;; aspectj-add-overlay-1 in a precise but simplified way.)
;;;
(defun aspectj-make-overlay-text (start-char
				  end-char type-names brackets&generic)
  (let ((n (length type-names)))
;    (if (or (= n 0)
;	    (> n 2))			;should really put as many names
;					;as there is room for on the line
;	(format "%s%s %s%s%s" 
;		(car brackets&generic)
;		n (caddr brackets&generic) (if (= n 1) "" "s")
;		(cadr brackets&generic)) ;)
      (format "%s%s%s" 
	      (car brackets&generic)
	      (aspectj-make-overlay-text-1 type-names)
	      (cadr brackets&generic))))

;;;
;;; Recursive function converts names list into a comma-separated string.
;;;  
(defun aspectj-make-overlay-text-1 (names)
  (if (null (cdr names))
      (car names)
    (format "%s, %s"
	    (car names)
	    (aspectj-make-overlay-text-1 (cdr names)))))


;;;
;;; Compute an accurate width for the string, adjusting for tabs, spaces,
;;; and the tab-width (variable).  The way it counts assumes that we the
;;; strings starts (in the buffer) at the 0 mod tab-width position.
;;;
;;; We have to define this because string-width in GNU Emacs is dumb, and
;;; even dumber in XEmacs.
;;;
(defvar aj-tab-char (aref "	" 0))  ; editor independent definition

;;; Geez...fake it.  (Could just be 1 for our purposes)
(when (and aspectj-xemacsp (not (fboundp 'char-width)))
  (defun char-width (c) (if (char-equal c aj-tab-char) tab-width 1)))
  
(defun aspectj-string-width (str)
  (let ((len 0))
    (loop for i from 0 below (length str)
          do (if (char-equal (aref str i) aj-tab-char)
		 (setq len (+ len (- tab-width (mod len tab-width))))
	       (setq len (+ len (char-width (aref str i)))))
	  finally return len)))
    
;;;
;;; This function computes the space available after the end of the line
;;; for a text annotation that won't run off the end of the screen.  This
;;; calculation handles long lines that wrap (the mod calculation).
;;;
;;; This is needed because if it goes off the end of the screen in GNU Emacs,
;;; the cursor positioning gets confused.
;;;
(defun aspectj-compute-fill (bol eol)
  (let* ((linelen (aspectj-string-width
		   (buffer-substring-no-properties bol eol)))
	 (usable-window-width (- (aspectj-target-window-width) 1)) ; last char for wrap char
	 (line-suffix-len (mod linelen usable-window-width))) ; if line wraps
    ;; Why back off 1?  Because sometimes the whole overlay
    ;; disappears without it (Emacs bug?).
    (- usable-window-width line-suffix-len 1)))


;;;
;;; This function is in the process of getting restructured, but part of this
;;; and next routine are responsibile for computing the final annotation
;;; (e.g., right adjusted, truncated).  There are INCREDIBLE machinations
;;; to (a) compute the length of the line, (b) compute the length of the
;;; annotation, and (c) combined multiple annotations on one line.  The last
;;; is the real icing on the cake.
;;;
;;; This function now computes a truncation of the annotation because overlay
;;; after-text behaves funny on GNU Emacs (checked on NTEmacs).  This logic is
;;; complicated, so beware.  We first have to compute the distance from
;;; the end of the line to the end of the window, avail-room.  If there
;;; is plenty of room for the button-text, we add fill to right-justify the
;;; button-text.  Otherwise we truncate the button-text to make it fit.
;;; Doesn't sound bad, but see the weirdnesses commented below.
;;;
;;; We just let it all hang out on XEmacs, since they don't have the hooks
;;; for resized windows (See aspectj-update-annotation-truncation) and it
;;; doesn't have the cursor mispositiong problem anyway.
;;;
;;; BUG WARNING - advisements of implicit constructors have no location, I
;;; *believe*.  We could solve this problem by building the overlays inside
;;; aspectj-convert-decls starting from the root or passing the parent in
;;; another way.  If we fix this, it should probably put an annotation on
;;; the class decl, since the jump will jump to the class decl as a placeholder
;;; for the implicit constructor.
;;; 
;;; The use of marks in the jump menus causes loading of "marked" buffers,
;;; a performance issue unless we're clever (which we are).  See
;;; variable aspectj-during-jumps-init-depth for details.
;;;
(defun aspectj-add-overlay-1 (start-char end-char jumps button-text)
  (let* ((aspectj-decl-show-non-decls t)  ; annotate within methods, etc.
         (existing-annotation-overlay
	  (car (remove-if-not 'aspectj-annotation-p
			       (aspectj-get-overlays start-char)))))
    (when existing-annotation-overlay
	 (setq button-text
	       (concat (overlay-get existing-annotation-overlay 'button)
		       button-text))
	 (save-excursion (delete-overlay existing-annotation-overlay)))
    (let* ((eol (save-excursion (goto-char start-char) (end-of-line) (point)))
	   (bol (save-excursion (goto-char start-char) (beginning-of-line) (point)))
	   (jump-list (aspectj-speedbar-to-imenu-translate
		       (aspectj-converted-decls-sort
			(aspectj-convert-decls 0 jumps nil nil)))))
      ;; WARNING - I want to color the annotation in comment face, but it ends
      ;; up coloring the whole overlay.  I can isolate it to the last
      ;; character on the line (don't overlay the whole line), but
      ;; that's not good enough.  Trying to set the overlay from eol to eol
      ;; fails to generate the coloring at all.  I also tried coloring
      ;; just the after-string text, but that didn't take either.  Stickiness
      ;; didn't help, priorities didn't help, using before-string didn't help.
      ;; Sigh.  It's apparent that the after-string merely inherits the
      ;; face from the character before it, period.  I might have better luck
      ;; with XEmacs, since extents support their form of glyphs.  Bleh.
      ;; 1. (overlay-put eol-overlay 'face font-lock-comment-face)
      ;; 2. (put-text-property 1 (length overlay-text)
      ;; 			     'face font-lock-comment-face overlay-text)
      (aspectj-make-annotation-overlay bol eol button-text)
      (aspectj-make-jumps-overlay start-char end-char jump-list))))

;;;
;;; Here's the idea: There is an insertion or deletion.  Either way, two
;;; lines could be affected.  The line on which point START sits, and the
;;; line on which point END sits.  So update the annotations for each of these.
;;;
(defun aspectj-maybe-adjust-overlays (start end pre-length)
  (when (> (- end start) pre-length)  ; real insertion
    (aspectj-maybe-adjust-jump-overlay-1 start end))
  (aspectj-maybe-adjust-annotation-overlay-1 start)
  (when (not (= start end))
    (aspectj-maybe-adjust-annotation-overlay-1 end)))

;;;
;;; Annotations are line based, so grab the whole line.  If it is an empty
;;; line, we have to do a +1 thing to actually get the line for some reason.
;;; The safest way to update the annotation is to delete and reinsert, so we
;;; always do that.
;;;
(defun aspectj-maybe-adjust-annotation-overlay-1 (pt)
  (let* ((bol (save-excursion (goto-char pt) (beginning-of-line) (point)))
  	 (eol (save-excursion (goto-char pt) (end-of-line) (point)))
	 (eol+1 (if (= bol eol) (+ eol 1) eol))
	 (existing-annotation-overlays
	  (remove-if-not 'aspectj-annotation-p
			 (aspectj-get-overlays (list bol eol+1)))))
    ;(message "bol: %s eol %s" bol eol)
    ;(message "aj overlays %s" existing-annotation-overlays)
    (when existing-annotation-overlays
      (let* ((button-text (mapconcat '(lambda (a-o) (overlay-get a-o 'button))
				     existing-annotation-overlays "")))
	(save-excursion (mapcar 'delete-overlay existing-annotation-overlays))
	(aspectj-make-annotation-overlay bol eol button-text)))))

;;;
;;; WARNING - This assumes that we delete/insert forward, as any inserted code
;;; is examined with respect to the code preceding it.
;;;
(defun aspectj-maybe-adjust-jump-overlay-1 (start end)
  (let* ((existing-jumps-overlays
	  (remove-if-not 'aspectj-jumps
			 (aspectj-get-overlays (list start (+ start 1))))))
    (message "start: %s end %s" start end)
    (message "aj overlays %s" existing-jumps-overlays)
    (while existing-jumps-overlays
      (let* ((existing-jumps-overlay (car existing-jumps-overlays))
	     (o-start (overlay-start existing-jumps-overlay))
	     (o-jumps (overlay-get existing-jumps-overlay 'aspectj-jumps)))
        (setq existing-jumps-overlays (cdr existing-jumps-overlays))
	(when (> end (overlay-end existing-jumps-overlay)) ; lengthened
	  (save-excursion (delete-overlay existing-jumps-overlay))
	  (aspectj-make-jumps-overlay o-start end o-jumps))))))

(defun aspectj-make-annotation-overlay (bol eol button-text)
  (let ((eol-overlay (make-overlay bol eol))
        (filled-text (aspectj-annotation-text bol eol button-text)))
    (overlay-put eol-overlay 'after-string filled-text)
    (overlay-put eol-overlay 'aspectj-annotation-p t)
    (overlay-put eol-overlay 'button button-text)))

	;; WARNING - should be able to do this w/o building new string.
;	(when (not (string= new-text annotation-text)) ...)

(defun aspectj-make-jumps-overlay (start-char end-char jump-list)
  (overlay-put (make-overlay start-char end-char) 'aspectj-jumps jump-list))

(defun aspectj-annotation-text (bol eol button-text)
  (let* ((avail-room (- (aspectj-compute-fill bol eol) (length button-text)))
	 (remaining-fill (max 1 avail-room))   ; fill must be positive
	 (ntruncate                            ; pos, -1 counters min rfill
	  (if aspectj-xemacsp 0 (max 0 (- (- avail-room 1)))))
	 (button-len (max 0 (- (length button-text) ntruncate)))) ; ditto
    (concat (make-string remaining-fill ?\ )
	    (aspectj-substring button-text button-len))))

(defun aspectj-clear-annotations ()
  "Clear the annotations from buffer."
  (let ((overlays (aspectj-get-overlays t)))
    (aspectj-clear-overlays 
      (remove-if-not #'(lambda (o) (aspectj-annotation-p o)) overlays))))

(defun aspectj-clear-jumps ()
  (let ((overlays (aspectj-get-overlays t)))
    (aspectj-clear-overlays 
      (remove-if-not #'(lambda (o) (aspectj-jumps o))        overlays))))

(defun aspectj-clear-overlays (overlays)
  (while overlays
    (delete-overlay (car overlays))
    (setq overlays (cdr overlays)))
  (recenter))  ; WGG - was call to gud-refresh, which calls recenter
  
(defun aspectj-get-jumps-at ()
  (apply 'append
	 (remove nil (mapcar 'aspectj-jumps (aspectj-get-overlays (point))))))

(defun aspectj-get-all-jumps ()
  (apply 'append
	 (remove nil (mapcar 'aspectj-jumps (aspectj-get-overlays t)))))

(defun aspectj-get-annotations-at ()
  (remove-if-not 'aspectj-annotation-p  (aspectj-get-overlays (point))))

(defun aspectj-get-all-annotations ()
  (remove-if-not 'aspectj-annotation-p  (aspectj-get-overlays t)))

(defun aspectj-get-overlays (point-or-t)
  (cond
   ((eq point-or-t t)
    (overlays-in (save-excursion (beginning-of-buffer) (point))
		 (save-excursion (end-of-buffer) (point))))
   ((listp point-or-t)
    (overlays-in (car point-or-t) (cadr point-or-t)))
   (t
    (overlays-at point-or-t))))

(defun aspectj-annotation-p (overlay)
  (overlay-get overlay 'aspectj-annotation-p))

(defun aspectj-jumps (overlay)
  (overlay-get overlay 'aspectj-jumps))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Menu helper(s).
;;;
;;; Used to shorten menu items and annotations when too long to fit
;;; comfortably (i.e., when would push other stuff off the screen).
;;;

(defun aspectj-substring (string maxlen)
  (cond
   ((<= (length string) maxlen)			; already short enough
    string)
   ((>= maxlen 4)				; too long, room for marker
    (concat (substring string 0 (- maxlen 2)) ".."))
   (t						; too long, no room for marker
    (substring string 0 maxlen))))

;;;
;;; Not a parameter because source of width and final call are far apart.
;;; Change this value by binding it as a local in caller to aspectj-shorten.
;;;
(defvar aspectj-shorten-menu-width 35
 "Use dynamic scoping to override this for aspectj-shorten")

;;; Threshold length of 5 avoids over-elision.  
(defun aspectj-shorten (depth string fraction)
  (let ((maxlen
	 (max 5 (- (floor (* fraction aspectj-shorten-menu-width)) depth 1))))
    (aspectj-substring string maxlen)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Jump Menu functions.
;;;
;;; The Jump Menu displays the points-tos and pointed-to-bys relevant to the
;;; program point current when the Jump Menu is activated.  Activating items
;;; in the list "jumps" you to the selected item to see the nature of the
;;; points-to/by.
;;;
;;; NOTE: the current "jumping" function does not split the window, it
;;; just switches the buffer.  We could do it like the classic version,
;;; which splits the window (and doesn't move the cursor to the new buffer).
;;;

(defcustom aspectj-classic-jump-menu t
  "*When true, the jump menu is displayed as a buffer rather than a pop up.
This is the default when Emacs runs in the no-windows configuration."
  :group 'aspectj
  :type 'boolean)

;;; WARNING BUG - This binding doesn't undo itself when the mode is off.
(defvar aspectj-control-j (if aspectj-xemacsp '(control j) "\C-j"))
(define-key ctl-x-map aspectj-control-j 'aspectj-jump-menu)


;;;
;;; WARNING - imenu--mouse-menu doesn't like one-item menus at the top-level,
;;; so we create an empty second entry when we have to.  Other tricks didn't
;;; work and were complicated, anyway.  Maybe worth a second try.
;;;
(defun aspectj-imenu-one-item-correction (index-alist)
  (if (= 1 (length index-alist))
      ;(list (cons 'filler-for-imenu index-alist))
      (append index-alist (list '("" . nil)))
    index-alist))


(defun aspectj-popup-jump-menu (index-alist &optional title)
  "Builds a popup menu which displays all of the possible jumps for the
object it was invoked on."
    (let* ((aspectj-imenu-include-def nil) ; we're already at the decl
	   (title (or title "Crosscut decls")))
      (setq index-alist (aspectj-imenu-one-item-correction index-alist))
      (let ((selection (cdr (imenu--mouse-menu
			     index-alist
			     (aspectj-modified-mouse-posn)
			     title))))
	(cond
	  ((null selection)
	   nil)
	  ((not (aspectj-point-or-mark-unmapped selection))
	   ;; implements "navigation" similar to classic buffer version
	   (switch-to-buffer-other-window (marker-buffer selection))
	   (aspectj-goto-point-or-mark selection)
;	   (recenter) ; WARNING - appears to fix GNU scroll-to-point bug
;	   (switch-to-buffer-other-window (window-buffer (get-lru-window)))
	   )
	  (t
	   (message "AspectJ Mode Internal Warning: Declaration unmapped."))))))

;      (if aspectj-xemacsp
;	  (let ((menu (cons title
;			    (mapcar
;			     #'(lambda (item)
;				 (vector (car item)
;					 (list 'aspectj-goto-point-or-mark
;					       (cdr item))))
;			     index-alist))))
;	    (popup-menu-and-execute-in-window menu (selected-window)))
;)

(if (not aspectj-xemacsp)
;;; From avoid.el
(defun aspectj-point-position ()
  "Return the position of point as (FRAME X . Y).
Analogous to mouse-position."
  (let* ((w (selected-window))
	 (edges (window-edges w))
	 (list 
	  (compute-motion (max (window-start w) (point-min))   ; start pos
			  ;; window-start can be < point-min if the
			  ;; latter has changed since the last redisplay 
			  '(0 . 0)	                       ; start XY
			  (point)	                       ; stop pos
			  (cons (window-width) (window-height)); stop XY: none
			  (1- (window-width))                  ; width
			  (cons (window-hscroll w) 0)          ; 0 may not be right?
			  (selected-window))))
    ;; compute-motion returns (pos HPOS VPOS prevhpos contin)
    ;; we want:               (frame hpos . vpos)
    (cons (selected-frame)
	  (cons (+ (car edges)       (car (cdr list)))
		(+ (car (cdr edges)) (car (cdr (cdr list))))))))

;;; from avoid.el (XEmacs distribution)
(defun aspectj-point-position ()
  "Returns (WINDOW X . Y) of current point - analogous to mouse-position"
  (let* ((beg (window-start))
         (pos (point))
         (col (current-column))
         (row))
    (setq row (count-lines beg pos))
    (cons (selected-window) (cons col row))))
) ; if (not aspectj-xemacsp)

;;; From avoid.el
(defun aspectj-set-mouse-position (pos)
  ;; Carefully set mouse position to given position (X . Y)
  ;; Ideally, should check if X,Y is in the current frame, and if not,
  ;; leave the mouse where it was.  However, this is currently
  ;; difficult to do, so we just raise the frame to avoid frame switches.
  ;; Returns t if it moved the mouse.
  (let ((f (selected-frame)))
    (raise-frame f)
    (when aspectj-xemacsp
      (setq f (frame-root-window f))) ; WGG BUG FIX - see aspectj-mouse-posn
      				      ; should be frame-selected-window
    (set-mouse-position f (car pos) (cdr pos))
    t))

;;;
;;; Compute a new mouse (and menu) position that will yield a likely
;;; satisfactory position for the jump-menu.  We want the menu to be
;;; near the point without obscuring the cursor, code, or annotation.
;;;
;;; WARNING - closely tuned to the odd observed behaviors of GNU Emacs
;;; and XEmacs.
;;;
(defun aspectj-set-mouse-position-near-point ()
  (let* ((x-inc (if aspectj-xemacsp  5 20))
  	 (x-dec (if aspectj-xemacsp 20 20))
  	 (y-inc (if aspectj-xemacsp  5  2))
  	 (point-position (cdr (aspectj-point-position)))
	 ;(junk (message "cursor position x %s y %s" (car point-position) (cdr point-position)))
	 (x-plus  (+ (car point-position) x-inc))
	 (x-minus (max (- (car point-position) x-dec) 0))
	 (x (if (< x-plus  (frame-width)) x-plus x-minus))
	 (y-plus  (+ (cdr point-position) y-inc))
	 (y-minus y-plus) ; WARNING - "long" y is auto-adjusted.
	 (y (if (< y-plus (frame-height)) y-plus y-minus))
  	 (best-position (cons x y)))
    ;(message "computed mouse position x %s y %s" x y)
    (aspectj-set-mouse-position best-position)))

;;;
;;; Routine that sets the mouse position near the current point
;;; and then sets up for the menu to be popped-up properly.
;;;
(defun aspectj-modified-mouse-posn ()
  (aspectj-set-mouse-position-near-point)
  (aspectj-mouse-posn))

;;;
;;; Borrowed from JDE, but substitutes near-0's, moves mouse when outside pane.
;;;
;;; WGG XEmacs BUG - mouse-pixel-position is computed for the selected
;;; window.  The button-press event is computed from the root window.
;;; We fix this above by warping the mouse into the root window instead
;;; of the selected window. :(
;;;
(defun aspectj-mouse-posn ()
  "Returns t for GNU Emacs and a mouse position event on XEmacs, so that
the mouse menu on XEmacs will pop-up near the mouse."
  (if aspectj-xemacsp
      (let* ((mouse-pos (mouse-pixel-position))
             (x (or (car (cdr mouse-pos)) 20))
             (y (or (cdr (cdr mouse-pos)) 20)))
	(when (null (car (cdr mouse-pos))) ; move mouse into position if nec.
	  (set-mouse-pixel-position
	   (get-buffer-window (current-buffer) nil) x y))
	;(message "computed mouse pixel position x %s y %s" x y)
        (make-event 'button-press `(button 1 modifiers nil x ,x y ,y)))
    t))

(defun aspectj-jumps-at-point-imenu (jump-list)
  "Displays the pertinent advice and introductions for the decl, or vice versa,
at point.  Selecting an item results in navigating to that item's declaration."
  (when jump-list
    (aspectj-popup-jump-menu jump-list)))

(defun aspectj-jump-menu ()
  "Make a menu of jumps to/from selected code."
  (interactive)
  (let ((jumps (aspectj-get-jumps-at)))
    ;; If a target buffer got killed, the marks are bogus, so recompute
    (when (member-if #'(lambda (decl) (eq (marker-buffer (cdr decl)) nil))
		     jumps)
      (aspectj-reset-annotations-and-jumps) ; WARNING - for popup, buffer redraw may not complete in time
      (setq jumps (aspectj-get-jumps-at)))
    (cond
     (jumps
      (if (and window-system (not aspectj-classic-jump-menu))
	  (aspectj-jumps-at-point-imenu jumps)
	(aspectj-jump-menu-buffer jumps)))
     (t
      (if (aspectj-declarations-loaded)
	  (message "No crosscut elements at point.")
	(message "No AspectJ declarations file for current buffer %s." (buffer-name)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; These are for the "classic" jump menu.  Otherwise, we do pop-ups.
;;; This puts the info in a special buffer and splits the window.
;;;


(defun aspectj-jump-menu-buffer (jumps)
  (let ((buffer (aspectj-list-jumps-noselect jumps)))
    (switch-to-buffer-other-window buffer)
    (let* ((start (progn (beginning-of-buffer) (point)))
	   (end   (progn (end-of-buffer) (point)))
	   (nlines (count-lines start end)))
      (enlarge-window (+ 2 (- nlines (window-height))))
      (goto-line 3)  ; moves point to first entry after header
      (recenter)     ; moves cursor to point and recenters
      (message "Commands: q to quit; n or p to move; SPC or x to jump; ? for help."))))

;;; ".." means "parent directory"
(defun aspectj-jump-menu-root-dir () "..")

(defun aspectj-list-jumps-noselect (jumps)
  "Create and return a buffer with a list ..."
  (let ((old-buffer (current-buffer))
	(standard-output standard-output))
    (save-excursion
      (set-buffer (get-buffer-create "*AspectJ Jumps*"))
      (setq buffer-read-only nil)
      (erase-buffer)
      (setq standard-output (current-buffer))
      (princ " Signature                                        Filename\n ---------                                        --------\n")
      (while jumps
	(let* ((decl       (car jumps)) ; decl is a cons
	       (filename   (buffer-file-name (marker-buffer (cdr decl))))
	       (signature  (car decl))
	       (midline    50) ; leave 30 characters for relative filename
	       line-start
	       line-end)
	  (indent-to 1)
	  (setq line-start (point))
	  (princ (aspectj-substring signature (- midline 4)))
	  (indent-to midline 2)
	  ;; keep jump-menu entries short
	  (princ (file-relative-name filename (aspectj-jump-menu-root-dir)))
	  (setq line-end (point))
	  (put-text-property line-start line-end 'jump decl)
	  (put-text-property line-start line-end 'mouse-face 'highlight)
	  (princ "\n")
	  (setq jumps (cdr jumps))))	
      (aspectj-jump-menu-mode)
      (current-buffer))))

(defvar aspectj-jump-menu-mode-map nil "")

(when (not aspectj-jump-menu-mode-map)
  (setq aspectj-jump-menu-mode-map (make-keymap))
  (suppress-keymap aspectj-jump-menu-mode-map t)
  (define-key aspectj-jump-menu-mode-map "q" 'aspectj-jump-menu-quit)
  (define-key aspectj-jump-menu-mode-map "x" 'aspectj-jump-menu-select)
  (define-key aspectj-jump-menu-mode-map " " 'aspectj-jump-menu-select)
  (define-key aspectj-jump-menu-mode-map "n" 'next-line)
  (define-key aspectj-jump-menu-mode-map "p" 'previous-line)
  (define-key aspectj-jump-menu-mode-map "?" 'describe-mode)
  (define-key aspectj-jump-menu-mode-map [mouse-2] 'aspectj-jump-menu-mouse-select)
)

;; AspectJ Jump mode is suitable only for specially formatted data.
(put 'aspectj-jump-menu-mode 'mode-class 'special)

(defun aspectj-jump-menu-mode ()
  "Major mode for viewing advice, introductions, or their targets that
   apply to selected code in source window.  All keystrokes are commands.
\\{aspectj-jump-menu-mode-map}"

  (kill-all-local-variables)
  (use-local-map aspectj-jump-menu-mode-map)
  (setq major-mode 'aspectj-jump-menu-mode)
  (setq mode-name "AspectJ Jump Menu")
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (run-hooks 'aspectj-jump-menu-mode-hook))

(defun aspectj-jump-menu-quit ()
  "Quit the jump menu."
  (interactive)
  (switch-to-buffer (window-buffer (get-lru-window)))
  (bury-buffer (other-buffer))
  (delete-other-windows))

(defun aspectj-jump-menu-select ()
  "Visit entry under the cursor in the buffer menu."
  (interactive)
  (aspectj-jump-menu-select-internal (current-buffer)
				     (point)))

;;; Aha!  Two useful functions that GNU Emacs lacks.
(when (not aspectj-xemacsp)
  (defun event-window (event) (posn-window (event-end event)))
  (defun event-point  (event) (posn-point  (event-end event)))
)

(defun aspectj-jump-menu-mouse-select (event)
  "Visit entry under mouse cursor in the buffer menu."
  (interactive "e")  
  (let ((window (event-window event)))
    (select-window window)
    (aspectj-jump-menu-select-internal (window-buffer window)
				       (event-point event))))

(defun aspectj-jump-menu-select-internal (buffer char)
  (let* ((decl       (aspectj-jump-menu-lookup-jump buffer char))
	 ;(filename   (aspectj-decl-filename decl))
	 ;(start-line (aspectj-decl-begin-line decl))
	 (back-to    (get-lru-window)))
    (aspectj-goto-point-or-mark (cdr decl))
    ;(find-file filename)
    ;(goto-line start-line)
    (recenter 0)
    (kill-buffer buffer)
    (select-window back-to)
    (balance-windows)))

(defun aspectj-jump-menu-lookup-jump (buffer char)
  "Return jump (an internal structure) described by this line of buffer menu."
  (save-excursion 
    (switch-to-buffer buffer)
    (goto-char char)
    (beginning-of-line)
    (and (not (eobp)) (get-text-property (+ (point) 2) 'jump))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Creates speedbar-like data from lisp-ified SymbolManager data.
;;;
;;; We use this data in many places (annotations, jumps, JDE classes menu),
;;; including speedbar.
;;;
;;; we need to produce what speedbar calls a "generic list"
;;; (... nil
;;;      ("foo" . 32)
;;;      ("foo" 32 <button> . ...)
;;;      ("foo" . ...)
;;;
;;; <-> spacewar/
;;;  [-] Ship.java
;;;   (-) Ship
;;;    (+) MyInnerClass
;;;        getX()
;;;     +  getY()
;;;     -  getZ()
;;;         Debug: static before
;;;
;;;    |+| paint(..)
;;;
;;; 
;;;    |-| paint(..)
;;;         introduction FooCut
;;;
;;;
;;;     +  introduction Game | ..
;;;         Game 
;;;         Player 
;;;         Bozo
;;;
;;;     + static before ...
;;;
;;;     - static before ...
;;;         Frotz: doit()
;;;
;;;   (+) GameCoordinator
;;;
;;; The "brackets" say what kind of thing is on this line, NOT what kind of 
;;; thing is underneath it.  The presence of +/- means you can expand the
;;; thing to get what's under it.
;;;

(defun aspectj-convert-decls (depth decls recurse parent-decl)
  (mapcar #'(lambda (decl)
	      (aspectj-convert-decl (+ depth 1) decl recurse parent-decl)) 
	  (aspectj-sort-decls (aspectj-conditionally-filter-non-decl-kinds
	  		       ;(aspectj-filter-unknown-decl-kinds decls)
			       decls))))

;;;
;;; This is the workhorse of menu creation.
;;;
(defun aspectj-convert-decl (depth decl recurse parent-decl)
  (let* ((fn             (aspectj-decl-filename       decl))
	 (begin-line     (aspectj-decl-begin-line     decl))
	 (kind           (aspectj-decl-kind           decl))
	 (sig            (aspectj-decl-signature      decl))
	 (declaring-type (aspectj-decl-declaring-type decl))
	 (pt             (aspectj-decl-points-to      decl))
	 (ptb            (aspectj-decl-pointed-to-by  decl))
	 (decls          (aspectj-decl-declarations   decl))

	 (button-type    (aspectj-decl-button-type decl))
	 (mark           (if (and (aspectj-unmapped fn begin-line) parent-decl)
			     (aspectj-mark  ; then mark from parent
			      (aspectj-decl-filename parent-decl)
			      (aspectj-decl-begin-line parent-decl))
			   (aspectj-mark fn begin-line)))
	 (qualified-sig
	  (if recurse
	      sig
	    (aspectj-elaborated-signature declaring-type sig kind depth))))

    ;;; WGG - Want introduction's to show their decls and to whom they
    ;;; introduce, but only at the actual declaration of introduction,
    ;;; not at each site it affects (hence recurse flag test).
    ;;; WGG - The recurse decision is subject to revision; programmer may
    ;;; want to know what the decl of introduced object is; however, JDEE
    ;;; does not show this information for normal field/method decls.
    (if (and (eq kind 'introduction) recurse)
;	(if (string-match "^introduction" qualified-sig) ; WARNING - old intro
;	    (list qualified-sig mark button-type
;		  (list* "Targets"
;			 (aspectj-convert-introduction-targets pt))
;		  (list* "Decls"   ; WGG - this was when intro had many decls
;			 (append (aspectj-convert-decls
;				  (1+ depth) decls recurse decl))))
	  (list* qualified-sig mark button-type	      
		 (aspectj-convert-introduction-targets pt))
;)
      (list* qualified-sig
	     mark
	     button-type
	     (and (and recurse (or pt ptb decls))
		  ;; WGG - sort only pt/ptb, as they have no predefined order
		  ;;  that programmer may expect items to be in, but decls do.
		  (append
		   (let ((aspectj-decl-show-non-decls t)) ; pt = advice target
		     (aspectj-converted-decls-sort
		      (aspectj-convert-decls (1+ depth) pt    nil nil)))
		   (aspectj-converted-decls-sort
		    (aspectj-convert-decls (1+ depth) ptb   nil nil))
		   (aspectj-convert-decls (1+ depth) decls t decl)))))))

(defun aspectj-elaborated-signature (declaring-type sig kind depth)
  (cond
   ((aspectj-decl-elem-null declaring-type)
    sig)
   ((or (eq kind 'advice) (eq kind 'introduction))
    (concat (aspectj-shorten depth declaring-type .5) ": " sig))
   ((eq kind 'decbodyelement)
    ;; Don't do "Foo." here because it is not a decl, but code.
    ;; Don't do "Foo: " because it's not an introducing/advising thing.
    (concat sig " in " declaring-type))
   (t
    ;; Can't do "..." shortening here because it's ambiguous with "."
    ;; Although could shorten *within* name, like "SpaceO..t".
    (concat declaring-type "." sig))))

(defun aspectj-convert-introduction-targets (pts)
    (mapcar #'(lambda (pt)
		(let* ((name (aspectj-decl-signature pt))
		       (file (aspectj-decl-filename pt))
		       (line (aspectj-decl-begin-line pt)))
		  (list name (aspectj-mark file line) 'blank)))
	    pts))


;;;
;;; Decls are sorted in a few places on a few different criteria.
;;; Seems redundant, only certain portions are getting resorted.
;;;
;;; Lists of advice points-tos can be quite long, especially in debugging,
;;;  so sorting helps to get things under control.  WGG
;;; 
(defun aspectj-converted-decls-sort (cdecls)
 (sort cdecls #'(lambda (cdecl1 cdecl2) (string< (car cdecl1) (car cdecl2)))))


(defvar *aspectj-decls-order* '(interface 
				class
				aspect
				field        ;introduced-field
				initializer  ;introduced-initializer
				constructor  ;introduced-constructor
				method       ;introduced-method
				pointcut
				introduction 
				advice))

(defun aspectj-sort-decls (decls)    
  (sort decls 
	#'(lambda (a b)
	    (member (aspectj-decl-kind b)
		    (cdr (member (aspectj-decl-kind a) 
				 *aspectj-decls-order*))))))


;;;
;;; Distinguishes between menus like speedbar and annotations directly
;;; in the code.  The latter can handle things referring to code.
;;;
(defvar aspectj-decl-show-non-decls nil)

(defun aspectj-conditionally-filter-non-decl-kinds (decls)
  (if aspectj-decl-show-non-decls
      decls
    (remove-if #'(lambda (d) (eq (aspectj-decl-kind d) 'decbodyelement))
	       decls)))

;;;
;;; WARNING - This may be getting called annoyingly low in call tree, because
;;; decls are nested.
;;;
(defun aspectj-filter-unknown-decl-kinds (decls)
  (remove-if-not #'aspectj-is-known-decl-kind decls))

;;;
;;; Note that function below also enumerates decl kinds.
;;; NOTE: we could decide that certain types are *known* but not shown in
;;; a browser (i.e., still shown as annotations). decbodyelement is one
;;; such possible kind, as it is not a decl per se.
;;;
(defun aspectj-is-known-decl-kind (decl)
  (let ((kind (aspectj-decl-kind decl)))
    (case kind 
      ((aspect interface class field initializer constructor method
        pointcut introduction advice decbodyelement declare)
       t)
      (otherwise
       (message "AspectJ Mode Internal Warning: unknown decl kind - %s" kind)
       nil))))
      

;;; NOTE - not expecting to display decbodyelements
(defun aspectj-decl-button-type (decl)
  (let ((kind (aspectj-decl-kind decl)))
    (case kind 
      ((aspect interface class) 'paren)
      ((field initializer constructor method)
       (if (aspectj-decl-is-introduced-p decl) 'vertical-bar 'blank))
      ((pointcut introduction advice decbodyelement declare) 'blank)
      (otherwise
       (message "AspectJ Mode Internal Warning: Unknown declaration type - %s" kind)
       'blank)))) ; WGG WARNING - adds robustness for language changes
  
;;;
;;; This used to be aspectj-point-or-mark, but I chose to return all markers
;;; because they are more robust with respect to editing (WGG).
;;;
(defun aspectj-mark (fn line-number)
  (save-excursion
    (let ((other-buffer (find-file-noselect fn t)))
      (unless (eq other-buffer (current-buffer)) (set-buffer other-buffer))
      (save-excursion ; this save-excursion is for the other-buffer case
	(goto-line line-number)
	(point-marker)))))
   
;;;
;;; Convention is that if line-number is less than 1, it's bogus.
;;; See SymbolManager for details.
;;;
(defun aspectj-unmapped (fn line-number)  (< line-number 1))

(defun aspectj-point-or-mark-unmapped (token)
  (if (markerp token)
      (aspectj-unmapped nil (marker-position token))
    (aspectj-unmapped nil token)))

(defun line->bol (line)
  (save-excursion (goto-line line)  (beginning-of-line) (point)))

(defun point->bol (point)  
  (save-excursion (goto-char point) (beginning-of-line) (point)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This is the lowest-level declarations code.  Note that we actually call
;;; StringBasedSymbolManager.  That is, most of the "Lisp" massaging
;;; is done in a Java class.  You probably need to go look at that as
;;; well.  The short story with StringBasedSymbolManager is that it
;;; takes a .ajesym file and "traverses" the data structures, putting out
;;; a giant next Lisp list.  In the Java, data is denoted by field name,
;;; in the target Lisp lists, data is denoted by position.  Since the original
;;; structures are circular (both parent and child links),
;;; StringBasedSymbolManager has to be explicitly cut off the recursion at
;;; a chosen depth.  Sometimes "back links" are just listed by name and
;;; file/line location.
;;;
;;; Interface to SymbolManager API.
;;;
;;; Here's where we bridge the gap between Lisp and the SymbolManager API
;;; the compiler provides.  We've got helper code, on the Java side, called
;;; StringBasedSymbolManager.
;;;
;;; The aspectj-fetch-... functions go get data from the compiler API.  The
;;; aspectj-decl-... functions are accessors over that data.  
;;;


(defun aspectj-fetch-declarations (filename)
  (if (aspectj-declarations-file-exists-p filename)
      (let* ((buffer (find-file-noselect (aspectj-declaration-file filename)))
	     (result (read buffer)))
	(kill-buffer buffer)
	result)
    (message
     "AspectJ Mode Warning: No declarations found for %s (compile with ajc)"
     filename)
    nil))
    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Declarations data cache management.
;;;

;;;
;;; Cached version of aspectj-fetch-declarations.
;;;
;;; WARNING/WGG: I'm doing a copy-tree here because *something* is
;;; corrupting the lists, and I can't find it.  It could be something
;;; in imenu.  Could be ours.  Could be JDEE.  It doesn't seem to hurt
;;; performance, fortunately.
;;;
(defun aspectj-declarations (filename) 
  (if (aspectj-declarations-cache-fresh filename)
      (save-excursion (set-buffer (find-file-noselect filename t))
	(copy-tree aspectj-buffers-declarations))  ; !!! WARNING WGG
    (aspectj-fetch-declarations filename)))

(defadvice aspectj-fetch-declarations (around aspectj-decls activate)
  "Caches the declarations data."
  (let ((timestamp ; do first to get stale-est timestamp
	 (aspectj-file-timestamp (aspectj-declaration-file (ad-get-arg 0)))))
    ad-do-it
    (save-excursion (set-buffer (find-file-noselect (ad-get-arg 0) t))
      (setq aspectj-declarations-timestamp
	    (if ad-return-value timestamp aspectj-notexist-timestamp))
      (setq aspectj-buffers-declarations ad-return-value))
    (setq ad-return-value (copy-tree ad-return-value)))) ; !!! WARNING WGG

;;;
;;; The time returned by file attributes are two 16-bit ints in hi-order
;;; low-order bit order, but we really don't care.  Since time moves
;;; forward, we only care if the timestamp changes.
;;;
(defun aspectj-file-timestamp (fn) (nth 5 (file-attributes fn)))

;;; Non-nil so as to not be confused with uninitialized timestamps.
(defconst aspectj-notexist-timestamp -1)

(defun aspectj-declaration-file (fn)
  (concat (file-name-sans-extension fn) ".ajesym"))  ; WGG/WARNING/TEST

(defvar aspectj-buffers-declarations nil)
(make-variable-buffer-local 'aspectj-buffers-declarations)

(defvar aspectj-declarations-timestamp nil)
(make-variable-buffer-local 'aspectj-declarations-timestamp)

(defun aspectj-declarations-file-timestamp (fn)
  (let ((declarations-file (aspectj-declaration-file fn)))
    (if (file-exists-p declarations-file)
	(aspectj-file-timestamp declarations-file)
      aspectj-notexist-timestamp)))

;;;
;;; For what we're doing with this, maybe we could just check the timestamp,
;;; but this is more inclusive if this check is done when aj-mode is on,
;;; but the declarations file isn't loaded *yet*.
;;;
(defun aspectj-declarations-file-exists-p (fn)
  (file-exists-p (aspectj-declaration-file fn)))

(defun aspectj-declarations-loaded ()
  aspectj-buffers-declarations)

;;;
;;; Note that the cache is "fresh" if there was no file last time and no
;;; file this time.  We use "equal" for the comparison because (a) the time
;;; stamp is a cons and (b) because time only moves forward (right?).
;;;
(defun aspectj-declarations-cache-fresh (fn)
  (equal (aspectj-declarations-file-timestamp fn)
	 (save-excursion (set-buffer (find-file-noselect fn t))
	   aspectj-declarations-timestamp)))

;;;
;;; Checks if a data structure derived from the decls is fresh, based on the
;;; timestamp of the declarations that the data structure was derived from.
;;;
(defun aspectj-derived-declarations-cache-fresh (fn source-timestamp)
    (and (equal source-timestamp
		(save-excursion (set-buffer (find-file-noselect fn t))
		  aspectj-declarations-timestamp))
	 (aspectj-declarations-cache-fresh fn)))
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Simple Lisp ADT for data coming in from StringBasedSymbolManager.
;;;
;;; This order lines up with the code in StringBasedSymbolManager.  We use
;;; order rather than a plist to make loading the data faster.
;;;

(defun aspectj-decl-begin-line         (decl) (car (nth 0 decl)))
(defun aspectj-decl-begin-column       (decl) (cdr (nth 0 decl)))
(defun aspectj-decl-end-line           (decl) (car (nth 1 decl)))
(defun aspectj-decl-end-column         (decl) (cdr (nth 1 decl)))
(defun aspectj-decl-kind               (decl) (nth 2 decl))
(defun aspectj-decl-signature          (decl)
  (aspectj-string-replace-match "  " (nth 3 decl) " " t t)) ;; no extra blanks
(defun aspectj-decl-filename           (decl) (nth 4 decl))
(defun aspectj-decl-declaring-type     (decl) (nth 5 decl))
(defun aspectj-decl-points-to          (decl) (nth 6 decl)) ;; getTargets !!!
(defun aspectj-decl-pointed-to-by      (decl) (nth 7 decl)) ;; getPointedToBy
(defun aspectj-decl-declarations       (decl) (nth 8 decl))
;;; WGG - fixes bug in emacssym output
(defun aspectj-decl-is-type-p          (decl)
  (let ((kind (aspectj-decl-kind decl)))
    (case kind 
      ((aspect interface class)
       t)
      (otherwise nil))))
;(defun aspectj-decl-is-type-p          (decl) (nth 9 decl))
(defun aspectj-decl-is-introduced-p    (decl) (nth 10 decl))
;(defun aspectj-decl-has-body-p         (decl) (nth 11 decl))
;(defun aspectj-decl-has-signature-p    (decl) (nth 12 decl))


;;; Based on conventions from StringBasedSymbolManager.
(defun aspectj-decl-elem-null (decl-elem)
  (or (null decl-elem)
      (string= decl-elem "")
      (string= decl-elem "*")		 ; WARNING - where did this come from?
      (string= decl-elem "null")
      (string= decl-elem "not.found")))  ; this one an "error"


;; Helper for above.  Borrowed/modified from dired.el.
(defun aspectj-string-replace-match (regexp string newtext
					  &optional literal global)
  "Replace first match of REGEXP in STRING with NEWTEXT.
Optional arg LITERAL means to take NEWTEXT literally.
Optional arg GLOBAL means to replace all matches."
  (if global
      (let ((start 0))
	(while (string-match regexp string start)
	  (let ((from-end (- (length string) (match-end 0))))
	    (setq string (replace-match newtext t literal string))
	    (setq start (- (length string) from-end))))
	  string)
    (if (not (string-match regexp string 0))
	string
      (replace-match newtext t literal string))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Java-mode highlighting behavior for AspectJ.  
;;;

(require 'font-lock)
(require 'cc-mode)

;;; There are a bunch of new keywords in AspectJ that should be highlighted
;;; properly and might impact indentation.  We treat pointcut and aspect as
;;; method/type decls.  before/after/around are special keywords that appear
;;; in the method-name position as far as Java is concerned, so we just color.
;;;
;;; The major challenge here is that we're hanging stuff off of Java mode.
;;; There are plenty of hooks, but they are not exactly designed for
;;; language extension.  It also makes them hard to turn off while leaving
;;; Java mode on.  Basically buffer-local variables need to get macked
;;; for indentation, and maybe globals for highlighting.  The global stuff
;;; is deferred.
;;;
;;; All this would be so much easier if we could hook off minor modes.
;;; Perhaps we should consider deriving AspectJ mode from Java/JDEE mode.
;;; The problem here is that we have to pick one, unlike with the minor mode.
;;;
;;; We are doing essentially nothing for indentation.
;;; There is a mechanism for controlling the indentation around ":", but it
;;; assumes a finite number of C uses (case, goto, etc.), so it would be
;;; a major hack to get the ":" after pointcut to behave.  So we'll do
;;; without (see progmode/cc-vars.el, part of cc-mode, in particular the
;;; c-hanging-colons-alist).  The real question in getting this to work is
;;; how would java mode misinterpret the pointcut declaration, so what
;;; syntactic class would it fall into?  Of course, if we tried to do this,
;;; it would screw up the colon formatting of the intended C entity as well.
;;; A quick check showed that java-mode does not have any settings on this
;;; var, so maybe we should be ignoring, too.
;;;

;;;
;;; Highlighting.  (Global and invasive to major modes).  All the magic
;;; for this for Java itself is in font-lock.el, not cc-mode.  That's
;;; where a lot of these patterns came from.
;;;
;;; We assign a color to introduction so it looks like class, and pointcut
;;; and advice so that they look like a method,
;;; but doesn't highlight the following class spec (since using, not
;;; defining, in a way).  It's also hard to highlight more than a word.
;;;
;;; Many of the choices/omissions here are highly subjective (e.g., what
;;; to do about receptions, within, eachobject etc.  I haven't fully worked
;;; out the "levels" (1,2,3) of highlighting, either.
;;;
(defvar aspectj-keyword-spec
  (list	'("\\<\\(declare\\)\\>" . font-lock-keyword-face)
        '("\\<\\(pointcut\\)\\>[ \t]*\\(\\sw+\\)?"
           (1 font-lock-keyword-face) (2 font-lock-function-name-face nil t))
        '("\\<\\(aspect\\)\\>[ \t]*\\(\\sw+\\)?"
           (1 font-lock-keyword-face) (2 font-lock-type-face nil t))
	'("\\<\\(of\\)\\>" . font-lock-keyword-face)
	'("\\<\\(dominates\\)\\>" . font-lock-keyword-face)
	'("\\<\\(privileged\\)\\>" . font-lock-keyword-face)
	;; func-name face because of its position, not behavior -- ease reading
	'("\\<\\(before\\|after\\|around\\)\\>" . font-lock-function-name-face)
	'("\\<\\(returning\\|throwing\\)\\>" . font-lock-keyword-face)
        ; '("\\<\\(returns\\)\\>[ \t]*\\(\\sw+\\)?"
        ;   (1 font-lock-keyword-face) (2 font-lock-type-face t t))
	; '("\\<\\(hasaspect\\)\\>" . font-lock-keyword-face)
	'("\\<\\(issingleton\\)\\>" . font-lock-keyword-face)
	'("\\<\\(perthis\\)\\>" . font-lock-keyword-face)
	'("\\<\\(pertarget\\)\\>" . font-lock-keyword-face)
	'("\\<\\(percflow\\)\\>" . font-lock-keyword-face)
	'("\\<\\(percflowbelow\\)\\>" . font-lock-keyword-face)
	'("\\<\\(thisJoinPoint\\)\\>" . font-lock-keyword-face)
	'("\\<\\(thisStaticJoinPointPart\\)\\>" . font-lock-keyword-face)
	'("\\<\\(proceed\\)\\>" . font-lock-keyword-face)
))


;;; WARNING - hack exploiting internals of XEmacs font-lock.el; don't use for
;;; anything else.  The lists storing information for java-mode (and jde-mode)
;;; contain symbols referring to these variables, so writing these variables
;;; changes what is retrieved when the symbols are eval'd.  I should probably
;;; be setting the lists directly and not the variables.  So shoot me.
(when aspectj-xemacsp
(defun font-lock-add-keywords (major-mode spec)
  (when (eq major-mode 'java-mode)
    (setq java-font-lock-keywords-2 (append spec java-font-lock-keywords-2))
    (setq java-font-lock-keywords-3 (append spec java-font-lock-keywords-3))))
) ; when aspectj-xemacsp


;;;
;;; Note we need to do this for only for the actual major mode being fired up,
;;; but that's hard to tell here and could even change.
;;;
(font-lock-add-keywords 'java-mode aspectj-keyword-spec)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Basic "testing" script.  This just prints out what came from SymbolManager
;;; and what goes out to speedbar and the classes menu.
;;;

(defun aspectj-test ()
;  (setq debug-on-error t)
 (let* ((filename (buffer-file-name)))
   (aspectj-test-fetch-decls filename)))

(defun aspectj-test-fetch-decls (filename)
 (let* ((decls (aspectj-fetch-declarations filename))
	(out-buffer (get-buffer-create "*scratch*")))
   (pp decls out-buffer)
   (pp (aspectj-convert-decls 0 decls t nil) out-buffer)
   (pp (aspectj-speedbar-to-imenu-translate
       (aspectj-convert-decls 0 decls t nil)) out-buffer)
   nil))


;; ---- end aspectj-mode.el
(provide 'aspectj-mode)

