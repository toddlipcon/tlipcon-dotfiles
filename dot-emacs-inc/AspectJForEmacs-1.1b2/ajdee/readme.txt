This is the readme.txt for AJDE for Emacs (AJDEE), which is an AspectJ minor mode
for JDEE mode.  This document discusses the distribution's content,
bugs and platform testing issues.  This distribution was obtained under
the GPL and is available from http://aspectj4emacs.sourceforge.net.

    http://eclipse.org/aspectj

This guide, along with the other AspectJ documentation, can be
downloaded at

    http://eclipse.org/aspectj

See the file changes.txt for the exact version number and information on updated
features and their possible effects on you.  Some features in this release
may not be manifested until your AspectJ code is recompiled.

If you should have troubles using AspectJ minor mode and the troubleshooting
instructions in the user guide do not solve your problem
please submit a bug report from http://aspectj4emacs.sourceforge.net.


FILES
=====
The following files are provided in the distribution:

	readme.txt      This file
	changes.txt	    Change history of AJDEE
	ajdee.el        Emacs Lisp code for extending JDEE
	sample.emacs    .emacs settings suitable for jde-mode
	sample.prj      Example of a JDEE prj.el file for spacewar


INCOMPLETE JDEE FEATURES
========================

JDEE Completion: Field and method completion is partially supported by a
  simple extension to the completion parser to recognize aspect declarations.

Templates: 	There are currently no templates specifically for AspectJ.

Wizards: 	There are currently no wizards specifically for AspectJ.


KNOWN BUGS
==========

1. On some speedbar navigations annotations will not appear in a
   newly-dislayed buffer until some input comes from the user.


PLATFORM TESTING
================
Testing is carried out as described in the readme.txt for vanilla aspectj-mode.

Auxiliary Packages
------------------
This distribution was tested on JDEE 2.2.9beta7, speedbar 0.14beta2, semantic
1.4beta12, elib-1.0, eieio-0.17beta3.  See the changes.txt file for details.

