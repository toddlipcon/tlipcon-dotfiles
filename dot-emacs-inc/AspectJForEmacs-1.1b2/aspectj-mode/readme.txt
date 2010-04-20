This is the readme.txt for aspectj-mode, an AspectJ minor mode for java-mode.  
This document discusses the distribution's contents, known
bugs and platform testing issues.  This distribution was obtained under
the GPL and is available from http://aspectj4emacs.sourceforge.net.

For all other issues refer to the development guide at 

    http://eclipse.org/aspectj

This guide, along with the other AspectJ documentation, can be
downloaded at

    http://eclipse.org/aspectj

See the file changes.txt for the exact version number and information on updated
features and their possible effects on you.  Some features in this release
may not be manifested until your AspectJ code is recompiled.

If you should have troubles using AspectJ minor mode and the troubleshooting
instructions in the user guide do not quickly solve your problem
please submit a bug report from http://aspectj4emacs.sourceforge.net.


FILES
=====
The following files are provided in the distribution:

	readme.txt        This file
	changes.txt       Change history of aspectj-mode
	aspectj-mode.el   Emacs Lisp code for extending java-mode
	sample.emacs      .emacs settings suitable for java-mode



PLATFORM TESTING
================

AspectJ Compiler
----------------
This distribution is normally tested on the AspectJ compiler release prior
to the current release (compiler and IDE development are concurrent).
When changes to the compiler may affect AspectJ mode, testing is also done
on a pre-release of the current compiler release.

Emacs
-----
This distribution was tested under GNU Emacs 20.7 (ntemacs),
XEmacs 21.4.3 (NT).  GNU Emacs has also been used on Emacs 20.3.1
on Solaris and 20.7 on Linux; XEmacs 21.1.4 on Linux.  Positive reports
have been heard on GNU Emacs 21 alpha.  Other combinations have not
been tested.  Please let us know if you try others.

Operating System
----------------
AspectJ mode is tested on Windows 2000, and has been observed to work
under Windows NT 4.0, Solaris, and Linux.  Please let us know if you
try any additional platforms.


