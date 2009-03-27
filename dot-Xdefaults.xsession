!
! ~/.Xdefaults $Revision: 1.6 $
!
! This file is your X resources configuration file. It includes your color
! scheme as defined in ~/.colors.h; use ROTOBG, ROOTFG, etc. where you would
! otherwise use a color name. Invoke xrdb with -I$HOME so ~/.colors.h is
! found.
!
! For a list of relevant X resource classes and resource names for your
! applications, check refer to the man pages for the applications.
!
! EDIT THIS FILE to define X resources for your applications.
!
! EXAMPLES are available in /usr/local/doc/startups.
!

#define FROM_XSESSION
#include <.colors.h>
!
! Global options
!

*foreground:            green
*background:            TEXTBG
*borderColor:           TRIMBG

#include <.Xdefaults.main>
