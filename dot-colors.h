!
! ~/.colors.h $Revision: 1.1 $
!
! This file is your color configuration file. Colors defined here are available
! in your ~/.Xdefaults file and in the environment; refer to hooks(7). You may
! specify additional colors here.
!
! EDIT THIS FILE to configure your color scheme.
!
! EXAMPLES are available in /usr/local/doc/startups.
!

#ifndef FROM_XSESSION
#define  ROOTBG  steelblue
#define  ROOTFG  black
#define  TEXTBG  gray80
#define  TEXTFG  black
#define  TRIMBG  grey10
#define  TRIMFG  red
#define  HIGHBG  grey20
#define  HIGHFG  red

#else

#define  ROOTFG  green
#define  ROOTBG  black
#define  TEXTBG  grey10
#define  TEXTFG  green
#define  TRIMBG  grey10
#define  TRIMFG  red
#define  HIGHBG  grey20
#define  HIGHFG  red

#endif
