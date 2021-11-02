# VAXMODEM
XMODEM/YMODEM file transfer program for OpenVMS

Here you will find a program that implements the XMODEM and
YMODEM file transfer protocols for OpenVMS (VAX, Alpha,
Integrity [and x86_64 soon]).

I originally wrote this in VAX Macro-32 in around May 1982, and
this version is the result of converting it to FORTRAN-77 so
that it could be ported to run on the then new Alpha architecture
in around June 1989.  Sadly, I've lost the original Macro-32
sources.

This version is known to build under VAX/VMS V5.5-2 and all
versions of OpenVMS after V6.0 with a FORTRAN-77 compiler.

There's a ZIP file containing the sources in
[vaxmodem.zip](https://raw.githubusercontent.com/agn453/VAXMODEM/master/vaxmodem.zip)
that you can download and transfer to an OpenVMS system.  To
extract the files to an empty directory use the OpenVMS version
of UNZIP and specify the "-V" option to preserve the RMS attributes
of each file -

```
$ create /dir [.vaxmodem]
$ set def [.vaxmodem]
$ unzip "-V" [-]vaxmodem.zip
Archive:  VOGON$DUA1:[LOCAL.SOURCES]VAXMODEM.ZIP;1
  inflating: BUILD_VAXMODEM.COM
  inflating: BUILD_VAXMODEM.LOG
  inflating: BUILD_VAXMODEMDBG.COM
  inflating: MAKEFILE
  inflating: VAXMCMDS.CLD
  inflating: VAXMMSGS.MSG
  inflating: VAXMODEM.EXE-AXP
  inflating: VAXMODEM.EXE-VAX
  inflating: VAXMODEM.FOR
  inflating: VAXMODEM.HLP
  inflating: VAXMODEM.HLP-OLD
  inflating: VAXMODEM.INC
  inflating: VAXMODEM.NEWS
  inflating: VAXMODEM.NEWS-OLD
  inflating: VAXMRECV.FOR
  inflating: VAXMSEND.FOR
  inflating: VAXMUTIL.FOR
$
```

You can build a new executable VAXMODEM.EXE using the BUILD_VAXMODEM
DCL command procedure.

The VAXMODEM.HLP file can be added to your OpenVMS Help library
using (for example)

```
$ library/help local_help:local.hlb vaxmodem.hlp/insert
```

and the compiled binary copied to where you keep your utility
programs (for example)
```
$ copy vaxmodem.exe local_exe:/prot=(w:re)
```

Install it as a foreign command using (for example)
```
$ vaxm*odem :== $local_exe:vaxmodem.exe
```
or have it in the automatic foreign command search-list path pointed to
by the DCL$PATH logical name.


For help on how to use VAXMODEM use
```
$ vaxmodem help
```

Comments/suggestions/bug reports welcome.  Just raise an issue here
using GitHub.

Tony
