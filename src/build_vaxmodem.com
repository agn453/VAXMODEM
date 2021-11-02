$ ver = f$verify(1)
$ env = "''f$environment("PROCEDURE")'"
$ dev = f$parse(env,,,"DEVICE")
$ dir = f$parse(env,,,"DIRECTORY")
$ set default 'dev''dir'
$ fortran/nolist vaxmodem,vaxmrecv,vaxmsend,vaxmutil
$ message/nolist vaxmmsgs
$ set command/object/nolist vaxmcmds
$ link/nomap/notrace vaxmodem,vaxmrecv,vaxmsend,vaxmutil,vaxmmsgs,vaxmcmds
$ exit $status + 0*f$verify(ver)