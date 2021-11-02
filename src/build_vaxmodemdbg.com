$ set default local_src_root:[vaxmodem]
$ fortran/d_lines/nolist vaxmodem,vaxmrecv,vaxmsend,vaxmutil
$ message/nolist vaxmmsgs
$ set command/object/nolist vaxmcmds
$ link/nomap/notrace/exe=vaxmodemdbg.exe vaxmodem,vaxmrecv,vaxmsend,vaxmutil,vaxmmsgs,vaxmcmds
$ exit