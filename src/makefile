MODULES = vaxmodem vaxmutil vaxmsend vaxmrecv vaxmcmds vaxmmsgs

vaxmodem.exe: $(MODULES).obj
	link/notrace/map/brief/exe=$@ $,(MODULES).obj

vaxmmsgs.obj: vaxmmsgs.msg
	message/list vaxmmsgs

vaxmcmds.obj: vaxmcmds.cld
	set command/object/list vaxmcmds

.for.obj: vaxmodem.inc
	fortran/d_lines/list $*.for
