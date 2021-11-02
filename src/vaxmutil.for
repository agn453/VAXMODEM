	subroutine help
	implicit none

	include '($RMSDEF)'
	include '($HLPDEF)'
	include '($LIBDEF)'

	integer*4	status, hlp_flags
	integer*2	cmd_line_size
	character	cmd_line*256

	integer*4	CLI$GET_VALUE, CLI$PRESENT,
     &			LBR$OUTPUT_HELP, LIB$GET_INPUT, LIB$PUT_OUTPUT

	external	CLI$GET_VALUE, CLI$PRESENT, CLI$_PRESENT,
     &			LBR$OUTPUT_HELP, LIB$GET_INPUT, LIB$PUT_OUTPUT

C----------------------------------------------------------------------

C
C			*** Use the VAX/VMS Librarian to output
C			*** help text
C

	if( CLI$PRESENT('HELP_SPEC') .eq. %loc(CLI$_PRESENT) ) then
	  status = CLI$GET_VALUE('HELP_SPEC',cmd_line,cmd_line_size)
	  if( .not.status ) call LIB$SIGNAL(%val(status))
	  cmd_line = 'VAXMODEM '//cmd_line(1:cmd_line_size)
	  cmd_line_size = cmd_line_size + 9
	else
	  cmd_line = 'VAXMODEM'
	  cmd_line_size = 8
	endif

	hlp_flags = HLP$M_PROCESS + HLP$M_GROUP + HLP$M_SYSTEM +
     &			HLP$M_PROMPT

	status = LBR$OUTPUT_HELP(LIB$PUT_OUTPUT,,
     &			cmd_line(1:cmd_line_size),,
     &			hlp_flags,
     &			LIB$GET_INPUT)
	if( .not.status ) call LIB$SIGNAL(%val(status))

	return
	end
	subroutine set_tt_normal
	implicit none

	include '($IODEF)'

	include 'VAXMODEM.INC/NOLIST'

	integer*4	status, SYS$QIOW

	external	SYS$QIOW

C----------------------------------------------------------------------

	status = SYS$QIOW(,%val(tt_chan),%val(IO$_SETMODE),
     &		tt_iosb,,,tt_normal_char,%val(12),,,,)
	if( .not.status ) call LIB$SIGNAL(%val(status))
	if( .not.tt_iosb.iostat) call LIB$SIGNAL(%val(tt_iosb.iostat))

	return
	end
	subroutine set_tt_binary
	implicit none

	include '($IODEF)'

	include 'VAXMODEM.INC/NOLIST'

	integer*4	status, SYS$QIOW

	external	SYS$QIOW

C----------------------------------------------------------------------

	status = SYS$QIOW(,%val(tt_chan),%val(IO$_SETMODE),
     &		tt_iosb,,,tt_binary_char,%val(12),,,,)
	if( .not.status ) call LIB$SIGNAL(%val(status))
	if( .not.tt_iosb.iostat) call LIB$SIGNAL(%val(tt_iosb.iostat))

	return
	end
	integer*4 function purge_input
	implicit none

	include '($IODEF)'
	include '($SSDEF)'

	include 'VAXMODEM.INC/NOLIST'

	integer*4	status, code, SYS$QIOW
	byte		junk(SECSIZ)

	external	SYS$QIOW

D	integer*4	dbgi

C----------------------------------------------------------------------

	code = IO$_TTYREADALL.or.IO$M_NOECHO.or.IO$M_TIMED.or.
     &		IO$M_PURGE

	do while (.TRUE.)

	  ! read with 3 second time out
	  status = SYS$QIOW(,%val(tt_chan),
     &		%val(code),tt_iordsb,,,
     &		%ref(junk),%val(SECSIZ),%val(3),tt_term,,)
	  if( .not.status ) goto 10

	  if( tt_iordsb.iostat .eq. SS$_TIMEOUT ) then
	    status = %loc(VAXMODEM_SUCCESS)
	    goto 10
	  elseif( .not.tt_iordsb.iostat ) then
	    status = tt_iordsb.iostat
	    goto 10
D	  elseif( debug_on ) then
D	    call put_line('Purge input')
D	    write(unit=debug_unit,fmt=7010)
D    &		(junk(dbgi),dbgi=1,tt_iordsb.ofs_term)
D7010	    format(13X,16Z3.2)
	  endif

	enddo

10	purge_input = status
	return
	end
	integer*4 function send_char(b)
	implicit none

	include '($IODEF)'

	include 'VAXMODEM.INC/NOLIST'

	byte		b
	integer*4	status, code, SYS$QIOW

	external	SYS$QIOW

C----------------------------------------------------------------------

D	if( debug_on ) write(unit=debug_unit,fmt=7010) b
D7010	format('send_char',Z3.2)
	send_packet.id = b
	code = IO$_WRITEVBLK.or.IO$M_NOFORMAT
	status = SYS$QIOW(,%val(tt_chan),
     &		%val(code),
     &		tt_iosb,,,%ref(send_packet),%val(1),,,,)

	if( status ) status = %loc(VAXMODEM_SUCCESS)
	send_char = status
	return
	end
	integer*4 function length( string )
	implicit none

	character	string*(*)
	integer*4	i

C----------------------------------------------------------------------

	do i = len(string), 1, -1
	    if ( string(i:i) .ne. ' ' ) goto 10
	end do
	i = 0
   10	length = i

	return
	end
	subroutine delay(time)
	implicit none

	character*(*)	time

	integer*4	status, flag, btime(2), SYS$BINTIM, SYS$SETIMR,
     &			LIB$GET_EF, SYS$CLREF, SYS$WAITFR, LIB$FREE_EF

	external	SYS$BINTIM, SYS$SETIMR, LIB$GET_EF,
     &			SYS$CLREF, SYS$WAITFR, LIB$FREE_EF

C----------------------------------------------------------------------

	status = LIB$GET_EF(flag)
	if( .not.status ) call LIB$SIGNAL(%val(status))
	status = SYS$CLREF(%val(flag))
	if( .not.status ) call LIB$SIGNAL(%val(status))
	status = SYS$BINTIM(time,btime)
	if( .not.status ) call LIB$SIGNAL(%val(status))
	status = SYS$SETIMR(%val(flag),btime,,,)
	if( .not.status ) call LIB$SIGNAL(%val(status))
	status = SYS$WAITFR(%val(flag))
	if( .not.status ) call LIB$SIGNAL(%val(status))
	status = LIB$FREE_EF(flag)
	if( .not.status ) call LIB$SIGNAL(%val(status))
	return
	end
	integer*4 function updcrc(b,c)
	implicit none
	byte		b
	integer*4	c

	logical		carry
	integer*4	crc, acc, i

C----------------------------------------------------------------------

	crc = c
	acc = b
	do i = 1, 8
	  carry = (IAND(crc,'8000'x) .ne. 0)
	  crc = ISHFT(crc,1)
	  if( IAND(acc,'0080'x) .ne. 0 ) crc = crc + 1
	  acc = ISHFT(acc,1)
	  if( carry ) crc = IEOR(crc,'1021'x)
	end do
	updcrc = crc
	return
	end