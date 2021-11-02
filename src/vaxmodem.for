	program vaxmodem
C----------------------------------------------------------------------
C
C>>>VAXMODEM   WARD CHRISTENSEN PROTOCOL FILE TRANSFER PROGRAM<<<
C
C This program is based on the CP/M XMODEM program for file transfer
C using the Ward Christensen Protocol (also known as XMODEM protocol).
C This allows the transfer of a single file at a time.  YMODEM protocol 
C is also supported to computers running YAM or a compatible program
C allowing one or more files to be transferred in a batch (when the
C /BATCH switch is specified).  VAXMODEM V2 also supports the CRC or
C regular checksum error checking of data packets.
C
C Currently this VAX/VMS version allows ASCII text to be received to
C RMS Variable length CR delimited record files, and BINARY data to be
C received to RMS Fixed length 512 byte record files.  Files sent from
C VMS must be SEQUENTIAL having a recordsize not exceeding 512 bytes.
C
C Author:		A. Nicholson
C Installation:		Computing Centre
C			University of Newcastle
C			NSW  2308  AUSTRALIA
C
C Modification History:
C
C V2.0A	06-Jun-1989	Conversion of the Macro-32 VAXMODEM V1.2F to
C			VAX Fortran, incorporating VAX/VMS command
C			language interpreter (CLI$) routines, and
C			CRC checksum mode.
C
C V2.0B 20-Jun-1989	Modify receive routines to handle YMODEM 1024
C			byte packets
C
C V2.0C 09-Jan-1990	Allow BINARY or ASCII mode to be specified
C			on SEND command line, increase maximum record
C			size for sending to 'buffermax' and remove the
C			512 byte fixed record limitation for binary
C			transfers.
C
C V2.0D 15-Aug-1990	Add 'RECL=buffermax' to VAXMSEND module and
C			increase buffermax to 16384 to overcome
C			'input record too long' error.
C
C----------------------------------------------------------------------
	implicit none

	include	'($RMSDEF)'
	include	'($STSDEF)'

	include 'VAXMODEM.INC/LIST'

	integer*4	status, LIB$GET_FOREIGN, LIB$PUT_OUTPUT,
     &			CLI$PRESENT, CLI$DCL_PARSE, CLI$GET_VALUE
	integer*2	foreign_cmd_size, timeout_value_size
	character	action*4, foreign_cmd*256, timeout_value*16
	logical		more_verbs, OK

	external	VAXMODEM_COMMANDS, LIB$GET_INPUT,
     &			LIB$GET_FOREIGN, LIB$PUT_OUTPUT,
     &			CLI$PRESENT, CLI$DCL_PARSE, CLI$GET_VALUE,
     &			CLI$_PRESENT, CLI$_NEGATED

D	character	cmd_line*256
D	integer*2	cmd_line_size
D	integer*4	error_logger, LIB$ESTABLISH
D	external	error_logger, LIB$ESTABLISH

C----------------------------------------------------------------------

	call initialise
C
C			*** Attempt to get DCL command line.
C			*** If any parameters were given, process
C			*** them and exit, otherwise enter
C			*** interactive mode and process commands
C			*** until 'EXIT' or CTRL-Z is entered
C
	status = LIB$GET_FOREIGN( foreign_cmd,,foreign_cmd_size )
	if( foreign_cmd_size.eq.0 ) then
	  more_verbs = .TRUE.
	  status = LIB$PUT_OUTPUT('VAXMODEM - Ward Christensen '//
     &		'Protocol File Transfer Program '//version)
	  if( .not.status ) call LIB$SIGNAL(%val(status))
	  status = CLI$DCL_PARSE(%val(0), VAXMODEM_COMMANDS,
     &			LIB$GET_INPUT, LIB$GET_INPUT,
     &			char(LF)//char(CR)//'VAXMODEM> ')
	  OK = status .ne. RMS$_EOF
	else
	  status = CLI$DCL_PARSE(foreign_cmd(1:foreign_cmd_size),
     &			VAXMODEM_COMMANDS, LIB$GET_INPUT )
	  more_verbs = .FALSE.
	  OK = .TRUE.
	endif

	do while ( OK )

	  if( status ) then	! no warnings or errors from CLI$DCL_PARSE

D	    if( debug_on ) then
D	      status = CLI$GET_VALUE('$LINE',cmd_line,cmd_line_size)
D	      if( .not.status ) call LIB$SIGNAL(%val(status))
D	      write(unit=debug_unit,fmt='(A)')
D    &		'VAXMODEM> '//cmd_line(1:cmd_line_size)
D	    endif

	    status = CLI$GET_VALUE('$VERB',action)
	    if( .not.status ) call LIB$SIGNAL(%val(status))

	    if( action .eq. 'EXIT' ) then
	      more_verbs = .FALSE.

	    else if( action .eq. 'HELP' ) then
	      call help

	    else if( action .eq. 'SEND' ) then
	      call send

	    else if( action .eq. 'SET ' ) then

	      if( CLI$PRESENT('TIMEOUT') ) then
	        status = CLI$GET_VALUE('TIMEOUT',timeout_value,
     &			timeout_value_size)
	        read(timeout_value(1:timeout_value_size),fmt='(I)')
     &			timeout
	        call LIB$SIGNAL(%val(%loc(VAXMODEM_RCVTMOSET)),
     &			%val(1),%val(timeout))

D	      elseif( CLI$PRESENT('DEBUG').eq.%loc(CLI$_PRESENT) ) then
D		if( debug_on ) then
D		  status = %loc(VAXMODEM_DBGACTIV)
D		else
D		  status = CLI$GET_VALUE('DEBUG',debug_filename,
D    &			debug_filename_size)
D		  if( .not.status ) call LIB$SIGNAL(%val(status))
D		  open(unit=debug_unit, status='NEW',
D    &			file=debug_filename(1:debug_filename_size),
D    &			form='FORMATTED',carriagecontrol='LIST')
D		  debug_on = .TRUE.
D		  debug_old_handler = LIB$ESTABLISH(error_logger)
D		  status = %loc(VAXMODEM_DEBUGON)
D		endif
D		call LIB$SIGNAL(%val(status),%val(1),
D    &			debug_filename(1:debug_filename_size))
D
D	      elseif( CLI$PRESENT('DEBUG').eq.%loc(CLI$_NEGATED) ) then
D		status = %loc(VAXMODEM_DEBUGOFF)
D		call LIB$SIGNAL(%val(status))
D		if( debug_on ) then
D		  call LIB$ESTABLISH(debug_old_handler)
D		  close(unit=debug_unit)
D		endif
D		debug_on = .FALSE.

	      endif

	    else if( action .eq. 'RECE' ) then
	      call receive

	    endif

	  else if( IBITS(status,0,3) .ne. STS$K_WARNING ) then
	    call LIB$SIGNAL(%val(status))  ! do not signal warnings
	  endif

	  if( more_verbs) then
	    status = CLI$DCL_PARSE(%val(0), VAXMODEM_COMMANDS,
     &			LIB$GET_INPUT, LIB$GET_INPUT,
     &			char(LF)//char(CR)//'VAXMODEM> ')
	    OK = status .ne. RMS$_EOF
	  else
	    OK = .FALSE.
	  endif

	enddo

	end
	subroutine initialise
	implicit none

	include '($IODEF)'
	include '($TTDEF)'

	include 'VAXMODEM.INC/NOLIST'

	integer*4	status, SYS$ASSIGN, SYS$QIOW, LIB$GET_EF,
     &			SYS$CLREF, LIB$GET_LUN, exit_status

	external	exit_handler, SYS$ASSIGN, SYS$QIOW, LIB$GET_EF,
     &			SYS$CLREF, LIB$GET_LUN

C----------------------------------------------------------------------

C
C			*** Initialise global variables, get
C			*** an event flag for asynchronous I/O,
C			*** assign a channel to the terminal
C			*** and declare an exit handler.
C

	status = LIB$GET_EF(replyflag)
	if( .not.status ) call LIB$SIGNAL(%val(status))
	status = SYS$CLREF(%val(replyflag))
	if( .not.status ) call LIB$SIGNAL(%val(status))

	status = SYS$ASSIGN('TT',tt_chan,,)
	if( .not.status ) call LIB$SIGNAL(%val(status))

	! get terminal status and save it
	status = SYS$QIOW(,%val(tt_chan),%val(IO$_SENSEMODE),
     &		tt_iosb,,,tt_binary_char,%val(12),,,,)
	if( .not.status ) call LIB$SIGNAL(%val(status))
	if( .not.tt_iosb.iostat) call LIB$SIGNAL(%val(tt_iosb.iostat))
	tt_normal_char = tt_binary_char

	! modify terminal status bits for binary transfers
	tt_binary_char.basic = IBSET(tt_binary_char.basic,TT$V_PASSALL)
	tt_binary_char.basic = IBSET(tt_binary_char.basic,TT$V_EIGHTBIT)
	tt_binary_char.basic = IBSET(tt_binary_char.basic,TT$V_NOBRDCST)
	tt_binary_char.basic = IBCLR(tt_binary_char.basic,TT$V_HALFDUP)
	tt_binary_char.basic = IBCLR(tt_binary_char.basic,TT$V_WRAP)
	tt_binary_char.basic = IBCLR(tt_binary_char.basic,TT$V_ESCAPE)

	! declare exit handler
	exit_block.addr = %loc(exit_handler)
	exit_block.status = %loc(exit_status)
	call SYS$DCLEXH(exit_block)

	! get logical unit numbers
	status = LIB$GET_LUN(xfer_unit)
	if( .not.status ) call LIB$SIGNAL(%val(status))

D	status = LIB$GET_LUN(debug_unit)
D	if( .not.status ) call LIB$SIGNAL(%val(status))

	return
	end
	subroutine exit_handler(exit_status)
	implicit none

	integer*4	exit_status

	include '($IODEF)'

	include 'VAXMODEM.INC/NOLIST'

	integer*4	status, SYS$CANCEL, SYS$QIOW, LIB$FREE_EF,
     &			SYS$DASSGN

	external	SYS$CANCEL, SYS$QIOW, LIB$FREE_EF, SYS$DASSGN

D	external	LIB$ESTABLISH

C----------------------------------------------------------------------

C
C			*** When VAXMODEM exits, restore the
C			*** terminal characteristics, and
C			*** release system resources
C

	! cancel any outstanding I/O
	status = SYS$CANCEL(%val(tt_chan))

	! restore saved terminal characteristics
	status = SYS$QIOW(,%val(tt_chan),%val(IO$_SETMODE),
     &		tt_iosb,,,tt_normal_char,%val(12),,,,)
	if( .not.status ) call LIB$SIGNAL(%val(status))
	if( .not.tt_iosb.iostat) call LIB$SIGNAL(%val(tt_iosb.iostat))

	! free event flag
	status = LIB$FREE_EF(replyflag)
	if( .not.status ) call LIB$SIGNAL(%val(status))

	! close terminal I/O channel
	status = SYS$DASSGN(%val(tt_chan))
	if( .not.status ) call LIB$SIGNAL(%val(status))

D	! turn debug logging off
D	if( debug_on ) then
D	  call LIB$ESTABLISH(debug_old_handler)
D	  close(unit=debug_unit)
D	  debug_on = .FALSE.
D	endif

	return
	end
D	integer*4 function error_logger(sig_args, mech_args)
D	implicit none
D
D	integer*4	sig_args(*), mech_args(*)
D
D	include '($SSDEF)'
D
D	logical*4	put_line
D	external	put_line
D
C----------------------------------------------------------------------
D
C
C			*** This condition handler logs all
C			*** signalled error messages to the
C			*** debugging log file
C
D
D	sig_args(1) = sig_args(1) - 2	! subtract PC/PSL from signal array
D	call SYS$PUTMSG( sig_args, put_line, )
D	sig_args(1) = sig_args(1) + 2	! replace PC/PSL
D
D	error_logger = SS$_RESIGNAL
D	return
D	end
D	logical*4 function put_line(line)
D	implicit none
D
D	character	line*(*)
D
D	include 'VAXMODEM.INC/NOLIST'
D
C----------------------------------------------------------------------
D
C
C			*** Write error message to debugging
C			*** log file
C
D	write(unit=debug_unit,fmt='(A)') line
D
D	put_line = .FALSE.
D	return
D	end
