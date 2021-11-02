	subroutine send
	implicit none

	include '($RMSDEF)'

	include 'VAXMODEM.INC/NOLIST'

	integer*4	status, i, xfer_pointer, xfer_filename_size,
     &			xfer_recl, xfer_bytes_read
	integer*2	file_spec_size
	logical		first_file
	character	file_spec*256, xfer_filename*256,
     &			xfer_organization*10,
     &			xfer_recordtype*9, xfer_carriagecontrol*7,
     &			xfer_form*11, xfer_data*(buffermax)

	integer*4	CLI$GET_VALUE, CLI$PRESENT, LIB$FIND_FILE,
     &			LIB$FIND_FILE_END,
     &			send_path_name, send_data, send_end_of_file,
     &			wait_for_nak, check_send, length, send_char
	external	CLI$GET_VALUE, CLI$PRESENT,
     &			CLI$_PRESENT, CLI$_NEGATED, CLI$_ABSENT,
     &			LIB$FIND_FILE, LIB$FIND_FILE_END,
     &			send_path_name, send_data, send_end_of_file,
     &			wait_for_nak, check_send, length, send_char

C----------------------------------------------------------------------

C
C			*** Send specified file(s) using selected
C			*** XMODEM/YMODEM protocol
C

	! Get command line details
	status = CLI$GET_VALUE('FILE_SPEC',file_spec,file_spec_size)
	if( .not.status) goto 910

	xfer_mode = 0
	if( (CLI$PRESENT('ASCII') .eq. %loc(CLI$_PRESENT)) .or.
     &	    (CLI$PRESENT('BINARY') .eq. %loc(CLI$_NEGATED)) ) then
	  xfer_mode = IBCLR(xfer_mode,BINARY_MODE)
	endif
	if( (CLI$PRESENT('BINARY') .eq. %loc(CLI$_PRESENT)) .or.
     &	    (CLI$PRESENT('ASCII') .eq. %loc(CLI$_NEGATED)) ) then
	  xfer_mode = IBSET(xfer_mode,BINARY_MODE)
	endif
	if( CLI$PRESENT('BATCH') .eq. %loc(CLI$_PRESENT) ) then
	  xfer_mode = IBSET(xfer_mode,BATCH_MODE)
	endif
	if( CLI$PRESENT('ASYNC86') .eq. %loc(CLI$_PRESENT) ) then
	  xfer_mode = IBSET(xfer_mode,ASYNC86_MODE)
	endif
	if( CLI$PRESENT('CR') .eq. %loc(CLI$_NEGATED) ) then
	  xfer_mode = IBSET(xfer_mode,STRIP_CR_MODE)
	endif
	if( CLI$PRESENT('LF') .eq. %loc(CLI$_NEGATED) ) then
	  xfer_mode = IBSET(xfer_mode,STRIP_LF_MODE)
	endif

	xfer_pointer = 0
	first_file = .TRUE.

	! Lookup file
	status = LIB$FIND_FILE(file_spec(1:file_spec_size),
     &		xfer_filename,xfer_pointer)
	if( status .ne. RMS$_NORMAL ) goto 910

	do while ( status .ne. RMS$_NMF )

	  if( status .ne. RMS$_NORMAL ) goto 900

	  xfer_filename_size = length(xfer_filename)

	  ! get file attributes
	  inquire(file=xfer_filename(1:xfer_filename_size),
     &		organization=xfer_organization,
     &		recordtype=xfer_recordtype,
     &		carriagecontrol=xfer_carriagecontrol,
     &		form=xfer_form,
     &		recl=xfer_recl)

D	  if( debug_on ) write(unit=debug_unit,fmt=7010)
D    &	    xfer_filename(1:xfer_filename_size),
D    &	    xfer_organization, xfer_recordtype,
D    &	    xfer_carriagecontrol, xfer_form, xfer_recl
D7010	  format('SEND file ',A/
D    &		10X,'Organization=',A,' Recordtype=',A/
D    &		10X,'Carriagecontrol=',A,' Form=',A/
D    &		10X,'Recordsize=',I)

	  ! allow only sequential files
	  if( xfer_organization .ne. 'SEQUENTIAL' ) then
	    status = %loc(VAXMODEM_FILNOTSEQ)
	    goto 900
	  endif

	  ! adjust record length
	  if( xfer_recordtype .ne. 'FIXED' ) then
	    xfer_recl = buffermax
	  endif

	  ! if mode not specified, try to guess it
	  if( (CLI$PRESENT('ASCII') .eq. %loc(CLI$_ABSENT)) .and.
     &	      (CLI$PRESENT('BINARY') .eq. %loc(CLI$_ABSENT)) ) then
	    ! fixed length records are sent in binary mode
	    if( xfer_recordtype .eq. 'FIXED' ) then
	      xfer_mode = IBSET(xfer_mode,BINARY_MODE)
	    else
	      xfer_mode = IBCLR(xfer_mode,BINARY_MODE)
	    endif
	  endif

D	  if( debug_on ) then
D	    if( BTEST(xfer_mode,BINARY_MODE) ) then
D	      call put_line('Sending in BINARY mode')
D	    else
D	      call put_line('Sending in ASCII mode')
D	    endif
D	  endif

	  ! check maximum recordsize
	  if( xfer_recl .gt. buffermax ) then
	    status = %loc(VAXMODEM_RECTOOBIG)
	    goto 900
	  endif

	  open(unit=xfer_unit,file=xfer_filename(1:xfer_filename_size),
     &		recl=xfer_recl,status='OLD',readonly)

	  if( first_file ) then
	    call LIB$SIGNAL(%val(%loc(VAXMODEM_SNDREADY)))
	    first_file = .FALSE.
	  endif

	  if( BTEST(xfer_mode, BATCH_MODE) ) then
	    status = send_path_name(xfer_filename)
	    if( .not.status ) goto 890
	  endif

	  bufferptr = 1
	  sector_number.i4 = 0
	  status = wait_for_nak()
	  if( .not.status ) goto 890

	  read(unit=xfer_unit,fmt='(Q,A)',end=10)
     &		xfer_bytes_read, xfer_data

	  do while (.TRUE.)

	    if( xfer_bytes_read .ne. 0 ) then
	      do i = 1, xfer_bytes_read
		buffer(bufferptr:bufferptr) = xfer_data(i:i)
		status = check_send()
		if( .not.status ) goto 890
	      enddo
	    endif
	    if( .not.BTEST(xfer_mode, BINARY_MODE) ) then
	      ! append line terminator(s)
	      if( .not.BTEST(xfer_mode,STRIP_CR_MODE) ) then
		buffer(bufferptr:bufferptr) = char(CR)
		status = check_send()
		if( .not.status ) goto 890
	      endif
	      if( .not.BTEST(xfer_mode,STRIP_LF_MODE) ) then
		buffer(bufferptr:bufferptr) = char(LF)
		status = check_send()
		if( .not.status ) goto 890
	      endif
	    endif
	    read(unit=xfer_unit,fmt='(Q,A)',end=10)
     &		xfer_bytes_read, xfer_data
	  enddo

	  ! close file, flush buffer
10	  close(unit=xfer_unit)
	  if( .not.BTEST(xfer_mode, BINARY_MODE) ) then
	    ! append end-of-file to text file
	    buffer(bufferptr:bufferptr) = char(CTRL_Z)
	    status = check_send()
	    if( .not.status ) goto 900
	  endif
	  if( bufferptr.ne.1 ) then
	    do while ( bufferptr - SECSIZ*(bufferptr/SECSIZ) .ne. 1 )
	      buffer(bufferptr:bufferptr) = char(NUL)
	      bufferptr = bufferptr + 1
	    enddo
	    status = send_data()
	    if( .not.status ) goto 900
	    bufferptr = 1
	  endif
	  status = send_end_of_file()
	  if( .not.status ) goto 900

	  ! next file
	  status = LIB$FIND_FILE(file_spec(1:file_spec_size),
     &		xfer_filename,xfer_pointer)
	enddo

	! send end path name
	if( BTEST(xfer_mode, BATCH_MODE) ) then
	  status = send_path_name(']'//char(NUL)//';')
	  if( .not.status ) goto 900
	endif

	call set_tt_normal
	status = %loc(VAXMODEM_SUCCESS)
	goto 910

	! error, close open file
890	close(unit=xfer_unit)

900	call set_tt_normal
	i = send_char(CAN)

	! delay 3 seconds
910	call delay('0 00:00:03.0')
	! signal send exit status
	if( .not.status ) call LIB$SIGNAL(%val(status))

	status = LIB$FIND_FILE_END(xfer_pointer)
	if( .not.status ) call LIB$SIGNAL(%val(status))
	return
	end
	integer*4 function check_send
	implicit none

	include 'VAXMODEM.INC/NOLIST'

	integer*4	send_data
	external	send_data

	integer*4	status

C----------------------------------------------------------------------

	bufferptr = bufferptr + 1
	if( bufferptr .gt. buffermax ) then
	  status = send_data()
	  bufferptr = 1
	else
	  status = %loc(VAXMODEM_SUCCESS)
	endif

	check_send = status
	return
	end
	integer*4 function send_data
	implicit none

	include 'VAXMODEM.INC/NOLIST'

	integer*4	status, i

	integer*4	send_sector
	external	send_sector

C----------------------------------------------------------------------

	do i = 1, (bufferptr-1)/SECSIZ
	  status = send_sector(i)
	  if( .not.status ) goto 999
	enddo

999	send_data = status
	return
	end
	integer*4 function send_sector(n)
	implicit none
	integer*4	n

	include '($SSDEF)'

	include 'VAXMODEM.INC/NOLIST'

	integer*4	status, i, j, nak_count
	record /int4byte/ c

	integer*4	output_sector, updcrc, SYS$WAITFR,
     &			purge_input

	external	output_sector, updcrc, SYS$WAITFR,
     &			purge_input

C----------------------------------------------------------------------

	nak_count = 0

	! form packet and calculate checksum
	sector_number.i4 = sector_number.i4 + 1
	send_packet.id = SOH
	send_packet.secnum = sector_number.b(1)
	send_packet.seknum = .not. sector_number.b(1)
	cksm.i4 = 0
	crc.i4 = 0
	j = (n-1)*SECSIZ
	do i = 1, SECSIZ
	  j = j + 1
	  c.i4 = ichar(buffer(j:j))
	  send_packet.sector(i) = c.b(1)
	  cksm.i4 = cksm.i4 + c.b(1)
	  crc.i4 = updcrc(c.b(1),crc.i4)
	enddo
	crc.i4 = updcrc(0,updcrc(0,crc.i4))

	do while (nak_count .lt. retrymax)

	  status = output_sector()
	  if( .not.status ) goto 999

	  status = SYS$WAITFR(%val(replyflag))
	  if( .not.status ) goto 999

	  if( tt_iordsb.iostat .eq. SS$_TIMEOUT ) then
	    ! reply timed out
D	    if( debug_on ) call put_line('Reply timed out')
	    nak_count = nak_count + 1
	  else
	    ! check reply
	    if( receive_packet.reply .eq. NAK ) then
D	      if( debug_on ) call put_line('SEND Sector NAK')
	      status = purge_input()
	      if( .not. status ) goto 999
	      nak_count = nak_count + 1
	    else if( receive_packet.reply .eq. ACK ) then
D	      if( debug_on ) call put_line('SEND Acknowledged')
	      status = %loc(VAXMODEM_SUCCESS)
	      goto 999
	    else if( receive_packet.reply .eq. CAN ) then
D	      if( debug_on ) call put_line('SEND Cancelled')
	      status = %loc(VAXMODEM_SENDCAN)
	      goto 999
	    else if( (receive_packet.reply .eq. CTRL_C) .or.
     &		(receive_packet.reply .eq. CTRL_Y) ) then
D	      if( debug_on ) call put_line('SEND Aborted by user')
	      status = purge_input()
	      if( .not. status ) goto 999
	      status = %loc(VAXMODEM_SENDABORT)
	      goto 999
	    else
D	      if( debug_on ) write(unit=debug_unit,fmt=7010)
D    &		receive_packet.reply
D7010	      format('Unrecognised reply',Z3.2)
	      status = purge_input()
	      if( .not. status ) goto 999
	      nak_count = nak_count + 1
	    endif
	  endif

	enddo
	status = %loc(VAXMODEM_NAK2MANY)

999	send_sector = status
	return
	end
	integer*4 function output_sector
	implicit none

	include '($IODEF)'

	include 'VAXMODEM.INC/NOLIST'

	integer*4	status, code, SYS$QIO, SYS$QIOW

	external	SYS$QIO, SYS$QIOW

D	integer*4	dbgi, dbgj
D	character	dbga*16

C----------------------------------------------------------------------

D	if( debug_on ) then
D	  write(unit=debug_unit,fmt=7000) send_packet.id,
D    &		send_packet.secnum, send_packet.seknum
D7000	  format('Output_sector',3Z3.2)
D	  do dbgi = 1, SECSIZ, 16
D	    dbga = send_packet.data(dbgi:dbgi+15)
D	    do dbgj = 1, 16
D	      if( dbga(dbgj:dbgj).lt.' ' .or. dbga(dbgj:dbgj).gt.'~' )
D    &		dbga(dbgj:dbgj) = '.'
D	    enddo
D	    write(unit=debug_unit,fmt=7010)
D    &		(send_packet.sector(dbgi+dbgj),dbgj=0,15),dbga
D7010	    format(13X,16Z3.2,2X,A16)
D	  enddo
D	  if( BTEST(xfer_mode,CRC_MODE) ) then
D	    write(unit=debug_unit,fmt=7020) crc.b(2), crc.b(1)
D7020	    format(10X,'crc',2Z3.2/)
D	  else
D	    write(unit=debug_unit,fmt=7030) cksm.b(1)
D7030	    format(9X,'cksm',Z3.2/)
D	  endif
D	endif

	! send all but the checksum of the send_packet
	code = IO$_WRITEVBLK.or.IO$M_NOFORMAT
	status = SYS$QIOW(,%val(tt_chan),
     &		%val(code),
     &		tt_iosb,,,%ref(send_packet),%val(SECSIZ+3),,,,)
	if( .not.status ) goto 999

	! post a read for the reply with 10 second timeout
	code = IO$_TTYREADALL.or.IO$M_NOECHO.or.IO$M_TIMED
	status = SYS$QIO(%val(replyflag),%val(tt_chan),
     &		%val(code),
     &		tt_iordsb,,,%ref(receive_packet),%val(1),
     &		%val(10),tt_term,,,,)
	if( .not.status ) goto 999

	! now send the checksum
	code = IO$_WRITEVBLK.or.IO$M_NOFORMAT
	if( BTEST(xfer_mode,ASYNC86_MODE) ) then
	  send_checksum.cksm = cksm.b(1)
	  send_checksum.necnak = NAK
	  status = SYS$QIOW(,%val(tt_chan),
     &		%val(code),
     &		tt_iosb,,,%ref(send_checksum.cksm),%val(2),,,,)
	else
	  if( BTEST(xfer_mode,CRC_MODE) ) then
	    send_checksum.crc(1) = crc.b(2)	! high byte first
	    send_checksum.crc(2) = crc.b(1)
	    status = SYS$QIOW(,%val(tt_chan),
     &		%val(code),
     &		tt_iosb,,,%ref(send_checksum.crc),%val(2),,,,)
	  else
	    send_checksum.cksm = cksm.b(1)
	    status = SYS$QIOW(,%val(tt_chan),
     &		%val(code),
     &		tt_iosb,,,%ref(send_checksum.cksm),%val(1),,,,)
	  endif
	endif

999	output_sector = status
	return
	end
	integer*4 function send_path_name(filename)
	implicit none

	character	filename*(*)

	include 'VAXMODEM.INC/NOLIST'

	integer		status, size, i, rsqbrk, semicolon,
     &			length, wait_for_nak, send_sector

	external	length, wait_for_nak, send_sector

C----------------------------------------------------------------------

	sector_number.i4 = -1
	do i = 1, SECSIZ
	  buffer(i:i) = char(NUL)
	end do

	size = length(filename)
	rsqbrk = index(filename(1:size),']')
	! remember alternate VMS syntax
	if( rsqbrk.eq.0 ) rsqbrk = index(filename(1:size),'>')
	semicolon = index(filename(1:size),';')
	i = semicolon - rsqbrk -1
	if( i .gt. SECSIZ ) then
	  status = %loc(VAXMODEM_NAMTOOBIG)
	  goto 999
	endif

	buffer(1:i) = filename(rsqbrk+1:semicolon-1)

20	status = wait_for_nak()
	if( .not.status ) goto 999

	status = send_sector(1)

999	send_path_name = status
	return
	end
	integer*4 function send_end_of_file
	implicit none

	include '($IODEF)'
	include '($SSDEF)'

	include 'VAXMODEM.INC/NOLIST'

	integer*4	status, code, nak_count, send_char,
     &			SYS$QIO, SYS$QIOW, SYS$WAITFR

	external	send_char, SYS$QIO, SYS$QIOW, SYS$WAITFR

C----------------------------------------------------------------------

	nak_count = 0
	do while (nak_count .lt. retrymax)

	  ! post a read for the reply with 10 second timeout
	  code = IO$_TTYREADALL.or.IO$M_NOECHO.or.IO$M_TIMED
	  status = SYS$QIO(%val(replyflag),%val(tt_chan),
     &		%val(code),
     &		tt_iordsb,,,%ref(receive_packet),%val(1),
     &		%val(10),tt_term,,,,)
	  if( .not.status ) goto 999

	  status = send_char(EOT)
	  if( .not.status ) goto 999

	  status = SYS$WAITFR(%val(replyflag))
	  if( .not.status ) goto 999

	  if( tt_iordsb.iostat .eq. SS$_TIMEOUT ) then
	    ! reply timed out
D	    if( debug_on ) call put_line('EOF Reply timed out')
	    nak_count = nak_count + 1
	  else
	    ! check reply
	    if( .not.tt_iordsb.iostat ) then
	      status = tt_iordsb.iostat
	      goto 999
	    else if( receive_packet.reply .eq. ACK ) then
D	      if( debug_on ) call put_line('EOF Acknowledged')
	      status = %loc(VAXMODEM_SUCCESS)
	      goto 999
	    else if( receive_packet.reply .eq. CAN ) then
D	      if( debug_on ) call put_line('EOF Cancelled')
	      status = %loc(VAXMODEM_SENDCAN)
	      goto 999
	    else if( (receive_packet.reply .eq. CTRL_C) .or.
     &		(receive_packet.reply .eq. CTRL_Y) ) then
D	      if( debug_on ) call put_line('EOF Aborted by user')
	      status = %loc(VAXMODEM_SENDABORT)
	      goto 999
	    else
D	      if( debug_on ) write(unit=debug_unit,fmt=7010)
D    &		receive_packet.reply
D7010	      format('EOF Unrecognised reply',Z3.2)
	      nak_count = nak_count + 1
	    endif
	  endif

	enddo
	status = %loc(VAXMODEM_NOEOTACK)

999	send_end_of_file = status
	return
	end
	integer*4 function wait_for_nak
	implicit none

	include '($IODEF)'
	include '($SSDEF)'

	include 'VAXMODEM.INC/NOLIST'

	integer*4	status, code, SYS$QIOW,
     &			nak_count, purge_input

	external	SYS$QIOW, purge_input

C----------------------------------------------------------------------

	nak_count = 0

	call set_tt_binary

	do while (nak_count .lt. retrymax)

	  ! post a read for the NAK handshaking character
	  code = IO$_TTYREADALL.or.IO$M_NOECHO.or.IO$M_TIMED
	  status = SYS$QIOW(,%val(tt_chan),
     &		%val(code),
     &		tt_iordsb,,,%ref(receive_packet),%val(1),
     &		%val(30),tt_term,,)
	  if( .not.status ) goto 10
	  if( tt_iordsb.iostat .eq. SS$_TIMEOUT ) then
D	    if( debug_on ) call put_line('NAK Timed out')
	    nak_count = nak_count + 1
	  else
	    if( .not.tt_iordsb.iostat ) then
	      status = tt_iordsb.iostat
	      goto 10
	    else
	      if( receive_packet.reply .eq. NAK ) then
D		if( debug_on ) call put_line('NAK checksum mode')
		xfer_mode = IBCLR(xfer_mode,CRC_MODE)
		status = %loc(VAXMODEM_SUCCESS)
		goto 10
	      else if( receive_packet.reply .eq. WANT_CRC ) then
D		if( debug_on ) call put_line('NAK CRC mode')
		xfer_mode = IBSET(xfer_mode,CRC_MODE)
		status = %loc(VAXMODEM_SUCCESS)
		goto 10
	      else if( (receive_packet.reply .eq. CAN) .or.
     &		       (receive_packet.reply .eq. CTRL_C) .or.
     &		       (receive_packet.reply .eq. CTRL_Y) ) then
D		if( debug_on )
D    &		  call put_line('NAK Cancelled or aborted by user')
		status = purge_input()
		if( .not. status ) goto 10
		status = %loc(VAXMODEM_SENDCAN)
		goto 10
	      else
D		if( debug_on ) write(unit=debug_unit,fmt=7010)
D    &		  receive_packet.reply
D7010		format('NAK Unrecognised reply',Z3.2)
		status = purge_input()
		if( .not. status ) goto 10
		nak_count = nak_count + 1
	      endif
	    endif
	  endif

	enddo
	status = %loc(VAXMODEM_NONAK)

10	wait_for_nak = status
	return
	end