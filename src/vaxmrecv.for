	subroutine receive
	implicit none

	include 'VAXMODEM.INC/NOLIST'

	integer*4	status, xfer_filename_size
	integer*2	file_spec_size
	character	file_spec*256, xfer_filename*128

	integer*4	CLI$GET_VALUE, CLI$PRESENT, LIB$GET_INPUT,
     &			length, receive_path_name, receive_sector,
     &			write_buffer

	external	CLI$GET_VALUE, CLI$PRESENT, CLI$_PRESENT,
     &			CLI$_NEGATED, LIB$GET_INPUT,
     &			length, receive_path_name, receive_sector,
     &			write_buffer

C----------------------------------------------------------------------

C
C			*** Receive one or more files using
C			*** XMODEM or YMODEM protocol
C

	! Get command line details
	xfer_mode = 0
	if( CLI$PRESENT('BATCH') .eq. %loc(CLI$_PRESENT) ) then
	  xfer_mode = IBSET(xfer_mode,BATCH_MODE)
	endif
	if( (CLI$PRESENT('ASCII') .eq. %loc(CLI$_PRESENT)) .or.
     &	    (CLI$PRESENT('BINARY') .eq. %loc(CLI$_NEGATED)) ) then
	  xfer_mode = IBCLR(xfer_mode,BINARY_MODE)
	endif
	if( (CLI$PRESENT('BINARY') .eq. %loc(CLI$_PRESENT)) .or.
     &	    (CLI$PRESENT('ASCII') .eq. %loc(CLI$_NEGATED)) ) then
	  xfer_mode = IBSET(xfer_mode,BINARY_MODE)
	endif
	if( CLI$PRESENT('CRC') .eq. %loc(CLI$_PRESENT) ) then
	  xfer_mode = IBSET(xfer_mode,CRC_MODE)
	endif
	if( CLI$PRESENT('EIGHTBIT') .eq. %loc(CLI$_PRESENT) ) then
	  xfer_mode = IBSET(xfer_mode,EIGHTBIT_MODE)
	endif
	if( CLI$PRESENT('SECTOR') .eq. %loc(CLI$_PRESENT) ) then
	  xfer_mode = IBSET(xfer_mode,SECTOR_MODE)	! Binary 128 byte
	  status = %loc(VAXMODEM_RCVSECTOR)
	  call LIB$SIGNAL(%val(status))
	endif
	if( .not.BTEST(xfer_mode,BATCH_MODE) ) then
	  if( CLI$PRESENT('FILE_SPEC') .eq. %loc(CLI$_PRESENT) ) then
	    status = CLI$GET_VALUE('FILE_SPEC',
     &			file_spec,file_spec_size)
	    if( .not.status) goto 910
	  else
	    status = LIB$GET_INPUT(file_spec,'_File: ',file_spec_size)
	    if( file_spec_size .eq. 0 ) goto 910
	    if( .not.status) goto 910
D	    if( debug_on ) call put_line('_File: '//
D    &		file_spec(1:file_spec_size))
	  endif
	endif

	! Initial packet timeout is longer
	rcv_timeout = timeout * 3

	! Tell user we're ready
	status = %loc(VAXMODEM_RCVREADY)
	call LIB$SIGNAL(%val(status))

	! Put terminal in binary I/O mode
	call set_tt_binary

	do while (status)

	  ! if batch mode, get filename
	  if( BTEST(xfer_mode,BATCH_MODE) ) then
	    status = receive_path_name(xfer_filename)
	    if( .not.status ) goto 900
	    if( xfer_filename(1:1).eq.char(NUL) ) then
	      ! no more files
	      status = %loc(VAXMODEM_SUCCESS)
	      goto 900
	    endif
	    xfer_filename_size = length(xfer_filename)
	    rcv_timeout = timeout
	  else
	    xfer_filename = file_spec
	    xfer_filename_size = file_spec_size
	  endif

	  if( BTEST(xfer_mode,SECTOR_MODE) ) then
	    open(unit=xfer_unit,status='NEW',err=900,
     &		file=xfer_filename(1:xfer_filename_size),
     &		form='UNFORMATTED',recordsize=SECSIZ/4,	! longwords
     &		recordtype='FIXED')
D	    if( debug_on ) call put_line('Opened file '//
D    &	      xfer_filename(1:xfer_filename_size)//
D    &	      ' for SECTOR transfer')
	  else
	    if( BTEST(xfer_mode,BINARY_MODE) ) then
	      open(unit=xfer_unit,status='NEW',err=900,
     &		file=xfer_filename(1:xfer_filename_size),
     &		form='UNFORMATTED',recordsize=linemax/4,	! longwords
     &		recordtype='FIXED')
D	      if( debug_on ) call put_line('Opened file '//
D    &	      xfer_filename(1:xfer_filename_size)//
D    &	      ' for BINARY transfer')
	    else
	      open(unit=xfer_unit,status='NEW',err=900,
     &		file=xfer_filename(1:xfer_filename_size),
     &		form='FORMATTED',carriagecontrol='LIST',
     &		recordsize=linemax,recordtype='VARIABLE')
D	      if( debug_on ) call put_line('Opened file '//
D    &	        xfer_filename(1:xfer_filename_size)//
D    &	        ' for ASCII transfer')
	      eof_seen = .FALSE.
	      line_bufferptr = 1
	    endif
	  endif

	  bufferptr = 1
	  sector_number.i4 = 0

	  do while (.TRUE.)

	    status = receive_sector()
	    if( status .eq. %loc(VAXMODEM_RECVDEOF) ) then
	      ! end of file, flush buffer to file
	      if( bufferptr .ne. 1 ) status = write_buffer()
	      if( .not.BTEST(xfer_mode,BINARY_MODE) .and.
     &		(line_bufferptr .gt. 1) .and. .not.eof_seen )
     &		  write(unit=xfer_unit,fmt='(A)')
     &		  line_buffer(1:line_bufferptr-1)
	      status = %loc(VAXMODEM_SUCCESS)
	      goto 890
	    elseif( status .eq. %loc(VAXMODEM_SUCCESS) ) then
	      ! copy sector to buffer
	      buffer(bufferptr:bufferptr+blklen-1) =
     &			receive_packet.data
	      bufferptr = bufferptr + blklen
	      if( bufferptr .gt. buffermax-KSIZE ) then
		status = write_buffer()
		if( .not.status ) goto 890
	      endif
	    else
	      ! an error has occurred, abort transfer
	      goto 890
	    endif

	    rcv_timeout = timeout

	  end do

890	  close(unit=xfer_unit)
	  if( .not.BTEST(xfer_mode,BATCH_MODE) ) goto 900

	enddo

900	call set_tt_normal

	call delay('0 00:00:03.0')

910	if( .not.status ) call LIB$SIGNAL(%val(status))

	return
	end
	integer*4 function receive_sector
	implicit none

	include '($IODEF)'
	include '($SSDEF)'

	include 'VAXMODEM.INC/NOLIST'

	integer*4	status, code, nak_count,
     &			SYS$QIOW, verify_checksum, send_char,
     &			send_nak, purge_input, receive_rest_of_packet

	external	SYS$QIOW, verify_checksum, send_char,
     &			send_nak, purge_input, receive_rest_of_packet

D	integer*4	dbgi, dbgj
D	character	dbga*16

C----------------------------------------------------------------------

C
C			*** Receive a single XMODEM or YMODEM
C			*** packet
C

	nak_count = 0

	do while (nak_count .lt. retrymax)

	  ! post a read for first character of packet
	  code = IO$_TTYREADALL.or.IO$M_NOECHO.or.IO$M_TIMED
	  status = SYS$QIOW(,%val(tt_chan),
     &		%val(code),
     &		tt_iordsb,,,%ref(receive_packet),%val(1),
     &		%val(rcv_timeout),tt_term,,)
	  if( .not.status ) goto 10

	  if( tt_iordsb.iostat .eq. SS$_TIMEOUT ) then
D	    if( debug_on ) call put_line('RECV Header timed out')
	    status = send_nak()
	    if( .not.status ) goto 10
	  else
	    if( .not.tt_iordsb.iostat ) then
	      ! must be an I/O error, so abort
	      status = tt_iordsb.iostat
	      goto 10
	    else if( receive_packet.reply .eq. SOH ) then
	      blklen = SECSIZ
D	      if( debug_on ) call put_line('RECV start of 128 packet')
	    else if( receive_packet.reply .eq. STX ) then
	      blklen = KSIZE
D	      if( debug_on ) call put_line('RECV start of 1024 packet')
	    else if( receive_packet.reply .eq. EOT ) then
D	      if( debug_on ) call put_line('RECV end of file')
	      status = send_char(ACK)
	      if( .not.status ) goto 10
	      status = %loc(VAXMODEM_RECVDEOF)
	      goto 10
	    else if( (receive_packet.reply .eq. CAN) .or.
     &		       (receive_packet.reply .eq. CTRL_C) .or.
     &		       (receive_packet.reply .eq. CTRL_Y) ) then
		! abort transfer
D		if( debug_on )
D    &		  call put_line('RECV Cancelled or aborted by user')
		status = %loc(VAXMODEM_RECVCAN)
		goto 10
	    else
	      ! un-recognised header character
D	      if( debug_on ) write(unit=debug_unit,fmt=7010)
D    &		receive_packet.reply
D7010	      format('RECV unrecognised header',Z3.2)
	      status = purge_input()
	      if( .not.status ) goto 10
	      status = send_nak()
	      if( .not.status ) goto 10
	      goto 20
	    endif
	    ! now get rest of packet
	    status = receive_rest_of_packet()
	    if( .not. status ) goto 10

	    if( tt_iordsb.iostat .eq. SS$_TIMEOUT ) then
D	      if( debug_on ) call put_line('RECV Data timed out')
	      status = send_nak()
	      if( .not.status ) goto 10
	    else if( .not.tt_iordsb.iostat ) then
	      ! must be an I/O error, so abort
	      status = tt_iordsb.iostat
	      goto 10
	    else
	      ! all OK, verify sector number and checksum
D	      if( debug_on ) then
D	      write(unit=debug_unit,fmt=7020) receive_packet.id,
D    &		receive_packet.secnum, receive_packet.seknum
D7020	      format('Receive_sectr',3Z3.2)
D	      do dbgi = 1, blklen, 16
D		dbga = receive_packet.data(dbgi:dbgi+15)
D		do dbgj = 1, 16
D		  if( dbga(dbgj:dbgj).lt.' ' .or.
D    &			dbga(dbgj:dbgj).gt.'~' )
D    &		    dbga(dbgj:dbgj) = '.'
D		enddo
D		write(unit=debug_unit,fmt=7030)
D    &		  (receive_packet.sector(dbgi+dbgj),dbgj=0,15),dbga
D7030		format(13X,16Z3.2,2X,A16)
D	      enddo
D	      if( BTEST(xfer_mode,CRC_MODE) ) then
D		write(unit=debug_unit,fmt=7040) receive_checksum.crc
D7040		format(10X,'crc',2Z3.2/)
D	      else
D		write(unit=debug_unit,fmt=7050) receive_checksum.cksm
D7050		format(9X,'cksm',Z3.2/)
D	      endif
D	      endif

	      if( receive_packet.secnum .eq. sector_number.b(1) ) then
		! duplicate sector
D		if( debug_on ) call put_line('RECV duplicate sector')
		status = send_char(ACK)
	        if( .not.status ) goto 10
		nak_count = 0
	      else
		sector_number.i4 = sector_number.i4 + 1
		if( (receive_packet.secnum .ne. sector_number.b(1))
     &		 .or. (receive_packet.seknum .ne. 
     &			.not.sector_number.b(1)) ) then
D		  if( debug_on )
D    &		    call put_line('RECV sector number corrupted')
		  sector_number.i4 = sector_number.i4 - 1
		  status = send_nak()
		  if( .not.status ) goto 10
		else
		  status = verify_checksum()
		  if( status ) then
D		    if( debug_on ) call put_line('RECV Acknowledged')
		    status = send_char(ACK)
		    goto 10
		  else
		    status = send_nak()
		    if( .not.status ) goto 10
		  endif
		endif
	      endif
	    endif
	  endif

20	  nak_count = nak_count + 1
	enddo
	status = %loc(VAXMODEM_NAK2MANY)

10	receive_sector = status
	end
	integer*4 function receive_rest_of_packet
	implicit none

	include '($IODEF)'
	include '($SSDEF)'

	include 'VAXMODEM.INC/NOLIST'

	integer*4	status, code, i, SYS$QIOW

	external	SYS$QIOW

C----------------------------------------------------------------------

C
C			*** Read remainder of packet header,
C			*** data and checksum
C

	code = IO$_TTYREADALL.or.IO$M_NOECHO.or.IO$M_TIMED

	! get secnum, seknum
	status = SYS$QIOW(,%val(tt_chan),
     &		%val(code),
     &		tt_iordsb,,,%ref(receive_packet.secnum),
     &		%val(2),
     &		%val(2),		! Timeout granularity is 1 sec
     &		tt_term,,)
	if( .not.status ) goto 10
	if( tt_iordsb.iostat .eq. SS$_TIMEOUT ) goto 10
	if( .not.tt_iordsb.iostat ) goto 10

	! get data SECSIZ bytes at a time
	do i = 1, blklen, SECSIZ
	  status = SYS$QIOW(,%val(tt_chan),
     &		%val(code),
     &		tt_iordsb,,,%ref(receive_packet.sector(i)),
     &		%val(SECSIZ),%val(2),tt_term,,)
	  if( .not.status ) goto 10
	  if( tt_iordsb.iostat .eq. SS$_TIMEOUT ) goto 10
	  if( .not.tt_iordsb.iostat ) goto 10
	end do

	! get checksum
	if( BTEST(xfer_mode,CRC_MODE) ) then
	  i = 2
	else
	  i = 1
	endif
	status = SYS$QIOW(,%val(tt_chan),
     &		%val(code),
     &		tt_iordsb,,,%ref(receive_checksum.cksm),
     &		%val(i),%val(2),tt_term,,)

10	receive_rest_of_packet = status
	return
	end
	integer*4 function receive_path_name(filename)
	implicit none
	character	filename*(*)

	include 'VAXMODEM.INC/NOLIST'

	character	valid*39
	parameter	(valid =
     &			 '$0123456789.ABCDEFGHIJKLMNOPQRSTUVWXYZ_')

	integer*4	status, nulpos, i, j

	integer*4	receive_sector, STR$UPCASE,
     &			STR$FIND_FIRST_NOT_IN_SET

	external	receive_sector, STR$UPCASE,
     &			STR$FIND_FIRST_NOT_IN_SET

C----------------------------------------------------------------------

C
C			*** Get YMODEM pathname packet,
C			*** extract filename and convert
C			*** it to a valid VMS filename
C

D	if( debug_on ) call put_line('Receive path name')
	sector_number.i4 = -1
	status = receive_sector()
	if( status .eq. %loc(VAXMODEM_SUCCESS) ) then
	  ! got pathname
	  nulpos = index(receive_packet.data,char(NUL))
	  if( nulpos.gt.1 ) then
	    nulpos = nulpos - 1
D	    if( debug_on ) call put_line('RCVD pathname '//
D    &		receive_packet.data(1:nulpos))
	    filename = receive_packet.data(1:nulpos)
	    ! filter filename for VMS
	    i = STR$UPCASE(filename,filename)
	    if( .not.i ) then
	      status = i
	      goto 10
	    endif
	    i = STR$FIND_FIRST_NOT_IN_SET(filename(1:nulpos),valid)
	    do while ( i.ne.0 )
	      ! Replace illegal characters with '_'
	      filename(i:i) = '_'
	      i = STR$FIND_FIRST_NOT_IN_SET(filename(1:nulpos),valid)
	    end do
	    j = index(filename(1:nulpos),'.')
	    i = j
	    do while ( j.ne.0 )
	      j = index(filename(i+1:nulpos),'.')
	      i = i + j
	      if( j.gt.0 ) filename(i:i) = '_'
	    end do
	  else
D	    if( debug_on ) call put_line('RCVD null pathname')
	    filename = char(NUL)
	  endif
	else
	  ! error no path name received
D	  if( debug_on )
D    &	    call put_line('RCVD path name receive sector failed')
	  filename = char(NUL)
	endif

10	receive_path_name = status
	end
	integer*4 function verify_checksum
	implicit none

	include 'VAXMODEM.INC/NOLIST'

	integer*4	status, i
	record /int4byte/ c

	integer*4	updcrc
	external	updcrc

C----------------------------------------------------------------------

C
C			*** Compute the checksum or CRC
C			*** of the received packet
C

	cksm.i4 = 0
	crc.i4 = 0
	do i = 1, blklen
	  c.i4 = ichar(receive_packet.data(i:i))
	  cksm.i4 = cksm.i4 + c.b(1)
	  crc.i4 = updcrc(c.b(1),crc.i4)
	enddo
	crc.i4 = updcrc(0,updcrc(0,crc.i4))

	status = %loc(VAXMODEM_SUCCESS)
	if( BTEST(xfer_mode, CRC_MODE) ) then
	  if( ( receive_checksum.crc(1) .ne. crc.b(2) ) .or.
     &		( receive_checksum.crc(2) .ne. crc.b(1) ) )
     &	    status = %loc(VAXMODEM_CRCERROR)
	  goto 999
	else
	  if( cksm.b(1) .ne. receive_checksum.cksm )
     &	    status = %loc(VAXMODEM_BADCHKSUM)
	endif

999	verify_checksum = status
	end
	integer*4 function write_buffer
	implicit none

	include 'VAXMODEM.INC/NOLIST'

	integer*4	status, i, ioerr
	character	c*1

C----------------------------------------------------------------------

C
C			*** Write received data buffer
C			*** to the current receive file
C

	if( BTEST(xfer_mode,SECTOR_MODE) ) then
	  ! SECTOR mode, just output binary 128-byte records
	  ! (no need to do padding with NULs)
	  do i = 1, bufferptr-SECSIZ, SECSIZ
	    write(unit=xfer_unit,err=900,iostat=ioerr)
     &		buffer(i:i+SECSIZ-1)
	  end do
	else
	if( BTEST(xfer_mode,BINARY_MODE) ) then
	  ! BINARY file, pad incomplete disk block with NULs
	  do while ( (bufferptr .le. buffermax) .and.
     &		((bufferptr - (bufferptr/linemax)*linemax) .ne. 1) )
	    buffer(bufferptr:bufferptr) = char(NUL)
	    bufferptr = bufferptr + 1
	  enddo
	  do i = 1, bufferptr-linemax, linemax
	    write(unit=xfer_unit,err=900,iostat=ioerr)
     &		buffer(i:i+linemax-1)
	  end do
	else
	  ! ASCII text file
	  do i = 1, bufferptr-1
	    c = buffer(i:i)
	    ! Strip parity bit if required
	    if( .not.BTEST(xfer_mode,EIGHTBIT_MODE) )
     &	      c = char(ichar(c).and.NO_PARITY)
	    if( c .ne. char(NUL) .and. .not.eof_seen ) then
	      if( c .eq. char(CR) ) then
		if( line_bufferptr .gt. 1 ) then
		  ! write the record
		  write(unit=xfer_unit,fmt='(A)',err=900,iostat=ioerr)
     &		    line_buffer(1:line_bufferptr-1)
		else
		  ! write empty record
		  write(unit=xfer_unit,fmt='()',err=900,iostat=ioerr)
		endif
		line_bufferptr = 1
	      elseif( c .eq. char(CTRL_Z) ) then
		! ASCII end of file seen
		eof_seen = .TRUE.
	      elseif( c .ne. char(LF) ) then
		! all other characters are kept (except linefeeds)
		line_buffer(line_bufferptr:line_bufferptr) = c
		line_bufferptr = line_bufferptr + 1
		if( line_bufferptr .gt. linemax ) then
		  write(unit=xfer_unit,fmt='(A)',err=900,iostat=ioerr)
     &			line_buffer(1:linemax)
		  line_bufferptr = 1
		endif
	      endif
	    endif
	  enddo
	endif
	endif
	status = %loc(VAXMODEM_SUCCESS)
	bufferptr = 1
	goto 999

	! Fortran I/O error, get VMS condition code
900	call errsns(,,,,status)
	
999	write_buffer = status
	end
	integer*4 function send_nak
	implicit none

	include 'VAXMODEM.INC/NOLIST'

	integer*4	status

	integer*4	send_char
	external	send_char

C----------------------------------------------------------------------

	if( BTEST(xfer_mode, CRC_MODE) ) then
	  status = send_char(WANT_CRC)
	else
	  status = send_char(NAK)
	endif
	send_nak = status
	return
	end