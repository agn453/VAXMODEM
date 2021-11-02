C
C			*** VAXMODEM include file
C

	character*5	version
	parameter	(version = 'V2.0E')

	! Message signalling definitions

	integer*4	VAXMODEM_SUCCESS
     &			,VAXMODEM_RECVDEOF
     &			,VAXMODEM_DEBUGON
     &			,VAXMODEM_DEBUGOFF
     &			,VAXMODEM_DBGACTIV
     &			,VAXMODEM_RCVTMOSET
     &			,VAXMODEM_FILNOTSEQ
     &			,VAXMODEM_SENDABORT
     &			,VAXMODEM_NAK2MANY
     &			,VAXMODEM_SNDREADY
     &			,VAXMODEM_RCVREADY
     &			,VAXMODEM_SENDCAN
     &			,VAXMODEM_RECVCAN
     &			,VAXMODEM_NONAK
     &			,VAXMODEM_NOEOTACK
     &			,VAXMODEM_CRCERROR
     &			,VAXMODEM_BADCHKSUM
     &			,VAXMODEM_NAMTOOBIG
     &			,VAXMODEM_RECTOOBIG
     &			,VAXMODEM_RCVSECTOR
	external	VAXMODEM_SUCCESS
     &			,VAXMODEM_RECVDEOF
     &			,VAXMODEM_DEBUGON
     &			,VAXMODEM_DEBUGOFF
     &			,VAXMODEM_DBGACTIV
     &			,VAXMODEM_RCVTMOSET
     &			,VAXMODEM_FILNOTSEQ
     &			,VAXMODEM_SENDABORT
     &			,VAXMODEM_NAK2MANY
     &			,VAXMODEM_SNDREADY
     &			,VAXMODEM_RCVREADY
     &			,VAXMODEM_SENDCAN
     &			,VAXMODEM_RECVCAN
     &			,VAXMODEM_NONAK
     &			,VAXMODEM_NOEOTACK
     &			,VAXMODEM_CRCERROR
     &			,VAXMODEM_BADCHKSUM
     &			,VAXMODEM_NAMTOOBIG
     &			,VAXMODEM_RECTOOBIG
     &			,VAXMODEM_RCVSECTOR

	! File transfer mode bit definitions

	integer*4	BATCH_MODE, BINARY_MODE, STRIP_CR_MODE,
     &			STRIP_LF_MODE, ASYNC86_MODE, CRC_MODE,
     &			EIGHTBIT_MODE, SECTOR_MODE
	parameter	(BATCH_MODE    = 0)	! Batch file transfer
	parameter	(BINARY_MODE   = 1)	! Binary file
	parameter	(STRIP_CR_MODE = 2)	! Ignore carriage returns
	parameter	(STRIP_LF_MODE = 3)	! Ignore linefeeds
	parameter	(ASYNC86_MODE  = 4)	! NEC Async-86
	parameter	(CRC_MODE      = 5)	! Use CRC instead of checksum
	parameter	(EIGHTBIT_MODE = 6)	! Allow 8-bit ASCII
	parameter	(SECTOR_MODE   = 7)	! Binary 128 byte sector

	! Global constants

	integer*4	SECSIZ			! CP/M sector size
	parameter	(SECSIZ = 128)
	integer*4	KSIZE			! YMODEM large packet size
	parameter	(KSIZE = 1024)
	integer*4	linemax			! Maximum VMS record size
	parameter	(linemax = 512)
	integer*4	buffermax		! size of internal buffer
	parameter	(buffermax = 16384)
	integer*4	retrymax		! maximum number of retries
	parameter	(retrymax = 10)

	! Special characters

	byte		NUL
	parameter	(NUL = 0)
	byte		SOH
	parameter	(SOH = 1)
	byte		STX
	parameter	(STX = 2)
	byte		EOT
	parameter	(EOT = 4)
	byte		ACK
	parameter	(ACK = 6)
	byte		CR
	parameter	(CR = 13)
	byte		LF
	parameter	(LF = 10)
	byte		NAK
	parameter	(NAK = 21)
	byte		CAN
	parameter	(CAN = 24)
	byte		CTRL_C
	parameter	(CTRL_C = 3)
	byte		CTRL_Y
	parameter	(CTRL_Y = 25)
	byte		CTRL_Z
	parameter	(CTRL_Z = 26)
	byte		WANT_CRC
	parameter	(WANT_CRC = 67)
	byte		NO_PARITY
	parameter	(NO_PARITY = 127)

	! Structure definitions

	structure /iostat_block/		! I/O status block for $QIO
	  integer*2	iostat
	  union
	    map
	      byte	transmit,receive,crfill,lffill,parity,zero
	    end map
	    map
	      integer*2	byte_count, lines_output
	      byte	column, line
	    end map
	    map
	      integer*2	ofs_term, terminator, term_size
	    end map
	  end union
	end structure

	structure /characteristics/		! TT characteristics
	  byte		class,type
	  integer*2	width
	  union
	    map
	      integer*4	basic
	    end map
	    map
	      byte	length(4)
	    end map
	  end union
	  integer*4	extended
	end structure

	structure /exit_descriptor/		! Exit handler block
	  integer*4	link, addr, args /0/, status
	end structure

	structure /xymodem_packet/		! packet header and data
	  union
	    map
	      byte	id
	    end map
	    map
	      byte	reply
	    end map
	  end union
	  byte		secnum, seknum
	  union
	    map
	      byte	sector(KSIZE)
	    end map
	    map
	      character	data*(KSIZE)
	    end map
	  end union
	end structure

	structure /checksum/			! packet checksum
	  union
	    map
	      byte	cksm			! additive checksum
	      byte	necnak			! special byte for ASYNC86
	    end map
	    map
	      byte	crc(2)			! CRC
	    end map
	  end union
	end structure

	structure /int4byte/			! longword to byte equivalence
	  union
	    map
	      integer*4	i4
	    end map
	    map
	      byte	b(4)
	    end map
	  end union
	end structure

	! Global variables

	logical		eof_seen
	integer*4	xfer_mode, timeout / 3 /, line_bufferptr,
     &			bufferptr, replyflag, blklen / SECSIZ /,
     &			xfer_unit, rcv_timeout, tt_term(2) / 0, 0 /
	integer*2	tt_chan
	character	line_buffer*(linemax), buffer*(buffermax)
	record /iostat_block/	tt_iosb, tt_iordsb
	record /characteristics/tt_binary_char, tt_normal_char
	record /exit_descriptor/exit_block
	record /xymodem_packet/	send_packet, receive_packet
	record /int4byte/ cksm, crc, sector_number
	record /checksum/ send_checksum, receive_checksum

D	logical		debug_on / .FALSE. /
D	integer*4	debug_unit, debug_old_handler
D	integer*2	debug_filename_size
D	character	debug_filename*128

	common /VAXMODEM_COMMON/ xfer_mode	! transfer mode
     &			,eof_seen		! received ASCII CTRL-Z
     &			,timeout		! default packet timeout
     &			,rcv_timeout		! receive packet timeout
     &			,line_bufferptr		! line buffer pointer
     &			,bufferptr		! buffer pointer
     &			,replyflag		! reply I/O event flag
     &			,sector_number		! packet sector number
     &			,cksm			! checksum
     &			,crc			! cyclic redundancy check
     &			,line_buffer		! ASCII text line buffer
     &			,buffer			! send/receive buffer
     &			,blklen			! size of packet data
     &			,send_packet		! send packet
     &			,receive_packet		! received packet
     &			,send_checksum		! send checksum
     &			,receive_checksum	! receive checksum
     &			,tt_chan		! TT channel number
     &			,tt_iosb		! TT I/O status block
     &			,tt_iordsb		! TT I/O read status block
     &			,tt_binary_char		! TT binary mode characteristics
     &			,tt_normal_char		! TT normal mode characteristics
     &			,tt_term		! TT terminator control block
     &			,exit_block		! exit handler block
     &			,xfer_unit		! logical unit for transfers
D    &			,debug_unit		! debug log file unit
D    &			,debug_old_handler	! saved condition handler
D    &			,debug_on		! debug on/off
D    &			,debug_filename_size	! debug filename length
D    &			,debug_filename		! debug filename