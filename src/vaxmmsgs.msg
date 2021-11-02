.TITLE		VMODEMSGS
.FACILITY	VAXMODEM,1
.IDENT		'Version 2.0E'
.SEVERITY SUCCESS
	SUCCESS		"Operation successful."
	RECVDEOF	"Received end of file."
.SEVERITY INFORMATIONAL
	DEBUGON		"Debug output is to file !AS"/FAO_COUNT=1
	DEBUGOFF	"Debug output disabled."
	DBGACTIV	"Debug output already active to file !AS"/FAO_COUNT=1
	RCVTMOSET	"Receive timeout set to !SL seconds."/FAO_COUNT=1
	SNDREADY	"Ready to send, start downloading now."
	RCVREADY	"Ready to receive, start uploading now."
	RCVSECTOR	"Binary 128 byte sector receive mode."
.SEVERITY WARNING
	NOEOTACK	"<EOT> not acknowledged."
.SEVERITY ERROR
	FILNOTSEQ	"File is not sequential, transfer aborted."
	NAMTOOBIG	"Filename too big, transfer aborted."
	RECTOOBIG	"Send file record too big, transfer aborted."
	SENDCAN		"Send packet cancelled, transfer aborted."
	RECVCAN		"Receive packet cancelled, transfer aborted."
	NONAK		"No <NAK> received, transfer aborted."
	NAK2MANY	"Too many <NAK>s or retries, transfer aborted."
	SENDABORT	"Send interrupted, file transfer aborted."
	CRCERROR	"CRC error in received sector."
	BADCHKSUM	"Bad checksum received."
.END