		include "../src/platinclude.asm"
		include "../src/wdfdc.asm"
		include "../src/intelfdc.asm"

if(ATOM=1)
		BASE	= $2900
else
		BASE	= $2800
endif
		org     BASE

BufBASE		= BASE+$0400		; buffer used for copying sectors
BufMSB		= >BufBASE			; High byte (page)
BufLSB		= <BufBASE			; Low byte offset

SrcDrive	= BASE+$03FE					; saved source drive
DstDrive	= BASE+$03FF					; saved dest drive
SaveSrcCat	= BASE+$0400					; saved source drive catalog

.BeebDisStartAddr
        JSR     INLINE_PRINT				; Signon message 

        EQUB    $0C
if(ATOM=1)
        EQUS    "DUTY PROGRAM"
else
        EQUS    "Duty Program"
endif
        EQUB    $0A

        NOP
        JSR     INLINE_PRINT				; prompt for source

        EQUB    $0A
        EQUB    $0D
if(ATOM=1)
        EQUS    "FROM DRIVE ?"
else
		EQUS    "From drive ?"
endif
        NOP
        JSR     GET_DRIVENO					; get source driveno

        STA     SrcDrive
        JSR     INLINE_PRINT				; prompt for destination drive


        EQUB    $0D
        EQUB    $0A
if(ATOM=1)
        EQUS    "  TO DRIVE ?"
else
        EQUS    "  to drive ?"
endif
        NOP
        JSR     GET_DRIVENO					; get dest driveno

        STA     DstDrive
        
		LDA     SrcDrive					; Load source drive catalog
        STA     DRIVENO
        JSR     load_cat

        JSR     WAIT_NOT_BUSY				; Wait for it to complete

        LDY     #$00
.L284F
        LDA     WKFileName1,Y				; Copy source catalog into buffer at $2C00
        STA     SaveSrcCat,Y
        LDA     WKLoadAddr1,Y
        STA     SaveSrcCat+$100,Y
        INY
        BNE     L284F

        LDY     WKFileCount					; get file count of first drive
        STY     FILENAME+5					; save it
        
		LDA     DstDrive					; load destination drive catalog
        STA     DRIVENO
        JSR     load_cat

        JSR     WAIT_NOT_BUSY				; Wait for it to complete

        LDA     WKFileCount					; get destination file count
        CLC									; clear carry for add
        ADC     FILENAME+5					; add source count
        BCS     JMP_ERROR_QUIT				; carry, more files than can fit on disk, error

        CMP     #$F8						; max files 
        BCS     JMP_ERROR_QUIT				; yep error : quit

        TAY									; filecount in Y
        LDX     WKFileCount					; Files on dest disk
        BNE     L2895						; non zero : 

        STA     WKFileCount
        LDA     #$02
        STA     FILENAME+3
        STA     FILENAME+1
        LDA     #$00
        STA     FILENAME+4
        STA     FILENAME+2
        JMP     L28D6

.JMP_ERROR_QUIT
        JMP     ERROR_QUIT

.L2895
        STA     WKFileCount
        LDA     #$FF
        CLC
        ADC     WKLength1
        LDA     #$00
        ADC     WKLength1+1
        STA     STARTSEC+2
        LDA     WKStartSec1
        
		PHP
        LSR     A							; a=a/16
        LSR     A
        LSR     A
        LSR     A
        PLP
        
		ADC     #$00
        STA     FILENAME
        LDA     WKStartSec1+1
        ADC     STARTSEC+2
        STA     FILENAME+3
        STA     FILENAME+1
        LDA     WKStartSec1
        AND     #$0F
        ADC     FILENAME
        STA     FILENAME+4
        STA     FILENAME+2
.L28C6
        LDA     WKTitle+7,X
        STA     WKTitle+7,Y
        LDA     WKSecCLSB,X
        STA     WKSecCLSB,Y
        DEY
        DEX
        BNE     L28C6

.L28D6
        LDY     FILENAME+5
.L28D8
        LDA     SaveSrcCat+$FF,Y
        STA     WKSecCLSB,Y
        LDA     DstDrive,Y
        STA     WKTitle+7,Y
        DEY
        BNE     L28D8

        LDY     FILENAME+5
.L28E9
        JSR     decy8

        CPY     #$F8
        BEQ     L2939

        LDA     #$FF
        ADC     WKLength1,Y
        LDA     #$00
        ADC     WKLength1+1,Y
        STA     STARTSEC+2
        LDA     WKStartSec1,Y
        PHP
        LSR     A
        LSR     A
        LSR     A
        LSR     A
        PLP
        ADC     #$00
        STA     FILENAME
        LDA     FILENAME+1
        ADC     STARTSEC+2
        STA     FILENAME+1
        LDA     FILENAME+2
        ADC     FILENAME
        STA     FILENAME+2
        LDA     WKSecCOpt
        AND     #$0F
        CMP     FILENAME+2
        BCC     ERROR_QUIT

        BNE     L2939

        LDA     FILENAME+1
        CMP     WKSecCLSB
        BCC     L28E9

.ERROR_QUIT
        JSR     INLINE_PRINT				; Print unable to copy error and quit
if(ATOM=1)
        EQUS    "UNABLE TO COPY"
else
        EQUS    "Unable to Copy"
endif
        BRK

.L2939
        LDY     FILENAME+5
        JSR     decy8

        CPY     #$F8
        BNE     L2945

        JMP     L2A0D

.L2945
        STY     FILENAME+5
        JSR     print_info

        LDY     FILENAME+5
        LDA     WKStartSec1,Y
        AND     #$F0
        ORA     WKLength1+1,Y
        ORA     WKLength1,Y
        BNE     L295C

        JMP     L29FF

.L295C
        LDA     #$00
        STA     LOADADDR
        STA     FILESIZE
        LDA     #$FF
        CLC
        ADC     WKLength1,Y
        LDA     #$00
        ADC     WKLength1+1,Y
        STA     STARTSEC+2
        LDA     WKStartSec1,Y
        PHP
        LSR     A
        LSR     A
        LSR     A
        LSR     A
        PLP
        ADC     #$00
        STA     FILENAME
        LDA     WKStartSec1+1,Y
        STA     FILENAME+1
        LDA     WKStartSec1,Y
        AND     #$0F
        STA     FILENAME+2
        LDA     FILENAME+3
        STA     WKStartSec1+1,Y
        LDA     WKStartSec1,Y
        AND     #$F0
        ORA     FILENAME+4
        STA     WKStartSec1,Y
.L2997
        LDA     STARTSEC+2
        TAY
        CMP     #$14
        LDA     FILENAME
        SBC     #$00
        BCC     L29A4

        LDY     #$14
.L29A4
        STY     FILESIZE+1
        LDA     FILENAME+1
        STA     STARTSEC+1
        LDA     FILENAME+2
        STA     STARTSEC
        LDA     #BufMSB
        STA     LOADADDR+1
        LDA     SrcDrive
        STA     DRIVENO
        JSR     START_MOTOR_SELECT

        JSR     ROM_READ_SECTORS

        JSR     WAIT_NOT_BUSY

        LDA     FILENAME+3
        STA     STARTSEC+1
        LDA     FILENAME+4
        STA     STARTSEC
        LDA     #BufMSB
        STA     LOADADDR+1
        LDA     DstDrive
        STA     DRIVENO
        JSR     START_MOTOR_SELECT

        JSR     ROM_WRITE_SECTORS

        JSR     WAIT_NOT_BUSY

        LDA     FILESIZE+1
        CLC
        ADC     FILENAME+3
        STA     FILENAME+3
        BCC     L29E5

        INC     FILENAME+4
.L29E5
        LDA     FILESIZE+1
        CLC
        ADC     FILENAME+1
        STA     FILENAME+1
        BCC     L29F0

        INC     FILENAME+2
.L29F0
        SEC
        LDA     STARTSEC+2
        SBC     FILESIZE+1
        STA     STARTSEC+2
        BCS     L29FB

        DEC     FILENAME
.L29FB
        ORA     FILENAME
        BNE     L2997

.L29FF
        JSR     write_cat

        JSR     WAIT_NOT_BUSY

        LDY     FILENAME+5
        JSR     print_info

        JMP     L2939

.L2A0D
        JSR     INLINE_PRINT
if(ATOM=1)
        EQUS    "COPY COMPLETED "
else
        EQUS    "Copy completed "
endif

.L2A1F
        NOP
        SEC
        LDA     WKSecCLSB
        SBC     FILENAME+3
        PHA
        LDA     WKSecCOpt
        AND     #$0F
        SBC     FILENAME+4
        JSR     PRINT_HEXA_LOWN

        PLA
        JSR     PRINT_HEXA

        JSR     INLINE_PRINT
if(ATOM=1)
        EQUS    " FREE SECTORS"
else
        EQUS    " free sectors"
endif
        NOP
        JMP     OSCRLF						; Exit after OSCRLF

.GET_DRIVENO
        JSR     READ_LINE					; Read a line of input from keyboard to stack page

        LDA     STACKPAGE+1						; second character entered
        CMP     #$0D						; return?
        BNE     L2A61						; nope : print error

        LDA     STACKPAGE					; get first character read
        CMP     #$30						; is is < '0'
        BCC     L2A61						; yep : print error

        CMP     #$34						; is it > '3'
        BCS     L2A61						; yep : print error

        AND     #$03						; Convert to binary drive no 0..3
        RTS

.L2A61
        JSR     INLINE_PRINT				; print error
if(ATOM=1)
        EQUS    "TRY AGAIN! (0-3) "
else
        EQUS    "try again! (0-3) "
endif
		NOP
        JMP     GET_DRIVENO						; loop again

.BeebDisEndAddr

SAVE "DUTY.DFS",BeebDisStartAddr,BeebDisEndAddr

;
; Here we include and assemble the system rom, this way we can access it's symbols
; however the assembled copy is not saved (as ISROM=0)
;
        include "..\src\rominclude.asm"
