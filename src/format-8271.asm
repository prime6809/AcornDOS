;
; System format / verify for 8271 FDC
;
; The original system did not have a seperate verify so I have created one from part 
; of the format code, to remain inline with the 1770 implementation.
; 

		include "../src/sysdefs.asm"
       	include "../src/intelfdc.asm"

        org     $2800
.codestart
if(FORMAT)
        JSR     INLINE_PRINT				; Print confirmation message

        EQUB    $0D
        EQUS    "Do you really want to format drive "

        LDA     DRIVENO						; print drive number
        JSR     PRINT_HEXA_LOWN

        JSR     INLINE_PRINT				; Print " ?"
		
        EQUS    " ?"

		NOP

        JSR     OSECHO						; read a character
        CMP     #$59						; check for 'Y'
        BNE     L2847						; no : exit

        JSR     OSECHO						; read a character
        CMP     #$45						; check for 'E'
        BNE     L2847						; no : exit

        JSR     OSECHO						; read a character
        CMP     #$53						; check for 'S'
        BEQ     DO_FORMAT					; yep : go do it

.L2847
        JSR     OSCRLF						; print EOL and exit to dos
        RTS

.DO_FORMAT
        JSR     OSCRLF						; print EOL 	
        JSR     OSCRLF						; print EOL 

        JSR     SELECT_WAIT_READY			; select drive, wait for it to come up to speed

        JSR     WAIT_NOT_BUSY				; Wait for drive ready

        LDA     #<L28EE						; setup NMI jump
        STA     INTJMP
        LDA     #>L28EE
        STA     INTJMP+1
		
        LDX     #$00
        STX     MOTOR_ON
.L2863
        STX     TRACKNO						; set track no
        JSR     copy_for_write				; copy NMI code into RAM

        LDY     #$04
.L286A
        JSR     L2969

        DEY
        BNE     L286A

        LDY     #$27
.L2872
        LDA     #$01
        STA     WKTitle,Y
        DEY
        JSR     L2969

        STA     WKTitle,Y
        DEY
        LDA     #$00
        STA     WKTitle,Y
        DEY
        LDA     TRACKNO
        STA     WKTitle,Y
        DEY
        BPL     L2872

        LDA     #ICMD_FORMAT+ICMD_DRIVE0	; Send format command to drive
        JSR     fdc_send_cmd_drv

        LDA     TRACKNO						; send track number
        JSR     SEND_PARAM_BYTE

        LDA     #$10						; Gap 3 length (-6, see data sheet)
        JSR     SEND_PARAM_BYTE

        LDA     #IREC_LEN1+SECTRACK			; record length (256) + no sectors / track
        JSR     SEND_PARAM_BYTE

        LDA     #$00						; Gap 5 length (-6, see data sheet) 
        JSR     SEND_PARAM_BYTE

        LDA     #$10						; Gap 1 length (-6, see data sheet)
        JSR     SEND_PARAM_BYTE

        JSR     WAIT_NOT_BUSY				; Wait for command completion

        JSR     get_fdc_result				; get result

        BNE     L28DF

        LDX     TRACKNO						; get current track number
        INX									; increment it
        CPX     #TRACKS						; done all?
        BNE     L2863						; no : loop again

        JSR     INLINE_PRINT

        EQUB    $0D
        EQUS    "Formatted"
        EQUB    $0A
        EQUB    $0D

        NOP
        LDA     #$00						; setup file count =0 
        STA     WKFileCount					
if(TRACKS=40)
		lda		#>TotalTrk40				; set no of tracks
		sta		WKSecCOpt
		lda		#<TotalTrk40
		sta		WKSecCLSB
else
		lda		#>TotalTrk80				; set no of tracks
		sta		WKSecCOpt
		lda		#<TotalTrk80
		sta		WKSecCLSB
endif		
;        LDA     #$03						; Setup default opt
;        STA     WKSecCOpt
;        LDA     #$20						; setup default qualifier
;        STA     WKSecCLSB
        JSR     write_cat					; write blank catalog to disk

        JMP     L28EF

.L28DF
        JSR     PRINT_HEXA
        JSR     INLINE_PRINT

        EQUB    $0D
        EQUS    "Crunch"
        NOP
        BRK
else
        JMP     L28EF
endif
.L28EE
        EQUB    $60


; Verify code starts here

.L28EF
        JSR     SELECT_WAIT_READY			; wait for drive

        LDA     #<L28EE						; setup NMI jump
        STA     INTJMP
        LDA     #>L28EE
        STA     INTJMP+1

        LDA     #$00
        STA     TRACKNO
.L28FE
        JSR     PRINT_HEXA					; print track no
        JSR     PRSPACE
        JSR     PRSPACE

        LDY     #$00						; Zero sector no
.L2909
        STY     SECTORNO
        LDA     #$0A
        STA     RETRYCOUNT
.L290F
        LDA     #ICMD_VERIFY_MULTI+ICMD_DRIVE0	; Verify data command
        JSR     fdc_send_cmd_drv			; send to FDC

        LDA     TRACKNO						; Send track number
        JSR     SEND_PARAM_BYTE

        LDA     SECTORNO					; send sector number
        JSR     SEND_PARAM_BYTE

        LDA     #IREC_LEN1+$01				; record length (256 bytes), verify 1 sector
        JSR     SEND_PARAM_BYTE

        LDY     SECTORNO					; get sector no
        CPY     #$09						; done all 9?
        BNE     L2929						; get result.....will do this always!

.L2929
        JSR     get_fdc_result				; get result from FDC
        BNE     DO_RETRY					; Error : retry

        INY									; increment sector no
        CPY     #SECTRACK					; done all?
        BNE     L2909						; nope : loop again

        INC     TRACKNO						; increment track no
        LDA     TRACKNO						; done all tracks?
        CMP     #TRACKS
        BNE     L28FE						; nope : loop again						

        JSR     OSCRLF						; print EOL
        JSR     INLINE_PRINT				; print verified message

        EQUS    "Verified"
        EQUB    $0A,$0D

.L294B
        NOP
        RTS

.DO_RETRY
        DEC     RETRYCOUNT					; decrement retry count
        BNE     L290F						; exhausted retries? nope : loop again

        JSR     PRINT_HEXA					
        JSR     INLINE_PRINT				; print error message

        EQUB	$0A
        EQUS    "Failure at "

        LDA     SECTORNO					; get sector no
        JSR     PRINT_HEXA					; print it
		BRK									; exit

.L2969
        LDX     MOTOR_ON
        DEX
        BPL     L2970

        LDX     #$09					; sector no?
.L2970
        TXA
        STX     MOTOR_ON
        RTS

.SELECT_WAIT_READY
        JSR     START_MOTOR_SELECT			; start motor and select drive
.L2977
        JSR     TESTREADY					; wait for it to become ready
        BEQ     L2977

        RTS

.codeend

print "TRACKS=",TRACKS
print "FORMAT=",FORMAT

if(FORMAT)
  if(TRACKS=40)
    save "FORM40.DFS",codestart,codeend,codestart
  else
    save "FORM80.DFS",codestart,codeend,codestart
  endif
else
  if(TRACKS=40)
    save "VERIFY4.DFS",codestart,codeend,codestart
  else
    save "VERIFY8.DFS",codestart,codeend,codestart
  endif    
endif  

;
; Here we include and assemble the system rom, this way we can access it's symbols
; however the assembled copy is not saved (as ISROM=0)
;
        SYS40   = 1
		INCLUDEVDG=0
        include "../src/sys5-1f.asm"
