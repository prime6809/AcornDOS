; WD 179x, 177x, 279x defines

; Register offsets

WDCMD		= 0							; Command register, write only
WDSTATUS	= 0							; Status register, read only
WDTRACK		= 1							; Track register 
WDSECTOR	= 2							; Sector register
WDDATA		= 3							; Data register

; Commands, top 3 or 4 bits gives command no
; bottom nibble may set parameters

; Type 1
WCMD_RESTORE	= $00						; Restore to track 0
WCMD_SEEK		= $10						; Seek to a track
WCMD_STEP		= $20						; Step
WCMD_STEPIN		= $40						; Step in
WCMD_STEPOUT	= $60						; Step out

; Type 2
WCMD_READ_SEC	= $80						; Read sector(s)
WCMD_WRITE_SEC	= $A0						; Write sector(s)

; Type 3
WCMD_READ_ADDR	= $C0						; Read address
WCMD_READ_TRACK	= $E0						; Read a track
WCMD_WRITE_TRACK= $F0						; Write a track

; Type 4
WCMD_FORCE_INT	= $D0						; Force Interrupt

; Command flags to be or'd with the above commands

; Type 1
WCF_VERIFY		= $04						; Verify destination track
WCF_RATE6		= $00						; 6ms / track step
WCF_RATE12		= $01						; 12ms / track step
WCF_RATE20		= $02						; 20ms / track step, 2ms on 1772
WCF_RATE30		= $03						; 30ms / track step, 3ms on 1772
WCF_UPDATE		= $10						; Update track register on step

; Type 1,2 & 3
WCF_NO_SPINUP	= $08						; Disable spinup sequence

; Type 2 & 3
WCF_MULTIPLE	= $10						; Read or Write multiple sectors
WCF_WRITE_DEL	= $01						; Write deleted address mark
WCF_DELAY		= $04						; 30ms head settle delay, 15ms on 1772
WCF_NO_PRECOMP	= $01						; Disable write precomp

; Type 4
WCF_INT_INDEX	= $04						; Interrupt on index pulse
WCF_INT_NOW		= $08						; Immediate interrupt
WCF_NO_INT		= $09						; Terminate without interrupt

; Status register bits

WST_MOTOR_ON	= $80						; Motor is on, status of MOTOR output
WST_WP			= $40						; Disk write protected
WST_SPINUP		= $20						; When set indicates spinup has completed (Type 1)
WST_REC_TYPE	= $20						; Record type, 0 = Data, 1 = Deleted data (Type 2,3)
WST_RNF			= $10						; Record not found
WST_CRC			= $08						; CRC Error, if ST_RNF set, error in address mark, if clear in data.
WST_TR00		= $04						; Track 0 (Type 1)
WST_LOST		= $04						; Lost data (Type 2,3)
WST_INDEX		= $02						; Status of index pules input (Type 1)
WST_DRQ			= $02						; Status of DRQ line (Type 2,3)
WST_BUSY		= $01						; Controller is busy.
		