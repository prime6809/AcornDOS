;
; i8271 Equates
;

ICMD_WRITE_MULTI	= $0B				; Write multiple sectors
ICMD_READ_MULTI		= $13				; Read multiple sectors
ICMD_VERIFY_MULTI	= $1F				; verify track multi record
ICMD_FORMAT			= $23				; Format track
ICMD_SEEK			= $29				; Seek to track
ICMD_READ_STATUS	= $2C				; Read drive status
ICMD_INIT_8271		= $35				; init 8271
ICMD_WRITE_SPEC		= $3A				; Write special register

; Special registers

ISR_SCAN_SEC	= $06					; Scan sectors
ISR_BAD1_0		= $10					; Bad track 1 drive 0
ISR_BAD2_0		= $11					; Bad track 2 drive 0
ISR_TRACK_0		= $12					; Track register drive 0
ISR_SCANC_LSB	= $13					; Scan count LSB
ISR_SCANC_MSB	= $14					; Scan count MSB
ISR_DMA_MODE	= $17					; DMA mode, not used on Atom
ISR_BAD1_1		= $18					; Bad track 1 drive 1
ISR_BAD2_1		= $19					; Bad track 2 drive 1
ISR_TRACK_1		= $1A					; Track register drive 1
ISR_DRV_CTL_IN	= $22					; Drive control input
ISR_DRV_CTL_OUT	= $23					; Drive control output

; The drive specifier is or'd with the command
ICMD_DRIVE0		= $40					; Physical drive 0, logical 0 & 2
ICMD_DRIVE1		= $80					; Physical drive 1, logical 1 & 3

; Record lengths for format, or'd with format command.
IREC_LEN0		= $00					; 128 bytes / sector
IREC_LEN1		= $20					; 256 bytes / sector
IREC_LEN2		= $40					; 512 bytes / sector
IREC_LEN3		= $50					; 1024 bytes / sector
IREC_LEN4		= $80					; 2048 bytes / sector
IREC_LEN5		= $A0					; 4096 bytes / sector
IREC_LEN6		= $C0					; 8192 bytes / sector
IREC_LEN7		= $E0					; 16384 bytes / sector
