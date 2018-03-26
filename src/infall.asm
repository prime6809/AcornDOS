		include "../src/sysdefs.asm"
       	include "../src/wdfdc.asm"
		include "../src/intelfdc.asm"

        org     $2800
.BeebDisStartAddr
        JSR     READ_CAT				; read disk catalog

        LDY     WKFileCount				; get file count
.L2806
        JSR     decy8					; point to next file

        STY     TEXTPTR					; point to it
        JSR     print_info				; print file info

        LDY     TEXTPTR
        BNE     L2806					; loop if more

        RTS

.BeebDisEndAddr

SAVE "INFALL.DFS",BeebDisStartAddr,BeebDisEndAddr

;
; Here we include and assemble the system rom, this way we can access it's symbols
; however the assembled copy is not saved (as ISROM=0)
;
        SYS40   = 1
if (WD1770)		
        include "../src/sys5-1f-1770.asm"
else
        include "../src/sys5-1f.asm"
endif
