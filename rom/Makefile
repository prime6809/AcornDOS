#
# Makefile for System ROMS and utility programs.
#
# This requires a version of BeebASM that supports command line defines with -D 
# BeebASM is available from : 
# http://www.retrosoftware.co.uk/wiki/index.php?title=BeebAsm
#

include ../vars.mk

# roms, Build 40 and 80 column variants of system roms with both 
# floppy controllers
all: sysrom40 sysrom80 sysrom-wd40 sysrom-wd80 

# roms 
sysrom40: $(ROMSRC) $(INCLUDES)
	$(ASM) -i $(SRCPATH)/sys5-1f.asm -D SYS40=1 -D ISROM=1 -v > $(LIST)/sys5-1f-40.txt

sysrom80: $(ROMSRC) $(INCLUDES)
	$(ASM) -i $(SRCPATH)/sys5-1f.asm -D SYS40=0 -D ISROM=1 -v > $(LIST)/sys5-1f-80.txt

sysrom-wd40: $(ROMSRC) $(INCLUDES)
	$(ASM) -i $(SRCPATH)/sys5-1f-1770.asm -D SYS40=1 -D ISROM=1 -v > $(LIST)/sys5-1770-40.txt

sysrom-wd80: $(ROMSRC) $(INCLUDES)
	$(ASM) -i $(SRCPATH)/sys5-1f-1770.asm -D SYS40=0 -D ISROM=1 -v > $(LIST)/sys5-1770-80.txt

clean:
	rm -f sys5-1770-*.rom sys5-8271-*.rom
	
