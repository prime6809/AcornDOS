#
# Makefile for utility programs WD1770 FDC.
#
# This requires a version of BeebASM that supports command line defines with -D 
# BeebASM is available from : 
# http://www.retrosoftware.co.uk/wiki/index.php?title=BeebAsm
#

include ../vars.mk

# Main target build everything
all: utils format 

# Utility commands are FDC independednt, but must be linked against
# the appropreate version of the rom so entry points are correct.
# Format and Verify commands ARE dependent on the FDC and the rom 
# version.
utils: backup compact copy copyf duty 
format: format1770 verify1770 

#
# Utility commands
#
backup: $(SRCPATH)/backup.asm $(INCLUDES) $(ROMSRC)
	$(ASM) -i $(SRCPATH)/backup.asm -D ISROM=0 -D WD1770=1 -v > $(LIST)/backup-1770.txt

compact: $(SRCPATH)/compact.asm $(INCLUDES) $(ROMSRC)
	$(ASM) -i $(SRCPATH)/compact.asm -D ISROM=0 -D WD1770=1 -v > $(LIST)/compact-1770.txt
	
copy: $(SRCPATH)/copy.asm $(INCLUDES) $(ROMSRC)
	$(ASM) -i $(SRCPATH)/copy.asm -D ISROM=0 -D WD1770=1 -v > $(LIST)/copy-1770.txt
	
copyf: $(SRCPATH)/copyf.asm $(INCLUDES) $(ROMSRC)
	$(ASM) -i $(SRCPATH)/copyf.asm -D ISROM=0 -D WD1770=1 -v > $(LIST)/copyf-1770.txt

duty: $(SRCPATH)/duty.asm $(INCLUDES) $(ROMSRC)
	$(ASM) -i $(SRCPATH)/duty.asm -D ISROM=0 -D WD1770=1 -v > $(LIST)/duty-1770.txt

infall: $(SRCPATH)/infall.asm $(INCLUDES) $(ROMSRC)
	$(ASM) -i $(SRCPATH)/infall.asm -D ISROM=0 -D WD1770=1 -v > $(LIST)/infall-1770.txt

format1770: $(SRCPATH)/format.asm $(INCLUDES) $(ROMSRC)
	$(ASM) -i $(SRCPATH)/format.asm -D FORMAT=1 -D TRACKS=40 -D ISROM=0 -v > $(LIST)/form40-1770.txt
	$(ASM) -i $(SRCPATH)/format.asm -D FORMAT=1 -D TRACKS=80 -D ISROM=0 -v > $(LIST)/form80-1770.txt
	
verify1770: $(SRCPATH)/format.asm $(INCLUDES) $(ROMSRC)
	$(ASM) -i $(SRCPATH)/format.asm -D FORMAT=0 -D TRACKS=40 -D ISROM=0 -v > $(LIST)/verify40.txt
	$(ASM) -i $(SRCPATH)/format.asm -D FORMAT=0 -D TRACKS=80 -D ISROM=0 -v > $(LIST)/verify80.txt
	
clean:
	rm -f *.DFS	
