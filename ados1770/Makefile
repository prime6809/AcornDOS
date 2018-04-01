#
# Makefile for Atom utility programs WD1770 FDC.
#
# This requires a version of BeebASM that supports command line defines with -D 
# BeebASM is available from : 
# http://www.retrosoftware.co.uk/wiki/index.php?title=BeebAsm
#

include ..\vars.mk

DEFS=-D ATOM=1 -D ISROM=0 -D WD1770=1

# Main target build everything
all: utils 

# Utility commands are FDC independednt, but must be linked against
# the appropreate version of the rom so entry points are correct.
# Format and Verify commands are included in ados1770 so don't need
# to be built here
utils: backup compact copy copyf duty sysinfo

#
# Utility commands
#
backup: $(SRCPATH)/backup.asm $(INCLUDES) $(ROMSRC)
	$(ASM) -i $(SRCPATH)/backup.asm $(DEFS) -v > $(LIST)/backup-1770-atom.txt

compact: $(SRCPATH)/compact.asm $(INCLUDES) $(ROMSRC)
	$(ASM) -i $(SRCPATH)/compact.asm $(DEFS) -v > $(LIST)/compact-1770-atom.txt
	
copy: $(SRCPATH)/copy.asm $(INCLUDES) $(ROMSRC)
	$(ASM) -i $(SRCPATH)/copy.asm $(DEFS) -v > $(LIST)/copy-1770-atom.txt
	
copyf: $(SRCPATH)/copyf.asm $(INCLUDES) $(ROMSRC)
	$(ASM) -i $(SRCPATH)/copyf.asm $(DEFS) -v > $(LIST)/copyf-1770-atom.txt

duty: $(SRCPATH)/duty.asm $(INCLUDES) $(ROMSRC)
	$(ASM) -i $(SRCPATH)/duty.asm $(DEFS) -v > $(LIST)/duty-1770-atom.txt

infall: $(SRCPATH)/infall.asm $(INCLUDES) $(ROMSRC)
	$(ASM) -i $(SRCPATH)/infall.asm $(DEFS) -v > $(LIST)/infall-1770-atom.txt

sysinfo: $(SRCPATH)/sysinfo.asm $(INCLUDES)
	$(ASM) -i $(SRCPATH)/sysinfo.asm $(DEFS) -D ATM=0 -v > $(LIST)/sysinfo-1770-atom.txt
	
clean:
	rm -f *.DFS	
