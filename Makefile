#
# Makefile for System ROMS and utility programs.
#
# This requires a version of BeebASM that supports command line defines with -D 
# BeebASM is available from : 
# http://www.retrosoftware.co.uk/wiki/index.php?title=BeebAsm
#

DOS1770=dos1770
DOS8271=dos8271
LISTPATH=list
LIST="../$(LISTPATH)"
ROM=rom
SRCPATH=../src
DISKPATH=disks

ASM=beebasm

ROMSRC=$(SRCPATH)/sys5-1f.asm $(SRCPATH)/sys5-1f-1770.asm 
INCLUDES=$(SRCPATH)/sysdefs.asm $(SRCPATH)/wdfdc.asm $(SRCPATH)/intelfdc.asm
export

# Main target build everything
#all: init roms utils format 

all: init roms wddos inteldos disks

# roms, Build 40 and 80 column variants of system roms with both 
# floppy controllers
#roms: sysrom40 sysrom80 sysrom-wd40 sysrom-wd80 

roms:
	@echo "Building ROMS"
	cd $(ROM) && make all

wddos:
	@echo "Building utils for WD1770"
	cd $(DOS1770) && make all

inteldos:
	@echo "Building utils for Intel 8271"
	cd $(DOS8271) && make all

init:
	mkdir -p $(ROM)
	mkdir -p $(DISKPATH)
	mkdir -p $(LISTPATH)
	
disks: init roms wddos inteldos
	sh build-disk.sh $(DOS1770) $(DISKPATH)
	sh build-disk.sh $(DOS8271) $(DISKPATH)
	
clean:
	cd $(ROM) && make clean
	cd $(DOS1770) && make clean
	cd $(DOS8271) && make clean
	rm -f list/*.txt
	rm -f $(DISKPATH)/*.ssd DISKPATH)/*.hfe
