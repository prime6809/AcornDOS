#
# Makefile for System and Atom ROMS and utility programs.
#
# This requires a version of BeebASM that supports command line defines with -D 
# BeebASM is available from : 
# http://www.retrosoftware.co.uk/wiki/index.php?title=BeebAsm
#

include vars.mk

# Main target build everything
#all: init roms utils format 

def:
	@echo "Make system for Acorn System and Atom versions of AcornDOS"
	@echo "valid targets are :"
	@echo "make atom        Make Atom roms and disks."
	@echo "make system      Make System roms and disks."
	@echo "make all         Make all roms and disks."
	@echo "make clean       Clean built files / listings etc."
	

all: atom system

#
# System targets
#

system: init sysroms syswddos sysinteldos sysdisks

# roms, Build 40 and 80 column variants of system roms with both 
# floppy controllers
#roms: sysrom40 sysrom80 sysrom-wd40 sysrom-wd80 

sysroms:
	@echo "Building System ROMS"
	cd $(SROM) && make all

syswddos:
	@echo "Building System utils for WD1770"
	cd $(SDOS1770) && make all

sysinteldos:
	@echo "Building System utils for Intel 8271"
	cd $(SDOS8271) && make all

sysdisks: init sysroms syswddos sysinteldos
	sh build-disk-system.sh $(SDOS1770) $(DISKPATH)
	sh build-disk-system.sh $(SDOS8271) $(DISKPATH)

#
# Atom targets
#
atom: init atomroms atomwddos atominteldos atomdisks

atomroms:
	@echo "Building Atom ROMS"
	cd $(AROM) && make all

atomwddos:
	@echo "Building Atom utils for WD1770"
	cd $(ADOS1770) && make all

atominteldos:
	@echo "Building Atom utils for Intel 8271"
	cd $(ADOS8271) && make all

atomdisks: init atomroms atomwddos atominteldos
	sh build-disk-atom.sh $(ADOS1770) $(DISKPATH)
	sh build-disk-atom.sh $(ADOS8271) $(DISKPATH)

init:
	mkdir -p $(DISKPATH)
	mkdir -p $(LISTPATH)
	
clean:
	cd $(SROM) && make clean
	cd $(SDOS1770) && make clean
	cd $(SDOS8271) && make clean
	cd $(AROM) && make clean
	cd $(ADOS1770) && make clean
	cd $(ADOS8271) && make clean
	rm -f list/*.txt
	rm -f $(DISKPATH)/*.ssd $(DISKPATH)/*.hfe
