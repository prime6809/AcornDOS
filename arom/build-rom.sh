#!/bin/bash
#
# Build roms for Atomulator and GDOS-2015.
#

BLANK=blank64k.rom
rm -f $BLANK
dd if=/dev/zero bs=1024 count=64 2> /dev/null | tr "\000" "\377" >> $BLANK 2> /dev/null

#build GDOS-2015 board rom
cat $BLANK ados1770-bc00.rom gdos1770-bc00.rom ados1770-eff0.rom gdos1770-eff0.rom sysinfoe0.rom > fdc.rom

#build Atomulator rom
cat ados1770-bc00.rom gdos1770-bc00.rom ados1770-eff0.rom gdos1770-eff0.rom sysinfoe0.rom > gdos2015.rom

