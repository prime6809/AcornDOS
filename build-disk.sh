#!/bin/bash
# 
# Build disk script for AcornDOS on the Acorn System (or clone) machines.
# Created on windows under Cygwin, but should be portable to any Unix / Linux
# like system.
#
# This script calls dfsdisk which is available from :
# https://github.com/prime6809/DFSdisk
#
# Creating the hfe file requires the HxC software availble from :
# https://sourceforge.net/projects/hxcfloppyemu/
# 

# File locations

DFSDISK=dfsdisk
HXCFE=/cygdrive/d/apps/HxC/hxcfe.exe

if [ $# -eq 0 ]
then
  echo "Error! must supply dos version to process."
  exit -1
fi

DFSPATH=`which $DFSDISK 2> /dev/null`
HXCPATH=`which $HXCFE 2> /dev/null`

if [ -z $DFSPATH ]
then
  echo "Error! cannot find $DFSDISK either directly or on path."
  echo "aborting."
  exit -1
fi

DOSVER=$1
if [ ! -z $2 ]
then
  SSD="$2/$DOSVER.ssd"
  HFE="$2/$DOSVER.hfe"
else
  SSD="$DOSVER.ssd"
  HFE="$DOSVER.hfe"  
fi

$DFSDISK create "$SSD" -t 40 -q \$20 -L "$DOSVER"

$DFSDISK write "$SSD" -f "$DOSVER"/BACKUP.DFS -q \$20 -l \$2800 -e \$2800
$DFSDISK write "$SSD" -f "$DOSVER"/COMPACT.DFS -q \$20 -l \$2800 -e \$2800
$DFSDISK write "$SSD" -f "$DOSVER"/COPY.DFS -q \$20 -l \$2800 -e \$2800
$DFSDISK write "$SSD" -f "$DOSVER"/COPYF.DFS -q \$20 -l \$2800 -e \$2800
$DFSDISK write "$SSD" -f "$DOSVER"/DUTY.DFS -q \$20 -l \$2800 -e \$2800
$DFSDISK write "$SSD" -f "$DOSVER"/FORM40.DFS -q \$20 -l \$2800 -e \$2800
$DFSDISK write "$SSD" -f "$DOSVER"/FORM80.DFS -q \$20 -l \$2800 -e \$2800
$DFSDISK write "$SSD" -f "$DOSVER"/VERIFY4.DFS -q \$20 -l \$2800 -e \$2800
$DFSDISK write "$SSD" -f "$DOSVER"/VERIFY8.DFS -q \$20 -l \$2800 -e \$2800
#$DFSDISK write "$SSD" -f /cygdrive/z/Retro/6502/SBC/SBC2OSLitest/BeebASM/MONS -q \$20 -l \$c000 -e \$c000

#cp "$SSD" /cygdrive/g/emulate/software/atom/disks/"$DOSVER".DSK
#cp "$SSD" /cygdrive/d/emulate/software/atom/disks/"$DOSVER".DSK

$DFSDISK cat "$SSD"

if  [ ! -z $HXCPATH ]
then
  $HXCFE -finput:"$SSD" -foutput:"$HFE" -conv 
else
  echo "$HXCFE does not exist, skipping hfe creation"
fi
