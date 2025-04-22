#!/bin/bash

if test -f ../../toolsenv.sh;
then
    . ../../toolsenv.sh
fi

set -e

TARGETPART=xc6slx16-2-csg324
TOPNAME=alinx_ax516
BUILDDIR=ax516_fnet
TARGETUCF=$TOPNAME.ucf
TARGETUT=$TOPNAME.ut
SPEEDGRADE=2

if [ 1 == 1 ]
then

rm -rf $BUILDDIR
mkdir -p $BUILDDIR

mkdir -p xst/projnav.tmp/

SHORTTIME=`date -u "+%s"`

cat $TOPNAME.xst | \
    sed -e "s/TARGETPART/$TARGETPART/" | \
    sed -e "s/SHORTTIME/$SHORTTIME/" \
          > $BUILDDIR/$TOPNAME.tmp.xst

GEN_TEXT_CONFIG_PL=../../boards/common/gen_text_config.pl
TEXT_CONFIG_SETUP_TXT=../../boards/common/text_config_setup.txt
TEXT_CONFIG_SETUP_VHDL=$BUILDDIR/text_config_data.vhd

${GEN_TEXT_CONFIG_PL} \
     < ${TEXT_CONFIG_SETUP_TXT} \
     > ${TEXT_CONFIG_SETUP_VHDL}

xst -ifn $BUILDDIR/$TOPNAME.tmp.xst -ofn $BUILDDIR/$TOPNAME.srp

ngdbuild -dd $BUILDDIR/_ngo -nt timestamp -uc $TARGETUCF \
        -p $TARGETPART \
        $BUILDDIR/$TOPNAME.ngc $BUILDDIR/$TOPNAME.ngd

MAPARGS=
MAP9EXTRAARG=

PAREFFORT='-ol high'

map -detail -p $TARGETPART \
        $MAPARGS \
        -t 3 \
        -pr b $MAP9EXTRAARG -c 100 \
        -o $BUILDDIR/${TOPNAME}_map.ncd \
        $BUILDDIR/$TOPNAME.ngd \
        $BUILDDIR/$TOPNAME.pcf

par -w $PAREFFORT $BUILDDIR/${TOPNAME}_map.ncd \
        $BUILDDIR/$TOPNAME.ncd $BUILDDIR/$TOPNAME.pcf

fi

trce -a -v 30 -n 5 -u -s $SPEEDGRADE -timegroups -fastpaths -l 100 \
         -tsi $BUILDDIR/$TOPNAME.tsi $BUILDDIR/$TOPNAME.ncd \
         -o $BUILDDIR/$TOPNAME.twr $BUILDDIR/$TOPNAME.pcf \
         -ucf $TARGETUCF

bitgen -f $TARGETUT $BUILDDIR/$TOPNAME.ncd

