#!/bin/bash

echo "Runonesim: $1"

INPUT=$1.txt
OUTBASEVTMP=$1

set -e

touch $OUTBASEVTMP.out
touch $OUTBASEVTMP.vcd

sim/tb_fakernet \
    --stop-time=100000ns \
    --vcd=$OUTBASEVTMP.vcd --wave=$OUTBASEVTMP.ghw < $INPUT 2> \
    >(egrep -v "@(0ms|10ns|20ns).*NUMERIC_STD.(TO_INTEGER|\"=\")" \
	    1>&2) > \
    $OUTBASEVTMP.out || \
    (echo "SIM-FAIL: $OUTBASEVTMP  " ; \
     mv $OUTBASEVTMP.out $OUTBASEVTMP.fail.out ; \
     mv $OUTBASEVTMP.vcd $OUTBASEVTMP.fail.vcd ; \
     exit 1)

LANG=C ./totextdump.pl $OUTBASEVTMP.out > $OUTBASEVTMP.out.txt

command -v text2pcap >/dev/null 2>&1 && \
    text2pcap $OUTBASEVTMP.out.txt $OUTBASEVTMP.pcap && \
    text2pcap $OUTBASEVTMP.good $OUTBASEVTMP.good.pcap -q

command -v editcap >/dev/null 2>&1 && \
    editcap -T rawip $OUTBASEVTMP.pcap $OUTBASEVTMP.ip.pcap && \
    editcap -T rawip $OUTBASEVTMP.good.pcap $OUTBASEVTMP.good.ip.pcap

diff -u $OUTBASEVTMP.good $OUTBASEVTMP.out.txt || \
    ( echo "--- Failure while running: $INPUT" ; \
      echo "---------------" ; false)

echo "Done: $1"
