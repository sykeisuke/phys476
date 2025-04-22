#!/bin/bash

echo "Run_nmea: $1"

INPUT=$1.txt
OUTBASEVTMP=$1

set -e

touch $INPUT
touch $OUTBASEVTMP.out
touch $OUTBASEVTMP.vcd

sim_efb_spi_read/tb_efb_spi_read \
    --stop-time=1000000ns \
    --vcd=$OUTBASEVTMP.vcd --wave=$OUTBASEVTMP.ghw < $INPUT 2> \
    >(egrep -v "@(0ms|10ns|20ns).*NUMERIC_STD.(TO_INTEGER|\"=\")" \
	    1>&2) > \
    $OUTBASEVTMP.out || \
    (echo "SIM-FAIL: $OUTBASEVTMP  " ; \
     mv $OUTBASEVTMP.out $OUTBASEVTMP.fail.out ; \
     mv $OUTBASEVTMP.vcd $OUTBASEVTMP.fail.vcd ; \
     exit 1)

echo "Done: $1"
