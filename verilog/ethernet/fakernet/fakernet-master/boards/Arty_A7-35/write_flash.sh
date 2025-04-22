#!/bin/bash

echo
echo "Program firmware into flash memory.  (Needs restart.)"
echo

if test -f ../../vivadoenv.sh;
then
    . ../../vivadoenv.sh
fi

vivado -mode batch -source write_flash.tcl || exit 1

echo
echo "Press 'PROG' button to restart FPGA."
echo
