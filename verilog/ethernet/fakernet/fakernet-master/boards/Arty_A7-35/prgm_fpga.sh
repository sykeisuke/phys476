#!/bin/bash

echo
echo "Program firmware into FPGA directly.  (Lost on restart.)"
echo

if test -f ../../vivadoenv.sh;
then
    . ../../vivadoenv.sh
fi

vivado -mode batch -source prgm_fpga.tcl

# to invoke manually (vivado session can then be used repeatedly):
#
# vivado -mode tcl
#
# source flash.tcl
