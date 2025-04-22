#!/bin/bash

if test -f ../../vivadoenv.sh;
then
    . ../../vivadoenv.sh
fi

if [ $# -eq 0 ]; then
    echo -n "Configurations: "
    ls cfg*.tcl | sed -e "s/cfg_//" -e "s/.tcl//" | xargs echo
    exit 1;
elif [ $# -ne 1 ]; then
    echo "One argument expected."
    exit 1;
else
    DIR=$1
fi

mkdir -p $DIR

cd $DIR

USE_RATASER=0

if [ -d ../../../../rataser/vhdl ]; then
    USE_RATASER=1
fi

GEN_TEXT_CONFIG_PL=../../../boards/common/gen_text_config.pl
TEXT_CONFIG_SETUP_TXT=../../../boards/common/text_config_setup.txt
TEXT_CONFIG_SETUP_VHDL=text_config_data.vhd

${GEN_TEXT_CONFIG_PL} \
     < ${TEXT_CONFIG_SETUP_TXT} \
     > ${TEXT_CONFIG_SETUP_VHDL}

vivado -mode batch -source ../../xilinx_vivado/build.tcl -tclargs $DIR $USE_RATASER

# to invoke manually:
#
# vivado -mode tcl
#
# source build.tcl
