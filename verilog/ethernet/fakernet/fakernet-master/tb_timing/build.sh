#!/bin/bash

set -e

TIMEC=4.0
KIND=0

KILLV5=dummykill
case $TARGET in
    virtex4)
	TARGETPART=xc4vlx25-10-sf363
	TOOLCHAIN=xilinx
	;;
    spartan6)
	TARGETPART=xc6slx45-fgg484-2
	KILLV5="bufr|slice_packing"
	TOOLCHAIN=xilinx
	;;
    virtex6)
	TARGETPART=xc6vlx75t-1-ff484
	KILLV5="bufr|slice_packing"
	TOOLCHAIN=xilinx
	;;
    max10)
	TARGETPART=10M50DAF484C7G
	TOOLCHAIN=altera
	;;
    cyclonev)
	TARGETPART=5CGXFC7D6F27I7 # 5CGXFC7C7F23C8
	TOOLCHAIN=altera
	;;
    cycloneiv)
	TARGETPART=EP4CE15F23C8
	TOOLCHAIN=altera
	;;
    cyclone10)
	TARGETPART=10CL016YU484C8G
	TOOLCHAIN=altera
	;;
    *)
	echo "Unknown target $TARGET"
	exit 1
	;;
esac

if test -f ../toolsenv.sh;
then
    . ../toolsenv.sh
fi

BUILDDIR=bld/$TARGET/k$KIND/r$REGS/tc$TIMEC

rm -rf $BUILDDIR
mkdir -p $BUILDDIR

#cat ../tb_common/tb_dptc_config_pkg.vhd | \
#    sed -e "s/BITS/$BITS/" | \
#    ../tb_common/regmask.sh $REGS $KIND \
#			    > $BUILDDIR/tb_dptc_config_pkg.vhd

case $TOOLCHAIN in
    xilinx)
	mkdir -p $BUILDDIR/build_fpga/xst
	mkdir -p $BUILDDIR/build_fpga/xst/projnav.tmp

	cp ../tb_timing/xilinx/tb_timing.prj $BUILDDIR
	cp ../tb_timing/xilinx/tb_timing.lso $BUILDDIR
	cat ../tb_timing/xilinx/tb_timing.xcf | \
	    sed -e "s/TIMECONSTRAINT/$TIMEC/" \
		> $BUILDDIR/tb_timing.xcf
	cat ../tb_timing/xilinx/tb_timing.xst | \
	    sed -e "s/TARGETPART/$TARGETPART/" | \
	    egrep -v $KILLV5 \
		  > $BUILDDIR/tb_timing.xst

	(cd $BUILDDIR ; \
	 xst -ifn tb_timing.xst -ofn tb_timing.srp > /dev/null) ||
	    ( cp $BUILDDIR/tb_timing.srp . && exit 1 )

	echo "# TARGET $TARGET k$KIND r$REGS tc$TIMEC" > $BUILDDIR/result.txt
	egrep "Minimum period:|Number of.*LUTs:|Number of.*Flip Flops:|Number of.*Registers:|Number of .*RAM" \
	      $BUILDDIR/tb_timing.srp >> \
	      $BUILDDIR/result.txt

	cat $BUILDDIR/result.txt
	echo ".srp: $BUILDDIR/tb_timing.srp"
	;;

    altera)
	cat ../tb_timing/altera/tb_timing.sdc | \
	    sed -e "s/TIMECONSTRAINT/$TIMEC/" \
		> $BUILDDIR/tb_timing.sdc
	cat ../tb_timing/altera/tb_timing.qsf | \
	    sed -e "s/TARGETPART/$TARGETPART/" \
		> $BUILDDIR/tb_timing.qsf

	QUARTUS_ARGS="--read_settings_files=on --write_settings_files=off"

	export NUM_PARALLEL_PROCESSORS=1

	(cd $BUILDDIR ; \
	 quartus_map $QUARTUS_ARGS tb_timing -c tb_timing > \
		     /dev/null 2> /dev/null &&
	     quartus_fit $QUARTUS_ARGS tb_timing -c tb_timing > \
			 /dev/null 2> /dev/null &&
	     quartus_sta --do_report_timing tb_timing -c tb_timing > \
			 /dev/null 2> /dev/null) || exit 1

	echo "# TARGET $TARGET k$KIND r$REGS tc$TIMEC" > $BUILDDIR/result.txt
	egrep "Total logic elements|Logic utilization|Total registers|Total RAM Blocks" \
	      $BUILDDIR/out/tb_timing.fit.summary >> \
	      $BUILDDIR/result.txt
	egrep "^; M9Ks|^; M10K blocks" \
	      $BUILDDIR/out/tb_timing.fit.rpt >> \
	      $BUILDDIR/result.txt
	egrep "MHz.*clk" $BUILDDIR/out/tb_timing.sta.rpt >> \
	      $BUILDDIR/result.txt

	cat $BUILDDIR/result.txt
	echo ".sta.rpt: $BUILDDIR/out/tb_timing.sta.rpt"
	;;

    *)
	echo "Unknown toolchain $TOOLCHAIN"
	exit 1
	;;
esac
