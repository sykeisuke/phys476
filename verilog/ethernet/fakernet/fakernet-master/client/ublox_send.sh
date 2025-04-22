#!/bin/sh

make_hex_string ()
{
    hex_string=""
    for i in 0 8 16 24; do
	hex_string=`printf "$hex_string %02x" $((($1 >> $i) & 0xff))`
    done
    echo $hex_string
}

FNETHOST=192.168.1.193

# Checksums are calculated automatically, not included.
while [ $# -gt 0 ]; do
    case $1 in
        --hw-reset)
	    DATA="B5 62  06 04  04 00  FF FF 00 00" # 0c 5d
	    ;;
        --ena-qerr)
	    DATA="b5 62  06 01  03 00  0d 01 01" # 19 69
	    ;;
	--ena-leap)
	    DATA="b5 62  06 01  03 00  01 26 01" # 32 8f
	    ;;
        --ena-tim-svin) # Survey information.
	    DATA="b5 62  06 01  03 00  0d 04 01"
	    ;;
        --ena-tmode2) # Enable survey.
	    NMEAS=$((12*3600))
	    LIMIT_MM=5000
	    DATA="b5 62  06 3d  1c 00  01  00  00 00 " # 01 = do survey
	    DATA="$DATA 00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00 "
	    # First 32-bit value is min number of measurements (0x3e8=1000),
	    # second 32-bit value is max accuracy limit [mm] (0x3e8=1000).
	    # DATA="$DATA e8 03 00 00  e8 03 00 00" # 1000 meas., 1.000 m
	    DATA="$DATA $(make_hex_string $NMEAS)"
	    DATA="$DATA $(make_hex_string $LIMIT_MM)"
	    ;;
        *)
            echo "Unknown argument: $1" 1>&2
            usage 1 1>&2
            ;;
    esac
    shift
done

if [ -z "$DATA" ]
then
    echo "No data to send (no command)?"
    exit 1
fi

# This script is used seldomly.  We just send one byte at a time.

# Initialize checksums.  Such that the fixed two bytes are 'ignored'.
ck_a=0xe9
ck_b=0x62

for i in $DATA
do
    ./fnetctrl $FNETHOST --write=8:0x$i
    # Calculate checksum
    ck_a=$((ck_a + 0x$i))
    ck_b=$((ck_b + ck_a))
done

printf "Checksum: %02x %02x\n" $((ck_a & 0xff)) $((ck_b & 0xff))

# Write the checksum.
./fnetctrl $FNETHOST --write=8:$((ck_a & 0xff))  # Write as plain integer
./fnetctrl $FNETHOST --write=8:$((ck_b & 0xff))  # Write as plain integer
