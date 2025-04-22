#!/bin/bash

set -e

for i in packet20 packet19 packet18 packet17 packet16 \
	 packet15 packet14 packet13 packet12 packet11 packet10 packet9 \
	 packet8 packet7 packet6 packet5 packet4 packet3 packet2 packet1
do
    ./runonesim.sh $i &
done

FAIL=0

for job in `jobs -p`
do
    wait $job || let "FAIL+=1"
done

if [ "$FAIL" == "0" ]
then
    echo "Done."
else
    echo "*"
    echo "* Failures: $FAIL"
    echo "*"
    exit 1
fi
