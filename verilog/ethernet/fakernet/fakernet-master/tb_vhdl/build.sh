#!/bin/bash

set -e

if test -f ../ghdlenv.sh;
then
    . ../ghdlenv.sh
fi

make
