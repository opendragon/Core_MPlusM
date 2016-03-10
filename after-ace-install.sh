#!/bin/bash

LIB_PATH=${1:-/opt/m+m}
for ii in $LIB_PATH/lib/libACE*.a
do
	rm $ii
done
rm $LIB_PATH/lib/libJAWS.a $LIB_PATH/lib/libKokyu.a
