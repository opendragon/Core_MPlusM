#!/bin/bash

LIB_PATH=${1:-/opt/m+m/spidermonkey}
cp dist/bin/libmozglue.dylib $LIB_PATH/lib
for ii in $LIB_PATH/lib/libmozjs*.dylib $LIB_PATH/bin/js
do
    install_name_tool -change "@executable_path/libmozglue.dylib" "@rpath/libmozglue.dylib" "$ii"
done
