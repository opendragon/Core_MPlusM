#!/bin/bash

LIB_PATH=${1:-/opt/m+m}
for ii in lib/*.1.dylib
do
	install_name_tool -change "`pwd`/lib/libYARP_OS.1.dylib" "@rpath/libYARP_OS.1.dylib" "$ii"
	install_name_tool -change "`pwd`/lib/libYARP_init.1.dylib" "@rpath/libYARP_init.1.dylib" "$ii"
	install_name_tool -change "`pwd`/lib/libYARP_name.1.dylib" "@rpath/libYARP_name.1.dylib" "$ii"
	install_name_tool -change "`pwd`/lib/libYARP_sig.1.dylib" "@rpath/libYARP_sig.1.dylib" "$ii"
	install_name_tool -change libACE.dylib "@rpath/libACE.dylib" "$ii"
done
for ii in bin/*
do
	install_name_tool -add_rpath "$LIB_PATH/lib" "$ii"
	install_name_tool -change "`pwd`/lib/libYARP_OS.1.dylib" "@rpath/libYARP_OS.1.dylib" "$ii"
	install_name_tool -change "`pwd`/lib/libYARP_init.1.dylib" "@rpath/libYARP_init.1.dylib" "$ii"
	install_name_tool -change "`pwd`/lib/libYARP_name.1.dylib" "@rpath/libYARP_name.1.dylib" "$ii"
	install_name_tool -change "`pwd`/lib/libYARP_sig.1.dylib" "@rpath/libYARP_sig.1.dylib" "$ii"
	install_name_tool -change libACE.dylib "@rpath/libACE.dylib" "$ii"
done
