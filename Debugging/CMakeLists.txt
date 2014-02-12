#
#  YarpPlusPlus/Debugging/CMakeLists.txt
#  YarpPlusPlus
#
#  Created by Norman Jaffe on 2014-02-06.
#  Copyright (c) 2014 OpenDragon. All rights reserved.
#

add_library(YPPDebugging ODSyslog.cpp)

install(TARGETS YPPDebugging DESTINATION bin)
install(FILES YPPDebugging DESTINATION include)
