#
#  YarpPlusPlus/Debugging/CMakeLists.txt
#  YarpPlusPlus
#
#  Created by Norman Jaffe on 2014-02-06.
#  Copyright (c) 2014 OpenDragon. All rights reserved.
#

option(USE_OBJECTIVE_C_DEBUG "Use Objective C Debug" FALSE)
mark_as_advanced(USE_OBJECTIVE_C_DEBUG)

if(USE_OBJECTIVE_C_DEBUG)
    add_library(YPPDebugging ODSyslog.m)
else(USE_OBJECTIVE_C_DEBUG)
    add_library(YPPDebugging ODSyslog.cpp)
endif(USE_OBJECTIVE_C_DEBUG)

install(TARGETS YPPDebugging DESTINATION bin)
install(FILES YPPDebugging DESTINATION include)
