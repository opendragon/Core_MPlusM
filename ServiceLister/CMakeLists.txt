#
#  YarpPlusPlus/ServiceLister/CMakeLists.txt
#  YarpPlusPlus
#
#  Created by Norman Jaffe on 2014-03-12.
#  Copyright (c) 2014 OpenDragon. All rights reserved.
#

include_directories("${YPP_DEBUGGING_DIR}"
                    "${YPP_COMMON_DIR}")

# set up our program
add_executable(ServiceLister main.cpp)

# link with YARP libraries and our interface classes
target_link_libraries(ServiceLister YPPBaseClient YPPBaseShared YPPDebugging YPPParser ${YARP_LIBRARIES})

enable_testing()
