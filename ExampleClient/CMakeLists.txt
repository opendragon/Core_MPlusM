#
#  YarpPlusPlus/ExampleClient/CMakeLists.txt
#  YarpPlusPlus
#
#  Created by Norman Jaffe on 2014-02-06.
#  Copyright (c) 2014 OpenDragon. All rights reserved.
#

include_directories("${DEBUGGING_DIR}"
                    "${COMMON_DIR}")

# set up our program
add_executable(ExampleClient main.cpp YPPExampleClient.cpp)

# link with YARP libraries and our interface classes
target_link_libraries(ExampleClient YPPBaseClient YPPBaseShared YPPDebugging ${YARP_LIBRARIES})

enable_testing()
