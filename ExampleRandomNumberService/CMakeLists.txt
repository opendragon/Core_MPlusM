#
#  YarpPlusPlus/ExampleRandomNumberService/CMakeLists.txt
#  YarpPlusPlus
#
#  Created by Norman Jaffe on 2014-03-06.
#  Copyright (c) 2014 OpenDragon. All rights reserved.
#

include_directories("${DEBUGGING_DIR}"
                    "${COMMON_DIR}")

# set up our program
add_executable(ExampleRandomNumberService main.cpp YPPExampleRandomNumberService.cpp YPPRandomRequestHandler.cpp)

# link with YARP libraries and our interface classes
target_link_libraries(ExampleRandomNumberService YPPBaseService YPPBaseShared YPPDebugging ${YARP_LIBRARIES})

enable_testing()
