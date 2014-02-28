#
#  YarpPlusPlus/CMakeLists.txt
#  YarpPlusPlus
#
#  Created by Norman Jaffe on 2014-02-06.
#  Copyright (c) 2014 OpenDragon. All rights reserved.
#

# YARP needs CMake 2.6 or greater
cmake_minimum_required(VERSION 2.6)

project(YARPPlusPlus)

set(YPP_VERSION_MAJOR 0)
set(YPP_VERSION_MINOR 6)
set(YPP_VERSION_PATCH 0)

set(DEBUGGING_DIR "${PROJECT_SOURCE_DIR}/Debugging")
set(COMMON_DIR "${PROJECT_SOURCE_DIR}/Common")

configure_file("${COMMON_DIR}/YPPConfig.h.in"
                "${COMMON_DIR}/YPPConfig.h")

# find YARP
find_package(YARP REQUIRED)
list(APPEND CMAKE_MODULE_PATH ${YARP_MODULE_PATH})

# add YARP include directories
include_directories(${YARP_INCLUDE_DIRS})

# find ACE
find_package(ACE REQUIRED)
list(APPEND CMAKE_MODULE_PATH ${ACE_MODULE_PATH})

# add ACE include directories
include_directories(${ACE_INCLUDE_DIRS})

add_subdirectory(Debugging)
add_subdirectory(Common)
add_subdirectory(ExampleService)
add_subdirectory(ExampleClient)
add_subdirectory(RegistryService)

include_directories("${DEBUGGING_DIR}"
                    "${COMMON_DIR}")

enable_testing()
