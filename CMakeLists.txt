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
set(YPP_VERSION_MINOR 9)
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

# find SQLite
if(WIN32)
	set(PC_SQLITE_INCLUDE_DIRS "D:/dev/yarp-2.3.22/extern/sqlite/sqlite")
	set(PC_SQLITE_LIBRARY_DIRS "D:/dev/yarp-2.3.22/lib/Debug/")
	set(PC_SQLITE_LIBRARIES "D:/dev/yarp-2.3.22/lib/Debug/YARP_sqlited.lib")
endif(WIN32)

find_package(SQLite REQUIRED)
list(APPEND CMAKE_MODULE_PATH ${SQLite_MODULE_PATH})

# add SQLite include directories
include_directories(${SQLite_INCLUDE_DIRS})

add_subdirectory(Common)
add_subdirectory(Debugging)
add_subdirectory(ExampleEchoClient)
add_subdirectory(ExampleEchoService)
add_subdirectory(ExampleRandomNumberClient)
add_subdirectory(ExampleRandomNumberService)
add_subdirectory(RegistryService)

include_directories("${DEBUGGING_DIR}"
                    "${COMMON_DIR}")

enable_testing()
