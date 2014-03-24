#--------------------------------------------------------------------------------------
#
#  File:       CMakeLists.txt
#
#  Project:    YarpPlusPlus
#
#  Contains:   The common CMAKE definitions for the Yarp++ project.
#
#  Written by: Norman Jaffe
#
#  Copyright:  (c) 2014 by HPlus Technologies Ltd. and Simon Fraser University.
#
#              All rights reserved. Redistribution and use in source and binary forms,
#              with or without modification, are permitted provided that the following
#              conditions are met:
#                * Redistributions of source code must retain the above copyright
#                  notice, this list of conditions and the following disclaimer.
#                * Redistributions in binary form must reproduce the above copyright
#                  notice, this list of conditions and the following disclaimer in the
#                  documentation and/or other materials provided with the
#                  distribution.
#                * Neither the name of the copyright holders nor the names of its
#                  contributors may be used to endorse or promote products derived
#                  from this software without specific prior written permission.
#
#              THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#              "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
#              LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
#              PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
#              OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
#              SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
#              LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
#              DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
#              THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
#              (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
#              OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
#  Created:    2014-02-06
#
#--------------------------------------------------------------------------------------

# YARP needs CMake 2.6 or greater
cmake_minimum_required(VERSION 2.6)

project(YARPPlusPlus)

set(YPP_VERSION_MAJOR 1)
set(YPP_VERSION_MINOR 1)
set(YPP_VERSION_PATCH 3)

option(YPP_MAIN_DOES_DELAY_NOT_YIELD "Use Delay in main() loop")

option(YPP_DEBUG_INCLUDES_YARP_TRACE "Include YARP trace when debugging")

option(YPP_SERVICES_DEBUG_TO_STDERR "Services send output to stderr")

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

set(YPP_COMMON_DIR "${PROJECT_SOURCE_DIR}/Common")
set(YPP_DEBUGGING_DIR "${PROJECT_SOURCE_DIR}/Debugging")
set(YPP_PARSER_DIR "${PROJECT_SOURCE_DIR}/Parser")

configure_file("${YPP_COMMON_DIR}/YPPConfig.h.in"
                "${YPP_COMMON_DIR}/YPPConfig.h")

include_directories("${YPP_COMMON_DIR}"
                    "${YPP_DEBUGGING_DIR}"
                    "${YPP_PARSER_DIR}")

add_subdirectory(Common)
add_subdirectory(Debugging)
add_subdirectory(ExampleEchoClient)
add_subdirectory(ExampleEchoService)
add_subdirectory(ExampleRandomNumberClient)
add_subdirectory(ExampleRandomNumberService)
add_subdirectory(ExampleRunningSumClient)
add_subdirectory(ExampleRunningSumService)
add_subdirectory(Parser)
add_subdirectory(RegistryService)
add_subdirectory(RequestCounterClient)
add_subdirectory(RequestCounterService)
add_subdirectory(RequestInfo)
add_subdirectory(ServiceLister)

enable_testing()
