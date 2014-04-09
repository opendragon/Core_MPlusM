#--------------------------------------------------------------------------------------
#
#  File:       CMakeLists.txt
#
#  Project:    MoAndMe
#
#  Contains:   The common CMAKE definitions for the MoAndMe project.
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

project(MovementAndMeaning)

set(MAM_VERSION_MAJOR 1)
set(MAM_VERSION_MINOR 2)
set(MAM_VERSION_PATCH 1)

option(MAM_DO_EXPLICIT_CLOSE "Perform an explicit CloseChannel() prior to freeing a channel")
option(MAM_DO_EXPLICIT_DISCONNECT "Perform an explicit disconnect prior to closing a channel")
option(MAM_LOG_INCLUDES_YARP_TRACE "Include YARP trace when logging")
option(MAM_MAIN_DOES_DELAY_NOT_YIELD "Use Delay() in main() loop")
option(MAM_SERVICES_LOG_TO_STDERR "Services send logging output to stderr")

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

set(MAM_COMMON_DIR "${PROJECT_SOURCE_DIR}/Common")
set(MAM_LOGGING_DIR "${PROJECT_SOURCE_DIR}/Logging")
set(MAM_PARSER_DIR "${PROJECT_SOURCE_DIR}/Parser")
set(MAM_SQLITE_DIR "${PROJECT_SOURCE_DIR}/SQLite3")

configure_file("${MAM_COMMON_DIR}/MoMeConfig.h.in"
                "${MAM_COMMON_DIR}/MoMeConfig.h")

include_directories("${MAM_COMMON_DIR}"
                    "${MAM_LOGGING_DIR}"
                    "${MAM_PARSER_DIR}")

##add_subdirectory(ClientList)
add_subdirectory(Common)
##add_subdirectory(examples)
add_subdirectory(Logging)
add_subdirectory(Parser)
##add_subdirectory(PortLister)
##add_subdirectory(RegistryService)
##add_subdirectory(RequestCounter)
##add_subdirectory(RequestInfo)
##add_subdirectory(ServiceLister)

enable_testing()
