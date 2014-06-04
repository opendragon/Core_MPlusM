#--------------------------------------------------------------------------------------
#
#  File:       CMakeLists.txt
#
#  Project:    M+M
#
#  Contains:   The common CMAKE definitions for the M+M project.
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

project(M+M)

set(MpM_VERSION_MAJOR 1)
set(MpM_VERSION_MINOR 2)
set(MpM_VERSION_PATCH 14)

# Make some flags to control the build visible
mark_as_advanced(CLEAR BUILD_SHARED_LIBS)
mark_as_advanced(CLEAR CMAKE_OSX_ARCHITECTURES)

if(APPLE)
	set(CMAKE_OSX_ARCHITECTURES "i386;x86_64" CACHE STRING "Build architectures for Mac OS X" FORCE)
	set(CMAKE_CXX_FLAGS "-stdlib=libstdc++" CACHE STRING "Fixes linking error for M+M/ACE" FORCE)
endif(APPLE)

option(MpM_CHANNELS_USE_RPC "Use RpcClient and RpcService objects for communication")
option(MpM_CHATTY_START "Report the version numbers when launching an executable")
option(MpM_DO_EXPLICIT_CLOSE "Perform an explicit CloseChannel() prior to freeing a channel")
option(MpM_DO_EXPLICIT_DISCONNECT "Perform an explicit disconnect prior to closing a channel")
option(MpM_DONT_USE_TIMEOUTS "Allow operations to take arbitrary lengths of time")
option(MpM_LOG_INCLUDES_YARP_TRACE "Include YARP trace when logging")
option(MpM_MAIN_DOES_DELAY_NOT_YIELD "Use Delay() in main() loop")
option(MpM_REPORT_ON_CONNECTIONS "Report on connections")
option(MpM_SERVICES_LOG_TO_STDERR "Services send logging output to stderr")
option(MpM_STALL_ON_SEND_PROBLEM "A send failure goes to a tight loop")

# Find YARP
find_package(YARP REQUIRED)
list(APPEND CMAKE_MODULE_PATH ${YARP_MODULE_PATH})

# Add YARP include directories
include_directories(${YARP_INCLUDE_DIRS})

# Find ACE
find_package(ACE REQUIRED)
list(APPEND CMAKE_MODULE_PATH ${ACE_MODULE_PATH})

# Add ACE include directories
include_directories(${ACE_INCLUDE_DIRS})

set(MpM_ADAPTER_BASE_NAME "/adapter/")
set(MpM_CLIENT_BASE_NAME  "/client/")
set(MpM_SERVICE_BASE_NAME "/service/")

set(MpM_COMMON_DIR "${PROJECT_SOURCE_DIR}/Common")
set(MpM_LOGGING_DIR "${PROJECT_SOURCE_DIR}/Logging")
set(MpM_PARSER_DIR "${PROJECT_SOURCE_DIR}/Parser")
set(MpM_SQLITE_DIR "${PROJECT_SOURCE_DIR}/SQLite3")

configure_file("${MpM_COMMON_DIR}/M+MConfig.h.in"
                "${MpM_COMMON_DIR}/M+MConfig.h")

include_directories("${MpM_COMMON_DIR}"
                    "${MpM_LOGGING_DIR}"
                    "${MpM_PARSER_DIR}")

add_subdirectory(ClientList)
add_subdirectory(Common)
add_subdirectory(examples)
add_subdirectory(Logging)
add_subdirectory(Parser)
add_subdirectory(PortLister)
add_subdirectory(RegistryService)
add_subdirectory(RequestCounter)
add_subdirectory(RequestInfo)
add_subdirectory(ServiceLister)
add_subdirectory(Version)

enable_testing()
