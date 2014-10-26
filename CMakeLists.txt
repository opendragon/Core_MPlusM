#--------------------------------------------------------------------------------------------------
#
#  File:       CMakeLists.txt
#
#  Project:    M+M
#
#  Contains:   The common CMAKE definitions for the M+M project.
#
#  Written by: Norman Jaffe & Johnty Wang
#
#  Copyright:  (c) 2014 by HPlus Technologies Ltd. and Simon Fraser University.
#
#              All rights reserved. Redistribution and use in source and binary forms, with or
#              without modification, are permitted provided that the following conditions are met:
#                * Redistributions of source code must retain the above copyright notice, this list
#                  of conditions and the following disclaimer.
#                * Redistributions in binary form must reproduce the above copyright notice, this
#                  list of conditions and the following disclaimer in the documentation and/or
#                  other materials provided with the distribution.
#                * Neither the name of the copyright holders nor the names of its contributors may
#                  be used to endorse or promote products derived from this software without
#                  specific prior written permission.
#
#              THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY
#              EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
#              OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT
#              SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
#              INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
#              TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
#              BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
#              CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
#              ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
#              DAMAGE.
#
#  Created:    2014-02-06
#
#--------------------------------------------------------------------------------------------------

# YARP needs CMake 2.6 or greater
# CPack requires CMake 2.6 or greater
cmake_minimum_required(VERSION 2.6 FATAL_ERROR)

project(M+M)

set(MpM_VERSION_MAJOR 1)
set(MpM_VERSION_MINOR 4)
set(MpM_VERSION_PATCH 14)

# Make some flags to control the build visible
mark_as_advanced(CLEAR BUILD_SHARED_LIBS)
mark_as_advanced(CLEAR CMAKE_OSX_ARCHITECTURES)

if(APPLE)
	set(CMAKE_OSX_ARCHITECTURES "i386;x86_64" CACHE STRING "Build architectures for Mac OS X" FORCE)
	set(CMAKE_CXX_FLAGS "-stdlib=libstdc++" CACHE STRING "Fixes linking error for M+M/ACE" FORCE)
    set(CMAKE_INSTALL_PREFIX "/opt/M+M" CACHE STRING "Fixes install path for M+M/ACE" FORCE)
endif(APPLE)

set(CMAKE_USE_RELATIVE_PATHS ON CACHE BOOL "Always set file paths as relative")

option(MpM_ChattyStart "Report the version numbers when launching an executable")
option(MpM_DoExplicitClose "Perform an explicit CloseChannel() prior to freeing a channel")
option(MpM_DoExplicitDisconnect "Perform an explicit disconnect prior to closing a channel")
option(MpM_DontUseTimeouts "Allow operations to take arbitrary lengths of time")
option(MpM_LogIncludesYarpTrace "Include YARP trace when logging")
option(MpM_MainDoesDelayNotYield "Use Delay() in main() loop")
option(MpM_ReportContactDetails
        "Report details of the contacts during operations that might change them")
option(MpM_ReportOnConnections "Report on connections")
option(MpM_ServicesLogToStandardError "Services send logging output to stderr" ON)
option(MpM_StallOnSendProblem "A send failure goes to a tight loop")
option(MpM_UseDiskDatabase "Use a disk-based database, rather than in-memory")
option(MpM_UseTimeoutsInRetryLoops "Use timeouts in retry loops")
option(MpM_UseTestDatabase "Use a test database, in /tmp, rather than a random disk location")
mark_as_advanced(MpM_DoExplicitClose)
mark_as_advanced(MpM_DoExplicitDisconnect)
mark_as_advanced(MpM_DontUseTimeouts)
mark_as_advanced(MpM_MainDoesDelayNotYield)
mark_as_advanced(MpM_ReportContactDetails)
mark_as_advanced(MpM_StallOnSendProblem)
mark_as_advanced(MpM_UseDiskDatabase)
mark_as_advanced(MpM_UseTestDatabase)
mark_as_advanced(MpM_UseTimeoutsInRetryLoops)

# Add the M+M target path so that YARP and ACE can be found
list(APPEND CMAKE_PREFIX_PATH "${CMAKE_INSTALL_PREFIX}")

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

set(MpM_ADAPTER_BASE_NAME "/_adapter_/")
set(MpM_CLIENT_BASE_NAME "/_client_/")
set(MpM_INPUT_BASE_NAME "/_input_/")
set(MpM_OUTPUT_BASE_NAME "/_output_/")
set(MpM_SERVICE_BASE_NAME "/_service_/")

set(MpM_SOURCE_DIR "${PROJECT_SOURCE_DIR}")
set(MpM_SQLITE_DIR "${PROJECT_SOURCE_DIR}/SQLite3")

configure_file("${MpM_SOURCE_DIR}/mpm/M+MConfig.h.in"
                "${MpM_SOURCE_DIR}/mpm/M+MConfig.h")

include_directories("${MpM_SOURCE_DIR}")

#TODO: add bonjour to package
#TODO: deal with ACE dependency

if(WIN32)
    set(BONJOUR_DIR "C:/Program Files/Bonjour SDK")
    set(BONJOUR_LIB "${BONJOUR_DIR}/Lib/Win32/dnssd.lib")
    include_directories("${BONJOUR_DIR}/Include")
endif(WIN32)


#packing stuff
set(CPACK_PACKAGE_NAME "M+M")
set(CPACK_VENDOR "Movement and Meaning")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "M+M Test Package")
set(CPACK_PACKAGE_VERSION "1.0.0")
set(CPACK_PACKAGE_VERSION_MAJOR "1")
set(CPACK_PACKAGE_VERSION_MINOR "0")
set(CPACK_PACKAGE_VERSION_PATCH "0")
set(CPACK_PACKAGE_INSTALL_DIRECTORY "M+M")
set(CPACK_RESOURCE_FILE_LICENSE ${MpM_SOURCE_DIR}/license.txt)
#display name for our components
set(CPACK_COMPONENT_APPLICATIONS_DISPLAY_NAME "Core Applications")
set(CPACK_COMPONENT_EXAMPLES_DISPLAY_NAME "Example Applications")
set(CPACK_COMPONENT_LIBRARIES_DISPLAY_NAME "M+M Libraries")
set(CPACK_COMPONENT_HEADERS_DISPLAY_NAME "C++ Header Files")
#set up dependencies
set(CPACK_COMPONENT_HEADERS_DEPENDS libraries)
set(CPACK_COMPONENT_APPLICATIONS_DEPENDS libraries)
set(CPACK_COMPONENT_EXAMPLES_DEPENDS libraries)
#set up CPack groups
set(CPACK_COMPONENT_LIBRARIES_GROUP "Minimal")
set(CPACK_COMPONENT_APPLICATIONS_GROUP "Minimal")
set(CPACK_COMPONENT_HEADERS_GROUP "Minimal")
set(CPACK_COMPONENT_EXAMPLES_GROUP "Extra")
#CPack group descriptions
set(CPACK_COMPONENT_GROUP_MINIMAL_DESCRIPTION
	"Just the M+M libraries and core executables")
set(CPACK_COMPONENT_GROUP_EVERYTHING_DESCRIPTION
	"Everything, including sample M+M Applications")
	
set(CPACK_COMPONENTS_ALL applications libraries headers examples)
include(CPack)
#end packing stuff


add_subdirectory(ClientList)
add_subdirectory(CommonTests)
add_subdirectory(examples)
add_subdirectory(FindServices)
add_subdirectory(LeapMotion)
if(WIN32)
    add_subdirectory(KinectV2)
endif(WIN32)
add_subdirectory(mpm)
add_subdirectory(odl)
add_subdirectory(ParserTest)
add_subdirectory(PortLister)
add_subdirectory(RegistryService)
add_subdirectory(RequestCounter)
add_subdirectory(RequestInfo)
add_subdirectory(ServiceLister)
add_subdirectory(ServiceMetrics)
add_subdirectory(Version)

enable_testing()

# platform independent install step:
#
# TODO: copy our yarp folder to M+M install location/yarp
# TODO: copy our ace folder to M+M install location/ACE_wrappers
#

#Windows specific install step
if(WIN32)
# copy yarp and ACE DLL's to install location
# set env variables for MpM_YARP_DIR and MpM_ACE_ROOT
# so VS external, preconfigured projects can find them
endif(WIN32)

