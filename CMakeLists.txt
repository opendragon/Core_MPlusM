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
#  Copyright:  (c) 2014 by H Plus Technologies Ltd. and Simon Fraser University.
#
#              All rights reserved. Redistribution and use in source and binary forms, with or
#              without modification, are permitted provided that the following conditions are met:
#                * Redistributions of source code must retain the above copyright notice, this list
#                  of conditions and the following disclaimer.
#                * Redistributions in binary form must reproduce the above copyright notice, this
#                  list of conditions and the following disclaimer in the documentation and / or
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
set(MpM_VERSION_PATCH 25)
set(MpM_VERSION_STRING "${MpM_VERSION_MAJOR}.${MpM_VERSION_MINOR}.${MpM_VERSION_PATCH}")
set(MpM_COMPANY "H Plus Technologies Ltd. and Simon Fraser University")

set(MpM_SOURCE_DIR "${PROJECT_SOURCE_DIR}")

# Make some flags to control the build flags visibilities
mark_as_advanced(CLEAR BUILD_SHARED_LIBS)
mark_as_advanced(CLEAR CMAKE_OSX_ARCHITECTURES)
mark_as_advanced(SET CMAKE_INSTALL_RPATH)
mark_as_advanced(SET CMAKE_BUILD_WITH_INSTALL_RPATH)

if(APPLE)
    set(E_PATH "@executable_path")
	set(CMAKE_OSX_ARCHITECTURES "i386;x86_64" CACHE STRING "Build architectures for Mac OS X" FORCE)
	set(CMAKE_CXX_FLAGS "-std=c++11 -stdlib=libstdc++" CACHE STRING
        "Fixes linking error for M+M/ACE" FORCE)
    set(CMAKE_INSTALL_PREFIX "/opt/M+M" CACHE STRING "Fixes install path for M+M/ACE" FORCE)
    set(CMAKE_INSTALL_RPATH
"${E_PATH}/../lib;${E_PATH}/../mpm;${E_PATH}/../odl;${E_PATH}/../../mpm;${E_PATH}/../../odl;${CMAKE_INSTALL_PREFIX}/lib"
        CACHE STRING "Fixes rpath" FORCE)
    set(CMAKE_BUILD_WITH_INSTALL_RPATH ON CACHE BOOL "Make sure to use rpath")
# 'rpath' warning
    cmake_policy(SET CMP0042 OLD)
endif(APPLE)

if(WIN32)
    if(NOT ("${CMAKE_EXE_LINKER_FLAGS}" MATCHES ".*NODEFAULTLIB:libcmt.*"))
        set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /NODEFAULTLIB:libcmt" CACHE STRING
                "Flags used by the linker." FORCE)
    endif(NOT ("${CMAKE_EXE_LINKER_FLAGS}" MATCHES ".*NODEFAULTLIB:libcmt.*"))
endif(WIN32)

set(CMAKE_USE_RELATIVE_PATHS ON CACHE BOOL "Always set file paths as relative")

option(MpM_ChattyStart "Report the version numbers when launching an executable")
option(MpM_DoExplicitClose "Perform an explicit CloseChannel() prior to freeing a channel")
option(MpM_DoExplicitDisconnect "Perform an explicit disconnect prior to closing a channel")
option(MpM_DontUseTimeouts "Allow operations to take arbitrary lengths of time")
option(MpM_LogIncludesYarpTrace "Include YARP trace when logging")
option(MpM_MainDoesDelayNotYield "Use Delay() in main() loop")
option(MpM_MetricsInitiallyOn "Collect channel measurement data by default")
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

option(MpM_NATURALPOINT "Build the NaturalPoint service" ON)
option(MpM_PROCOMP2 "Build the ProComp2 service" ON)
option(MpM_VICON "Build the Vicon Input service" ON)
option(MpM_DO_SWIG "Build the SWIG files" OFF)

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

# Find SWIG
if(MpM_DO_SWIG)
    find_package(SWIG REQUIRED)
    include(${SWIG_USE_FILE})
    set(CMAKE_SWIG_FLAGS "")
endif(MpM_DO_SWIG)

set(MpM_ADAPTER_BASE_NAME "/_adapter_/")
set(MpM_CLIENT_BASE_NAME "/_client_/")
set(MpM_INPUT_BASE_NAME "/_input_/")
set(MpM_OUTPUT_BASE_NAME "/_output_/")
set(MpM_SERVICE_BASE_NAME "/_service_/")

set(MpM_SQLITE_DIR "${PROJECT_SOURCE_DIR}/SQLite3")

configure_file("${MpM_SOURCE_DIR}/mpm/M+MConfig.h.in" "${MpM_SOURCE_DIR}/mpm/M+MConfig.h")

include_directories("${MpM_SOURCE_DIR}")

#TODO: add bonjour to package
#TODO: deal with ACE dependency

if(WIN32)
    set(BONJOUR_DIR "C:/Program Files/Bonjour SDK")
    set(BONJOUR_LIB "${BONJOUR_DIR}/Lib/Win32/dnssd.lib")
    include_directories("${BONJOUR_DIR}/Include")
else(WIN32)
    set(BONJOUR_LIB "")
endif(WIN32)

add_subdirectory(Address)
add_subdirectory(ClientList)
add_subdirectory(CommonTests)
add_subdirectory(examples)
add_subdirectory(FindServices)
#if(APPLE)
    add_subdirectory(JavaScript)
#endif(APPLE)
add_subdirectory(LeapMotion)
if(WIN32)
    add_subdirectory(KinectV2)
endif(WIN32)
add_subdirectory(mpm)
if(WIN32 AND MpM_NATURALPOINT)
    add_subdirectory(NaturalPoint)
endif(WIN32 AND MpM_NATURALPOINT)
add_subdirectory(odl)
add_subdirectory(ParserTest)
add_subdirectory(PortLister)
if(WIN32 AND MpM_PROCOMP2)
    add_subdirectory(ProComp2)
endif(WIN32 AND MpM_PROCOMP2)
add_subdirectory(RecordAsJSON)
add_subdirectory(RegistryService)
add_subdirectory(RequestCounter)
add_subdirectory(RequestInfo)
add_subdirectory(ServiceLister)
add_subdirectory(ServiceMetrics)
add_subdirectory(Tunnel)
add_subdirectory(Unreal)
add_subdirectory(Version)
if(WIN32 AND MpM_VICON)
	add_subdirectory(ViconDataStream)
endif(WIN32 AND MpM_VICON)

add_subdirectory(CPack)

enable_testing()
