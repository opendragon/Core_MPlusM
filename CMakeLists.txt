#--------------------------------------------------------------------------------------------------
#
#  File:       CMakeLists.txt
#
#  Project:    m+m
#
#  Contains:   The common CMAKE definitions for the m+m project.
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

if(WIN32)
    set(CMAKE_CONFIGURATION_TYPES Debug Release CACHE INTERNAL "" FORCE)
endif(WIN32)

project(m+m)

set(MpM_VERSION_MAJOR 1)
set(MpM_VERSION_MINOR 6)
set(MpM_VERSION_PATCH 7)
set(MpM_VERSION_STRING "${MpM_VERSION_MAJOR}.${MpM_VERSION_MINOR}.${MpM_VERSION_PATCH}")
set(MpM_COMPANY "H Plus Technologies Ltd. and Simon Fraser University")

set(MpM_SOURCE_DIR "${PROJECT_SOURCE_DIR}")

# Make some flags to control the build flags visibilities
mark_as_advanced(CLEAR BUILD_SHARED_LIBS)
mark_as_advanced(CLEAR CMAKE_OSX_ARCHITECTURES)
mark_as_advanced(SET CMAKE_INSTALL_RPATH)
mark_as_advanced(SET CMAKE_BUILD_WITH_INSTALL_RPATH)

if(CMAKE_HOST_SYSTEM_NAME STREQUAL "Linux")
    set(LINUX ON)
    if(CMAKE_HOST_SYSTEM_PROCESSOR STREQUAL "unknown")
        set(UNKNOWN_CPU ON)
    else(CMAKE_HOST_SYSTEM_PROCESSOR STREQUAL "unknown")
        set(UNKNOWN_CPU OFF)
    endif(CMAKE_HOST_SYSTEM_PROCESSOR STREQUAL "unknown")
else(CMAKE_HOST_SYSTEM_NAME STREQUAL "Linux")
    set(LINUX OFF)
    set(UNKNOWN_CPU OFF)
endif(CMAKE_HOST_SYSTEM_NAME STREQUAL "Linux")

if(APPLE)
    set(E_PATH "@executable_path")
    set(CMAKE_OSX_ARCHITECTURES "i386;x86_64" CACHE STRING "Build architectures for Mac OS X" FORCE)
    set(CMAKE_CXX_FLAGS "-std=c++11 -stdlib=libstdc++" CACHE STRING
        "Fixes linking error for m+m/ACE" FORCE)
    set(CMAKE_EXE_LINKER_FLAGS "-headerpad_max_install_names" CACHE STRING "Increases link limit"
        FORCE)
    set(CMAKE_INSTALL_PREFIX "/opt/m+m" CACHE STRING "Fixes install path for m+m/ACE" FORCE)
    set(CMAKE_INSTALL_RPATH
"${E_PATH}/../lib;${E_PATH}/../m+m;${E_PATH}/../odl;${E_PATH}/../../m+m;${E_PATH}/../../odl;${CMAKE_INSTALL_PREFIX}/lib"
        CACHE STRING "Fixes rpath" FORCE)
    set(CMAKE_BUILD_WITH_INSTALL_RPATH ON CACHE BOOL "Make sure to use rpath")
# 'rpath' warning
    cmake_policy(SET CMP0042 OLD)
endif(APPLE)
if(LINUX)
    set(CMAKE_CXX_FLAGS "-std=c++11" CACHE STRING "Fixes compile problems with JavaScript" FORCE)
endif(LINUX)

if(WIN32)
    if(NOT ("${CMAKE_EXE_LINKER_FLAGS}" MATCHES ".*NODEFAULTLIB:libcmt.*"))
        set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /NODEFAULTLIB:libcmt" CACHE STRING
                "Flags used by the linker." FORCE)
    endif(NOT ("${CMAKE_EXE_LINKER_FLAGS}" MATCHES ".*NODEFAULTLIB:libcmt.*"))
endif(WIN32)

set(CMAKE_USE_RELATIVE_PATHS ON CACHE BOOL "Always set file paths as relative")

if(WIN32)
    option(WIN64 "Build 64-bit binaries" ON)
else(WIN32)
    set(WIN64 OFF)
endif(WIN32)

if(APPLE)
    option(MpM_BuildDummyServices "Build dummy services for screen capture")
else(APPLE)
    set(MpM_BuildDummyServices OFF)
endif(APPLE)

option(MpM_ChattyStart "Report the version numbers when launching an executable")

option(MpM_DoExplicitCheckForOK "Check OK responses for validity")

option(MpM_DoExplicitClose "Perform an explicit CloseChannel() prior to freeing a channel")
mark_as_advanced(MpM_DoExplicitClose)

option(MpM_DoExplicitDisconnect "Perform an explicit disconnect prior to closing a channel")
mark_as_advanced(MpM_DoExplicitDisconnect)

option(MpM_DontUseTimeouts "Allow operations to take arbitrary lengths of time")
mark_as_advanced(MpM_DontUseTimeouts)

option(MpM_LogIncludesYarpTrace "Include YARP trace when logging")

option(MpM_MetricsInitiallyOn "Collect channel measurement data by default")

option(MpM_ReportContactDetails
        "Report details of the contacts during operations that might change them")
mark_as_advanced(MpM_ReportContactDetails)

option(MpM_ReportOnConnections "Report on connections")

option(MpM_ServicesLogToStandardError "Services send logging output to stderr" ON)

option(MpM_StallOnSendProblem "A send failure goes to a tight loop")
mark_as_advanced(MpM_StallOnSendProblem)

option(MpM_UseCustomStringBuffer "Use a custom string buffer for large string output" ON)
mark_as_advanced(MpM_UseCustomStringBuffer)

option(MpM_UseDiskDatabase "Use a disk-based database, rather than in-memory")
mark_as_advanced(MpM_UseDiskDatabase)

option(MpM_UseTestDatabase "Use a test database, in /tmp, rather than a random disk location")
mark_as_advanced(MpM_UseTestDatabase)

option(MpM_UseTimeoutsInRetryLoops "Use timeouts in retry loops")
mark_as_advanced(MpM_UseTimeoutsInRetryLoops)

if(WIN32)
    if(WIN64)
        set(MpM_COMMONLISP OFF)
    else(WIN64)
        option(MpM_COMMONLISP "Build the Common Lisp filter service" ON)
    endif(WIN64)
else(WIN32)
    option(MpM_COMMONLISP "Build the Common Lisp filter service" ON)
endif(WIN32)
option(MpM_JAVASCRIPT "Build the JavaScript filter service" ON)
if(WIN32)
    option(MpM_KINECTV2 "Build the KinectV2 input services" ON)
else(WIN32)
    set(MpM_KINECTV2 ${MpM_BuildDummyServices})
endif(WIN32)
option(MpM_LEAPMOTION "Build the Leap Motion input services" ON)
if(WIN32)
    option(MpM_NATNET "Build the NatNet input services" ON)
else(WIN32)
    set(MpM_NATNET ${MpM_BuildDummyServices})
endif(WIN32)
if(WIN32)
    if(WIN64)
        option(MpM_OPENSTAGE "Build the OpenStage input services" ON)
    else(WIN64)
        set(MpM_OPENSTAGE OFF)
    endif(WIN64)
else(WIN32)
    set(MpM_OPENSTAGE ${MpM_BuildDummyServices})
endif(WIN32)
if(WIN32)
    if(WIN64)
        set(MpM_PROCOMP2 OFF)
    else(WIN64)
        option(MpM_PROCOMP2 "Build the ProComp2 input service" ON)
    endif(WIN64)
else(WIN32)
    set(MpM_PROCOMP2 ${MpM_BuildDummyServices})
endif(WIN32)
#if(WIN32)
    set(MpM_PYTHON OFF)
#else(WIN32)
#    option(MpM_PYTHON "Build the Python filter service" OFF)
#endif(WIN32)
if(WIN32)
    if(WIN64)
        option(MpM_SENDTOMQ "Build the SendToMQ output service" ON)
    else(WIN64)
        set(MpM_SENDTOMQ OFF)
    endif(WIN64)
else(WIN32)
    option(MpM_SENDTOMQ "Build the SendToMQ output service" ON)
endif(WIN32)
option(MpM_UNREAL "Build the Unreal output service" OFF)
mark_as_advanced(MpM_UNREAL)
if(WIN32)
    option(MpM_VICON "Build the Vicon input services" ON)
else(WIN32)
    set(MpM_VICON ${MpM_BuildDummyServices})
endif(WIN32)
option(MpM_DO_SWIG "Build the SWIG files" OFF)

# Add the m+m target path so that YARP and ACE can be found
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

set(MpM_BASE_NAME_PREFIX "/")
set(MpM_NAME_SEPARATOR "/")

set(MpM_ADAPTER_BASE_NAME "${MpM_BASE_NAME_PREFIX}_adapter_")
set(MpM_CLIENT_BASE_NAME "${MpM_BASE_NAME_PREFIX}_client_")
set(MpM_INPUT_BASE_NAME "${MpM_BASE_NAME_PREFIX}_input_")
set(MpM_OUTPUT_BASE_NAME "${MpM_BASE_NAME_PREFIX}_output_")
set(MpM_REGISTRY_EXECUTABLE_NAME "m+mRegistryService")
set(MpM_SERVICE_BASE_NAME "${MpM_BASE_NAME_PREFIX}_service_")

set(MpM_SQLITE_DIR "${PROJECT_SOURCE_DIR}/SQLite3")

configure_file("${MpM_SOURCE_DIR}/m+m/m+mConfig.h.in" "${MpM_SOURCE_DIR}/m+m/m+mConfig.h")

include_directories("${MpM_SOURCE_DIR}")

if(WIN32)
    set(BONJOUR_DIR "C:/Program Files/Bonjour SDK")
    if(WIN64)
        set(BONJOUR_LIB "${BONJOUR_DIR}/Lib/x64/dnssd.lib")
    else(WIN64)
        set(BONJOUR_LIB "${BONJOUR_DIR}/Lib/Win32/dnssd.lib")
    endif(WIN64)
    include_directories("${BONJOUR_DIR}/Include")
else(WIN32)
    set(BONJOUR_LIB "")
endif(WIN32)

set(MpM_LINK_LIBRARIES m+mCommon odLogging ${YARP_LIBRARIES} ${ACE_LIBRARIES} ${BONJOUR_LIB})

if(LINUX)
    set(MpM_LINK_LIBRARIES ${MpM_LINK_LIBRARIES} pthread dns_sd)
endif(LINUX)

add_subdirectory(Address)
add_subdirectory(Blob)
add_subdirectory(ClientList)
if(MpM_COMMONLISP)
    add_subdirectory(CommonLisp)
endif(MpM_COMMONLISP)
add_subdirectory(CommonTests)
add_subdirectory(examples)
add_subdirectory(FindServices)
if(MpM_JAVASCRIPT)
    add_subdirectory(JavaScript)
endif(MpM_JAVASCRIPT)
if(MpM_KINECTV2)
    add_subdirectory(KinectV2)
endif(MpM_KINECTV2)
if(MpM_LEAPMOTION)
    add_subdirectory(LeapMotion)
endif(MpM_LEAPMOTION)
add_subdirectory(m+m)
if(MpM_NATNET)
    add_subdirectory(NatNet)
endif(MpM_NATNET)
add_subdirectory(odl)
if(MpM_OPENSTAGE)
    add_subdirectory(OpenStage)
endif(MpM_OPENSTAGE)
add_subdirectory(ParserTest)
add_subdirectory(PortList)
if(MpM_PROCOMP2)
    add_subdirectory(ProComp2)
endif(MpM_PROCOMP2)
if(MpM_PYTHON)
    add_subdirectory(Python)
endif(MpM_PYTHON)
add_subdirectory(Registry)
add_subdirectory(RequestCounter)
add_subdirectory(RequestInfo)
if(MpM_SENDTOMQ)
    add_subdirectory(SendToMQ)
endif(MpM_SENDTOMQ)
add_subdirectory(ServiceList)
add_subdirectory(ServiceMetrics)
add_subdirectory(ServiceStop)
add_subdirectory(Tunnel)
if(MpM_UNREAL)
    add_subdirectory(Unreal)
endif(MpM_UNREAL)
add_subdirectory(Version)
if(MpM_VICON)
    add_subdirectory(ViconDataStream)
endif(MpM_VICON)

add_subdirectory(CPack)

enable_testing()
