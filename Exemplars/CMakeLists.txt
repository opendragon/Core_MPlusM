#--------------------------------------------------------------------------------------------------
#
#  File:       Exemplars/CMakeLists.txt
#
#  Project:    m+m
#
#  Contains:   The common CMAKE definitions for the m+m exemplar files.
#
#  Written by: Norman Jaffe
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
#  Created:    2014-09-15
#
#--------------------------------------------------------------------------------------------------

# YARP needs CMake 2.6 or greater
cmake_minimum_required(VERSION 2.6)

if(WIN32)
    set(CMAKE_CONFIGURATION_TYPES Debug Release CACHE INTERNAL "" FORCE)
endif()

project(m+mExemplars)

# Make some flags to control the build flags visibilities
mark_as_advanced(CLEAR BUILD_SHARED_LIBS)
mark_as_advanced(CLEAR CMAKE_OSX_ARCHITECTURES)
mark_as_advanced(SET CMAKE_INSTALL_RPATH)
mark_as_advanced(SET CMAKE_BUILD_WITH_INSTALL_RPATH)

if(APPLE)
    set(E_PATH "@executable_path")
    set(CMAKE_OSX_ARCHITECTURES "x86_64" CACHE STRING "Build architectures for Mac OS X" FORCE)
    set(CMAKE_OSX_DEPLOYMENT_TARGET "10.7" CACHE STRING "Deployment target for Mac OSX" FORCE)
    set(CMAKE_CXX_FLAGS "-Wall -Wextra -std=c++11" CACHE STRING "Fixes linking error for m+m/ACE"
        FORCE)
#    set(CMAKE_CXX_FLAGS "-Wall -Wextra -std=c++11 -stdlib=libstdc++" CACHE STRING
#        "Fixes linking error for m+m/ACE" FORCE)
    set(CMAKE_EXE_LINKER_FLAGS "-headerpad_max_install_names -L${CMAKE_INSTALL_PREFIX}/lib" CACHE
        STRING "Increases link limit" FORCE)
    set(CMAKE_INSTALL_PREFIX "/opt/m+m" CACHE STRING "Fixes install path for m+m/ACE" FORCE)
    set(CMAKE_INSTALL_RPATH
"${E_PATH}/../lib;${E_PATH}/../m+m;${E_PATH}/../odl;${E_PATH}/../../m+m;${E_PATH}/../../odl;${CMAKE_INSTALL_PREFIX}/lib"
        CACHE STRING "Fixes rpath" FORCE)
    set(CMAKE_BUILD_WITH_INSTALL_RPATH ON CACHE BOOL "Make sure to use rpath")
# 'rpath' warning
    cmake_policy(SET CMP0042 OLD)
endif()

set(CMAKE_USE_RELATIVE_PATHS ON CACHE BOOL "Always set file paths as relative")

if(WIN32)
    option(WIN64 "Build 64-bit binaries" ON)
else()
    set(WIN64 OFF)
endif()

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

set(MpMExemplars_SOURCE_DIR "${PROJECT_SOURCE_DIR}")

include_directories("${MpMExemplars_SOURCE_DIR}")

if(WIN32)
#    set(ODL_DIR "??")
else()
    set(ODL_DIR "/opt/odl/lib")
endif()
find_package(ODL 1.0.0 REQUIRED)
if(ODL_FOUND)
    include_directories(${ODL_INCLUDE_DIRS})
endif()

set(MpM_LINK_LIBRARIES m+mCommon ${ODL_LIBRARY} ${YARP_LIBRARIES} ${ACE_LIBRARIES} ${BONJOUR_LIB})

if(LINUX)
    set(MpM_LINK_LIBRARIES ${MpM_LINK_LIBRARIES} pthread dns_sd)
endif()

if(APPLE)
    function(fix_dynamic_libs target)
        add_custom_command(TARGET ${THIS_TARGET} POST_BUILD COMMAND install_name_tool -change
                            libACE.dylib "@rpath/libACE.dylib" ${target})

        add_custom_command(TARGET ${THIS_TARGET} POST_BUILD COMMAND install_name_tool -change
                            libm+mCommon.dylib "@rpath/libm+mCommon.dylib" ${target})

        add_custom_command(TARGET ${THIS_TARGET} POST_BUILD COMMAND install_name_tool -change
                            ${ODL_LIBRARY_ROOT} ${ODL_LIBRARY} ${target})

        add_custom_command(TARGET ${THIS_TARGET} POST_BUILD COMMAND install_name_tool -change
                            libYARP_OS.1.dylib "@rpath/libYARP_OS.1.dylib" ${target})
        add_custom_command(TARGET ${THIS_TARGET} POST_BUILD COMMAND install_name_tool -change
                            libYARP_sig.1.dylib "@rpath/libYARP_sig.1.dylib" ${target})
        add_custom_command(TARGET ${THIS_TARGET} POST_BUILD COMMAND install_name_tool -change
                            libYARP_dev.1.dylib "@rpath/libYARP_dev.1.dylib" ${target})
        add_custom_command(TARGET ${THIS_TARGET} POST_BUILD COMMAND install_name_tool -change
                            libYARP_name.1.dylib "@rpath/libYARP_name.1.dylib" ${target})
        add_custom_command(TARGET ${THIS_TARGET} POST_BUILD COMMAND install_name_tool -change
                            libYARP_init.1.dylib "@rpath/libYARP_init.1.dylib" ${target})
    endfunction()
else()
# Do nothing
    function(fix_dynamic_libs target)
    endfunction()
endif()

# Add the subdirectories for a client / service
add_subdirectory(ExemplarAdapter)
add_subdirectory(ExemplarClient)
add_subdirectory(ExemplarService)

# Add the subdirectories for input / output services
add_subdirectory(ExemplarFilterService)
add_subdirectory(ExemplarInputService)
add_subdirectory(ExemplarOutputService)

enable_testing()
