#--------------------------------------------------------------------------------------
#
#  File:       RGBLEDService/CMakeLists.txt
#
#  Project:    m+m
#
#  Contains:   The CMAKE definitions for the example RGB service application.
#
#  Written by: Norman Jaffe; Johnty Wang
#
#  Copyright:  (c) 2014 by H Plus Technologies Ltd. and Simon Fraser University.
#
#              All rights reserved. Redistribution and use in source and binary forms,
#              with or without modification, are permitted provided that the following
#              conditions are met:
#                * Redistributions of source code must retain the above copyright
#                  notice, this list of conditions and the following disclaimer.
#                * Redistributions in binary form must reproduce the above copyright
#                  notice, this list of conditions and the following disclaimer in the
#                  documentation and / or other materials provided with the
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

cmake_minimum_required(VERSION 3.0)

set(THIS_TARGET m+mRGBLEDService)

if(WIN32)
    set(VERS_RESOURCE ${THIS_TARGET}.rc)
else()
    set(VERS_RESOURCE "")
endif()

configure_file(${THIS_TARGET}.rc.in ${THIS_TARGET}.rc)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_EXTENSIONS OFF)

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

if(APPLE)
    set(CMAKE_OSX_ARCHITECTURES "x86_64" CACHE STRING "Build architectures for Mac OSX" FORCE)
    set(CMAKE_OSX_DEPLOYMENT_TARGET "10.9" CACHE STRING "Deployment target for Mac OSX" FORCE)
    set(CMAKE_INSTALL_PREFIX "/opt/m+m" CACHE STRING "Fixes install path for m+m/ACE" FORCE)
    set(CMAKE_EXE_LINKER_FLAGS "-L${CMAKE_INSTALL_PREFIX}/lib" CACHE STRING "Fixes link path" FORCE)
endif()

set(CMAKE_USE_RELATIVE_PATHS ON CACHE BOOL "Always set file paths as relative")

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

# Set up our program
add_executable(${THIS_TARGET}
               m+mRGBLEDServiceMain.cpp
               m+mRGBLEDRequestHandler.cpp
               m+mRGBLEDService.cpp
               ${VERS_RESOURCE})

# Note that the order of inclusion of libraries is critical in Linux, as they appear to only be
# processed once.
target_link_libraries(${THIS_TARGET} ${MpM_LINK_LIBRARIES})

fix_dynamic_libs(${THIS_TARGET})

enable_testing()
