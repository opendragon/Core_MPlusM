#--------------------------------------------------------------------------------------------------
#
#  File:       OpenStageInputService/CMakeLists.txt
#
#  Project:    m+m
#
#  Contains:   The CMAKE definitions for the Organic Motion OpenStage input service application.
#
#  Written by: Norman Jaffe
#
#  Copyright:  (c) 2015 by H Plus Technologies Ltd. and Simon Fraser University.
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
#  Created:    2015-06-25
#
#--------------------------------------------------------------------------------------------------

set(THIS_TARGET m+mOpenStageInputService)

get_filename_component(CUR_DIR ${CMAKE_CURRENT_LIST_FILE} PATH)

set(OPENSTAGESDK_DIR "${CUR_DIR}/om_sdk")

set(OPENSTAGESDK_INCLUDE_DIR "${OPENSTAGESDK_DIR}/include")

set(OPENSTAGESDK_LIBRARY "${OPENSTAGESDK_DIR}/lib/omsdk2.lib")
set(OPENSTAGESDK_UUID_LIBRARY "${OPENSTAGESDK_DIR}/lib/omuuid.lib")

set(OPENSTAGESDK_INCLUDE_DIRS "${OPENSTAGESDK_INCLUDE_DIR}")
set(OPENSTAGESDK_LIBRARIES "${OPENSTAGESDK_LIBRARY}" "${OPENSTAGESDK_UUID_LIBRARY}")

set(OPENSTAGESDK_BINARIES
    "${OPENSTAGESDK_DIR}/bin/omsdk2.dll"
    "${OPENSTAGESDK_DIR}/bin/libglog.dll"
    "${OPENSTAGESDK_DIR}/bin/tbb.dll")

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

include_directories(glm)
include_directories("${OPENSTAGESDK_INCLUDE_DIRS}")

# Set up our program
add_executable(${THIS_TARGET}
               m+mOpenStageInputServiceMain.cpp
               m+mOpenStageInputService.cpp
               m+mOpenStageInputThread.cpp
               ${VERS_RESOURCE})

# Note that the order of inclusion of libraries is critical in Linux, as they appear to only be
# processed once.
if(MpM_BuildDummyServices)
    target_link_libraries(${THIS_TARGET} ${MpM_LINK_LIBRARIES})
else()
    target_link_libraries(${THIS_TARGET} "${OPENSTAGESDK_LIBRARIES}" ${MpM_LINK_LIBRARIES})
endif()

if(WIN32)
    install(FILES
            "${OPENSTAGESDK_DIR}/bin/omsdk2.dll"
            "${OPENSTAGESDK_DIR}/bin/libglog.dll"
            "${OPENSTAGESDK_DIR}/bin/tbb.dll"
            DESTINATION bin
            COMPONENT applications)
endif()

fix_dynamic_libs(${THIS_TARGET})

install(TARGETS ${THIS_TARGET}
        DESTINATION bin
        COMPONENT applications)

set(THIS_TARGET m+mOpenStageBlobInputService)

if(WIN32)
    set(VERS_RESOURCE ${THIS_TARGET}.rc)
else()
    set(VERS_RESOURCE "")
endif()

configure_file(${THIS_TARGET}.rc.in ${THIS_TARGET}.rc)

include_directories(glm)
include_directories("${OPENSTAGESDK_INCLUDE_DIRS}")

# Set up our program
add_executable(${THIS_TARGET}
                m+mOpenStageBlobInputServiceMain.cpp
                m+mOpenStageBlobInputService.cpp
                m+mOpenStageBlobInputThread.cpp
                ${VERS_RESOURCE})

# Note that the order of inclusion of libraries is critical in Linux, as they appear to only be
# processed once.
if(MpM_BuildDummyServices)
    target_link_libraries(${THIS_TARGET} ${MpM_LINK_LIBRARIES})
else()
    target_link_libraries(${THIS_TARGET} "${OPENSTAGESDK_LIBRARIES}" ${MpM_LINK_LIBRARIES})
endif()

fix_dynamic_libs(${THIS_TARGET})

install(TARGETS ${THIS_TARGET}
        DESTINATION bin
        COMPONENT applications)

enable_testing()
