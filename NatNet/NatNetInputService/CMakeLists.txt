#--------------------------------------------------------------------------------------------------
#
#  File:       NatNetInputService/CMakeLists.txt
#
#  Project:    m+m
#
#  Contains:   The CMAKE definitions for the Natural Point NatNet input service application.
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
#  Created:    2015-04-13
#
#--------------------------------------------------------------------------------------------------

set(THIS_TARGET m+mNatNetInputService)

if(WIN32)
    set(VERS_RESOURCE ${THIS_TARGET}.rc)
else()
    set(VERS_RESOURCE "")
endif()

configure_file(${THIS_TARGET}.rc.in ${THIS_TARGET}.rc)

# Set up our program
add_executable(${THIS_TARGET}
               m+mNatNetInputServiceMain.cpp
               m+mNatNetInputService.cpp
               m+mNatNetInputThread.cpp
               ${VERS_RESOURCE})

if(WIN32)
    include_directories("${MpMNATNET_SOURCE_DIR}/NatNetInputService/NatNetSDK/include")
    if(WIN64)
        set(MpMNATNET_LIB_DIR "${MpMNATNET_SOURCE_DIR}/NatNetInputService/NatNetSDK/lib/x64")
    else()
        set(MpMNATNET_LIB_DIR "${MpMNATNET_SOURCE_DIR}/NatNetInputService/NatNetSDK/lib")
    endif()
    add_library(NatNet SHARED IMPORTED)
    set_property(TARGET NatNet PROPERTY IMPORTED_IMPLIB "${MpMNATNET_LIB_DIR}/NatNetLib.lib")
    set_property(TARGET NatNet PROPERTY IMPORTED_LOCATION "${MpMNATNET_LIB_DIR}/NatNetLib.dll")
    add_custom_command(TARGET ${THIS_TARGET} POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy_directory
                       "${MpMNATNET_LIB_DIR}" $<TARGET_FILE_DIR:m+mNatNetInputService>)
    install(FILES
            "${MpMNATNET_LIB_DIR}/NatNetLib.dll"
            DESTINATION bin
            COMPONENT applications)
endif()

# Note that the order of inclusion of libraries is critical in Linux, as they appear to only be
# processed once.
if(MpM_BuildDummyServices)
    target_link_libraries(${THIS_TARGET} ${MpM_LINK_LIBRARIES})
else()
    target_link_libraries(${THIS_TARGET} NatNet ${MpM_LINK_LIBRARIES})
endif()

fix_dynamic_libs(${THIS_TARGET})

install(TARGETS ${THIS_TARGET}
        DESTINATION bin
        COMPONENT applications)

set(THIS_TARGET m+mNatNetBlobInputService)

if(WIN32)
    set(VERS_RESOURCE ${THIS_TARGET}.rc)
else()
    set(VERS_RESOURCE "")
endif()

configure_file(${THIS_TARGET}.rc.in ${THIS_TARGET}.rc)

# Set up our program
add_executable(${THIS_TARGET}
                m+mNatNetBlobInputServiceMain.cpp
                m+mNatNetBlobInputService.cpp
                m+mNatNetBlobInputThread.cpp
                ${VERS_RESOURCE})

# Note that the order of inclusion of libraries is critical in Linux, as they appear to only be
# processed once.
if(MpM_BuildDummyServices)
    target_link_libraries(${THIS_TARGET} ${MpM_LINK_LIBRARIES})
else()
    target_link_libraries(${THIS_TARGET} NatNet ${MpM_LINK_LIBRARIES})
endif()

fix_dynamic_libs(${THIS_TARGET})

install(TARGETS ${THIS_TARGET}
        DESTINATION bin
        COMPONENT applications)

enable_testing()
