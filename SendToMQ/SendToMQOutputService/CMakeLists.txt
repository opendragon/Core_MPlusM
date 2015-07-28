#--------------------------------------------------------------------------------------------------
#
#  File:       SendToMQOutputService/CMakeLists.txt
#
#  Project:    m+m
#
#  Contains:   The CMAKE definitions for the SendToMQ output service application.
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
#  Created:    2015-07-26
#
#--------------------------------------------------------------------------------------------------

set(THIS_TARGET m+mSendToMQOutputService)

if(WIN32)
    set(VERS_RESOURCE ${THIS_TARGET}.rc)
else(WIN32)
    set(VERS_RESOURCE "")
endif(WIN32)

configure_file(${THIS_TARGET}.rc.in ${THIS_TARGET}.rc)

if(WIN32)
    if(WIN64)
        set(APR_INC_PATH "")
        set(ACTIVEMQ_INC_PATH "C:/Program Files/m+m/spidermonkey") #!!!!
    else(WIN64)
        set(APR_INC_PATH "")
        set(ACTIVEMQ_INC_PATH "C:/Program Files (x86)/m+m/spidermonkey") #!!!!!
    endif(WIN64)
endif(WIN32)
if(APPLE)
    set(APR_INC_PATH "/usr/include/apr-1")
    set(ACTIVEMQ_INC_PATH "/opt/m+m/include/activemq-cpp-3.9.0")
endif(APPLE)

#include_directories(${APR_INC_PATH})
include_directories(${ACTIVEMQ_INC_PATH})

# Set up our program
add_executable(${THIS_TARGET}
               m+mSendToMQOutputServiceMain.cpp
               m+mSendToMQOutputInputHandler.cpp
               m+mSendToMQOutputService.cpp
               ${VERS_RESOURCE})

add_library(activemq SHARED IMPORTED)

if(WIN32)
    if(WIN64)
        set(MpMLEAPMOTION_LIB_DIR "${MpMLEAPMOTION_SOURCE_DIR}/LeapMotionInputService/win_lib/x64") #!!!!
    else(WIN64)
        set(MpMLEAPMOTION_LIB_DIR "${MpMLEAPMOTION_SOURCE_DIR}/LeapMotionInputService/win_lib/x86") #!!!!
    endif(WIN64)
    set_property(TARGET activemq PROPERTY IMPORTED_LOCATION "${MpMLEAPMOTION_LIB_DIR}/Leap.dll") #!!!!
    set_property(TARGET activemq PROPERTY IMPORTED_IMPLIB "${MpMLEAPMOTION_LIB_DIR}/Leap.lib") #!!!!
    add_custom_command(TARGET ${THIS_TARGET} POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy_directory #!!!!
                        "${MpMLEAPMOTION_LIB_DIR}" #!!!!
                        $<TARGET_FILE_DIR:m+mLeapMotionInputService>) #!!!!
    install(FILES "${MpMLEAPMOTION_LIB_DIR}/Leap.dll" #!!!!
            DESTINATION bin
            COMPONENT applications)
endif(WIN32)

if(APPLE)
    set_property(TARGET activemq PROPERTY IMPORTED_LOCATION "/opt/m+m/lib/libactivemq-cpp.dylib")

    add_custom_command(TARGET ${THIS_TARGET} POST_BUILD COMMAND install_name_tool -change
                        libactivemq-cpp.dylib "@rpath/libactivemq-cpp.dylib" ${THIS_TARGET})

    add_custom_command(TARGET ${THIS_TARGET} POST_BUILD COMMAND install_name_tool -change
                        libACE.dylib "@rpath/libACE.dylib" ${THIS_TARGET})

    add_custom_command(TARGET ${THIS_TARGET} POST_BUILD COMMAND install_name_tool -change
                        libm+mCommon.dylib "@rpath/libm+mCommon.dylib" ${THIS_TARGET})

    add_custom_command(TARGET ${THIS_TARGET} POST_BUILD COMMAND install_name_tool -change
                        libodLogging.dylib "@rpath/libodLogging.dylib" ${THIS_TARGET})

    add_custom_command(TARGET ${THIS_TARGET} POST_BUILD COMMAND install_name_tool -change
                        libYARP_OS.1.dylib "@rpath/libYARP_OS.1.dylib" ${THIS_TARGET})
    add_custom_command(TARGET ${THIS_TARGET} POST_BUILD COMMAND install_name_tool -change
                        libYARP_sig.1.dylib "@rpath/libYARP_sig.1.dylib" ${THIS_TARGET})
    add_custom_command(TARGET ${THIS_TARGET} POST_BUILD COMMAND install_name_tool -change
                        libYARP_dev.1.dylib "@rpath/libYARP_dev.1.dylib" ${THIS_TARGET})
    add_custom_command(TARGET ${THIS_TARGET} POST_BUILD COMMAND install_name_tool -change
                        libYARP_name.1.dylib "@rpath/libYARP_name.1.dylib" ${THIS_TARGET})
    add_custom_command(TARGET ${THIS_TARGET} POST_BUILD COMMAND install_name_tool -change
                        libYARP_init.1.dylib "@rpath/libYARP_init.1.dylib" ${THIS_TARGET})
endif(APPLE)

# Note that the order of inclusion of libraries is critical in Linux, as they appear to only be
# processed once.
target_link_libraries(${THIS_TARGET}
                        m+mCommon
                        odLogging
                        activemq
                        ${YARP_LIBRARIES}
                        ${ACE_LIBRARIES}
                        ${BONJOUR_LIB})

if(WIN32)
    target_link_libraries(${THIS_TARGET}
                            ${BONJOUR_LIB})
endif(WIN32)


install(TARGETS ${THIS_TARGET}
        DESTINATION bin
        COMPONENT applications)

enable_testing()
