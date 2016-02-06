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

set(MQ_VERSION "3.10.0")

configure_file(${THIS_TARGET}.rc.in ${THIS_TARGET}.rc)

if(WIN32)
    if(WIN64)
        set(APR_INSTALL_PATH "C:/Program Files/m+m")
        set(ACTIVEMQ_INSTALL_PATH "C:/Program Files/m+m")
    else(WIN64)
        set(APR_INSTALL_PATH "C:/Program Files (x86)/m+m")
        set(ACTIVEMQ_INSTALL_PATH "C:/Program Files (x86)/m+m")
    endif(WIN64)
	set(APR_INC_PATH "${APR_INSTALL_PATH}/include")
	set(ACTIVEMQ_INC_PATH "${ACTIVEMQ_INSTALL_PATH}/include")
endif(WIN32)
if(APPLE)
    set(ACTIVEMQ_INC_PATH "${CMAKE_INSTALL_PREFIX}/include/activemq-cpp-${MQ_VERSION}")
endif(APPLE)
if(LINUX)
    set(ACTIVEMQ_INC_PATH "${CMAKE_INSTALL_PREFIX}/include/activemq-cpp-${MQ_VERSION}")
endif(LINUX)

include_directories(${ACTIVEMQ_INC_PATH})

if(WIN32)
	add_library(activemq STATIC IMPORTED)
    if(WIN64)
        set(MpMACTIVEMQ_LIB_DIR "${ACTIVEMQ_INSTALL_PATH}/lib")
	else(WIN64)
        set(MpMACTIVEMQ_LIB_DIR "${ACTIVEMQ_INSTALL_PATH}/lib")
    endif(WIN64)
    set_property(TARGET activemq PROPERTY IMPORTED_LOCATION
                    "${MpMACTIVEMQ_LIB_DIR}/libactivemq-cpp.lib")
	add_library(aprlib STATIC IMPORTED)
    if(WIN64)
        set(MpMAPR_LIB_DIR "${APR_INSTALL_PATH}/lib")
    else(WIN64)
        set(MpMAPR_LIB_DIR "${APR_INSTALL_PATH}/lib")
    endif(WIN64)
    set_property(TARGET aprlib PROPERTY IMPORTED_LOCATION "${MpMAPR_LIB_DIR}/libapr-1.lib")
	# APR and ActiveMQ libraries
    install(FILES
            "${APR_INSTALL_PATH}/bin/libapr-1.dll"
            DESTINATION bin
            COMPONENT applications)
    install(FILES
            "${MpMAPR_LIB_DIR}/apr-1.lib"
            "${MpMAPR_LIB_DIR}/aprapp-1.lib"
            "${MpMAPR_LIB_DIR}/libapr-1.lib"
            "${MpMAPR_LIB_DIR}/libaprapp-1.lib"
            "${MpMACTIVEMQ_LIB_DIR}/libactivemq-cpp.lib"
            DESTINATION lib
            COMPONENT libraries)
	# ActiveMQ headers
	install(DIRECTORY
			"${ACTIVEMQ_INC_PATH}/activemq"
			DESTINATION include
			COMPONENT libraries)
	install(DIRECTORY
			"${ACTIVEMQ_INC_PATH}/cms"
			DESTINATION include
			COMPONENT libraries)
	install(DIRECTORY
			"${ACTIVEMQ_INC_PATH}/decaf"
			DESTINATION include
			COMPONENT libraries)
	# APR headers
	install(DIRECTORY
			"${APR_INC_PATH}/"
			DESTINATION include
			COMPONENT libraries
			FILES_MATCHING
			PATTERN "include/include/*" EXCLUDE
			PATTERN "apr*.h")
endif(WIN32)

# Set up our program
add_executable(${THIS_TARGET}
               m+mSendToMQOutputServiceMain.cpp
               m+mSendToMQOutputInputHandler.cpp
               m+mSendToMQOutputService.cpp
               ${VERS_RESOURCE})

if(APPLE)
	add_library(activemq SHARED IMPORTED)
    set_property(TARGET activemq PROPERTY IMPORTED_LOCATION
                    "${CMAKE_INSTALL_PREFIX}/lib/libactivemq-cpp.dylib")

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

if(LINUX)
	add_library(activemq SHARED IMPORTED)
    set_property(TARGET activemq PROPERTY IMPORTED_LOCATION
                    "${CMAKE_INSTALL_PREFIX}/lib/libactivemq-cpp.so")
endif(LINUX)

# Note that the order of inclusion of libraries is critical in Linux, as they appear to only be
# processed once.
target_link_libraries(${THIS_TARGET} activemq ${MpM_LINK_LIBRARIES})

if(WIN32)
    target_link_libraries(${THIS_TARGET}
                            aprlib
                            ${BONJOUR_LIB})
endif(WIN32)

install(TARGETS ${THIS_TARGET}
        DESTINATION bin
        COMPONENT applications)

enable_testing()
