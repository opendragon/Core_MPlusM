#--------------------------------------------------------------------------------------------------
#
#  File:       KinectV2InputService/CMakeLists.txt
#
#  Project:    m+m
#
#  Contains:   The CMAKE definitions for the Kinect V2 input service application.
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

set(THIS_TARGET m+mKinectV2InputService)

if(WIN32)
    set(VERS_RESOURCE ${THIS_TARGET}.rc)
else(WIN32)
    set(VERS_RESOURCE "")
endif(WIN32)

configure_file(${THIS_TARGET}.rc.in ${THIS_TARGET}.rc)

# Set up our program
add_executable(${THIS_TARGET}
               m+mKinectV2InputServiceMain.cpp
               m+mKinectV2EventThread.cpp
               m+mKinectV2InputService.cpp
               ${VERS_RESOURCE})

if(WIN32)
	include_directories(${MpMKINECTV2_SOURCE_DIR}/KinectV2InputService/KinectLib/inc)
	if(WIN64)
		set(MpMKINECT_LIB_DIR "${MpMKINECTV2_SOURCE_DIR}/KinectV2InputService/KinectLib/Lib/x64")
	else(WIN64)
		set(MpMKINECT_LIB_DIR "${MpMKINECTV2_SOURCE_DIR}/KinectV2InputService/KinectLib/Lib/x86")
	endif(WIN64)
	add_library(Kinect SHARED IMPORTED)
	set_property(TARGET Kinect PROPERTY IMPORTED_IMPLIB "${MpMKINECT_LIB_DIR}/Kinect20.lib")
	set_property(TARGET Kinect PROPERTY IMPORTED_LOCATION "${MpMKINECT_LIB_DIR}/Kinect20.dll")
	add_custom_command(TARGET ${THIS_TARGET} POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy_directory
                           "${MpMKINECT_LIB_DIR}" $<TARGET_FILE_DIR:m+mKinectV2InputService>)
	install(FILES "${MpMKINECT_LIB_DIR}/Kinect20.dll"
			DESTINATION bin
			COMPONENT applications)
endif(WIN32)

# Note that the order of inclusion of libraries is critical in Linux, as they appear to only be
# processed once.
if(MpM_BuildDummyServices)
    target_link_libraries(${THIS_TARGET} ${MpM_LINK_LIBRARIES})
else(MpM_BuildDummyServices)
    target_link_libraries(${THIS_TARGET} Kinect ${MpM_LINK_LIBRARIES})
endif(MpM_BuildDummyServices)

if(APPLE)
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

install(TARGETS ${THIS_TARGET}
		DESTINATION bin
		COMPONENT applications)

set(THIS_TARGET m+mKinectV2BlobInputService)

if(WIN32)
    set(VERS_RESOURCE ${THIS_TARGET}.rc)
else(WIN32)
    set(VERS_RESOURCE "")
endif(WIN32)

configure_file(${THIS_TARGET}.rc.in ${THIS_TARGET}.rc)

# Set up our program
add_executable(${THIS_TARGET}
                m+mKinectV2BlobInputServiceMain.cpp
                m+mKinectV2BlobEventThread.cpp
                m+mKinectV2BlobInputService.cpp
                ${VERS_RESOURCE})

# Note that the order of inclusion of libraries is critical in Linux, as they appear to only be
# processed once.
if(MpM_BuildDummyServices)
    target_link_libraries(${THIS_TARGET} ${MpM_LINK_LIBRARIES})
else(MpM_BuildDummyServices)
    target_link_libraries(${THIS_TARGET} Kinect ${MpM_LINK_LIBRARIES})
endif(MpM_BuildDummyServices)

if(APPLE)
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

install(TARGETS ${THIS_TARGET}
        DESTINATION bin
        COMPONENT applications)

set(THIS_TARGET m+mKinectV2SpecialInputService)

if(WIN32)
    set(VERS_RESOURCE ${THIS_TARGET}.rc)
else(WIN32)
    set(VERS_RESOURCE "")
endif(WIN32)

configure_file(${THIS_TARGET}.rc.in ${THIS_TARGET}.rc)

# Set up our program
add_executable(${THIS_TARGET}
                m+mKinectV2SpecialInputServiceMain.cpp
                m+mKinectV2SpecialEventThread.cpp
                m+mKinectV2SpecialInputService.cpp
                ${VERS_RESOURCE})

# Note that the order of inclusion of libraries is critical in Linux, as they appear to only be
# processed once.
if(MpM_BuildDummyServices)
    target_link_libraries(${THIS_TARGET} ${MpM_LINK_LIBRARIES})
else(MpM_BuildDummyServices)
    target_link_libraries(${THIS_TARGET} Kinect ${MpM_LINK_LIBRARIES})
endif(MpM_BuildDummyServices)

if(APPLE)
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

install(TARGETS ${THIS_TARGET}
        DESTINATION bin
        COMPONENT applications)

enable_testing()
