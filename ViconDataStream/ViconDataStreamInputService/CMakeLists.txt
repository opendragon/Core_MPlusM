#--------------------------------------------------------------------------------------------------
#
#  File:       ViconDataStreamInputService/CMakeLists.txt
#
#  Project:    M+M
#
#  Contains:   The CMAKE definitions for the Vicon DataStream input service application.
#
#  Written by: Norman Jaffe
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
#  Created:    2014-11-07
#
#--------------------------------------------------------------------------------------------------

set(THIS_TARGET mpmViconDataStreamInputService)

include_directories(${MpMVICONDATASTREAM_SOURCE_DIR}/ViconDataStreamInputService/ViconLib/inc)

# Set up our program
add_executable(${THIS_TARGET}
               M+MViconDataStreamInputServiceMain.cpp
               M+MViconDataStreamEventThread.cpp
               M+MViconDataStreamInputService.cpp)

# Note that the order of inclusion of libraries is critical in Linux, as they appear to only be
# processed once.
target_link_libraries(${THIS_TARGET}
                      mpmCommon
                      odLogging
                      ${YARP_LIBRARIES}
                      ${ACE_LIBRARIES})

if(WIN32)
	add_library(ViconDataStream SHARED IMPORTED)
	set_property(TARGET ViconDataStream PROPERTY IMPORTED_LOCATION
"${MpMVICONDATASTREAM_SOURCE_DIR}/ViconDataStreamInputService/ViconLib/Lib/ViconDataStreamSDK_CPP.dll")
	set_property(TARGET ViconDataStream PROPERTY IMPORTED_IMPLIB
"${MpMVICONDATASTREAM_SOURCE_DIR}/ViconDataStreamInputService/ViconLib/Lib/ViconDataStreamSDK_CPP.lib")
	target_link_libraries(${THIS_TARGET}
                          ${BONJOUR_LIB}
                          ViconDataStream)
	add_custom_command(TARGET ${THIS_TARGET} POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy_directory
                        "${MpMVICONDATASTREAM_SOURCE_DIR}/ViconDataStreamInputService/ViconLib/Lib"
                        $<TARGET_FILE_DIR:mpmViconDataStreamInputService>)
    install(FILES
"${MpMVICONDATASTREAM_SOURCE_DIR}/ViconDataStreamInputService/ViconLib/Lib/ViconDataStreamSDK_CPP.dll"
            DESTINATION bin
            COMPONENT applications)
endif(WIN32)

if(APPLE)
    add_custom_command(TARGET ${THIS_TARGET} POST_BUILD COMMAND install_name_tool -add_rpath
                        "${CMAKE_INSTALL_PREFIX}/lib" ${THIS_TARGET})

    add_custom_command(TARGET ${THIS_TARGET} POST_BUILD COMMAND install_name_tool -change
                        libACE.dylib "@rpath/libACE.dylib" ${THIS_TARGET})
    
    add_custom_command(TARGET ${THIS_TARGET} POST_BUILD COMMAND install_name_tool -change
                        "${MpM_SOURCE_DIR}/mpm/libmpmCommon.dylib" "@rpath/libmpmCommon.dylib"
                        ${THIS_TARGET})

    add_custom_command(TARGET ${THIS_TARGET} POST_BUILD COMMAND install_name_tool -change
                        "${MpM_SOURCE_DIR}/odl/libodLogging.dylib" "@rpath/libodLogging.dylib"
                        ${THIS_TARGET})

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
