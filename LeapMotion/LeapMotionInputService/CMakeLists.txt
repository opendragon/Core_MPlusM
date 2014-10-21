#--------------------------------------------------------------------------------------------------
#
#  File:       LeapMotionInputService/CMakeLists.txt
#
#  Project:    M+M
#
#  Contains:   The CMAKE definitions for the LEAP input service application.
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
#  Created:    2014-09-16
#
#--------------------------------------------------------------------------------------------------

# Set up our program
add_executable(mpmLeapMotionInputService
               M+MLeapMotionInputServiceMain.cpp
               M+MLeapMotionInputListener.cpp
               M+MLeapMotionInputService.cpp)

# Note that the order of inclusion of libraries is critical in Linux, as they appear to only be
# processed once.

target_link_libraries(mpmLeapMotionInputService
                      mpmCommon
                      odLogging
                      ${YARP_LIBRARIES})

if(WIN32)
	add_library(Leap SHARED IMPORTED)
	set_property(TARGET Leap PROPERTY IMPORTED_LOCATION "${MpMLEAPMOTION_SOURCE_DIR}/LeapMotionInputService/win_lib/Leap.dll")
	set_property(TARGET Leap PROPERTY IMPORTED_IMPLIB "${MpMLEAPMOTION_SOURCE_DIR}/LeapMotionInputService/win_lib/Leap.lib")
	target_link_libraries(mpmLeapMotionInputService
                          ${BONJOUR_LIB}
                          Leap)
	add_custom_command(TARGET mpmLeapMotionInputService POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy_directory
                        "${MpMLEAPMOTION_SOURCE_DIR}/LeapMotionInputService/win_lib"
                        $<TARGET_FILE_DIR:mpmLeapMotionInputService>)
endif(WIN32)
if(APPLE)
    add_library(Leap SHARED IMPORTED)
    set_property(TARGET Leap PROPERTY IMPORTED_LOCATION "${MpMLEAPMOTION_SOURCE_DIR}/LeapMotionInputService/libLeap.dylib")
    target_link_libraries(mpmLeapMotionInputService
                            Leap)
    install(FILES "${MpMLEAPMOTION_SOURCE_DIR}/LeapMotionInputService/libLeap.dylib" DESTINATION lib)
endif(APPLE)


install(TARGETS mpmLeapMotionInputService
		DESTINATION bin
		COMPONENT applications)
install(FILES "${MpMLEAPMOTION_SOURCE_DIR}/LeapMotionInputService/win_lib/Leap.dll" 
				DESTINATION bin
				COMPONENT applications)

enable_testing()
