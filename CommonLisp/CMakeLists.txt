#--------------------------------------------------------------------------------------------------
#
#  File:       CommonLisp/CMakeLists.txt
#
#  Project:    m+m
#
#  Contains:   The CMAKE definitions for the CommonLisp filter service application.
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
#  Created:    2015-08-05
#
#--------------------------------------------------------------------------------------------------

set(THIS_TARGET m+mCommonLispFilterService)

if(WIN32)
    set(VERS_RESOURCE ${THIS_TARGET}.rc)
else(WIN32)
    set(VERS_RESOURCE "")
endif(WIN32)

configure_file(${THIS_TARGET}.rc.in ${THIS_TARGET}.rc)

if(WIN32)
    if(WIN64)
        set(ECL_PATH "C:/Program Files/m+m/ecl_src")
    else(WIN64)
        set(ECL_PATH "C:/Program Files (x86)/m+m/ecl_src")
    endif(WIN64)
endif(WIN32)

if(APPLE)
# There's a problem with building ECL as a Universal - it can't be configured correctly
# So, we need to temporarily build for 64-bit only...
    set(OLD_CMAKE_OSX_ARCHITECTURES "${CMAKE_OSX_ARCHITECTURES}")
    set(CMAKE_OSX_ARCHITECTURES "x86_64" CACHE STRING "Build architectures for Mac OS X" FORCE)
endif(APPLE)

add_library(ECL SHARED IMPORTED)

if(WIN32)
	include_directories("${ECL_PATH}")
	set_property(TARGET ECL PROPERTY IMPORTED_LOCATION "${ECL_PATH}/ecl.dll")
	set_property(TARGET ECL PROPERTY IMPORTED_IMPLIB "${ECL_PATH}/ecl.lib")
	install(FILES
                "${ECL_PATH}/ecl.dll"
                DESTINATION bin
                COMPONENT applications)
	install(DIRECTORY
                "${ECL_PATH}/ecl"
                DESTINATION ecl
                COMPONENT libraries)
endif(WIN32)
if(APPLE)
    set_property(TARGET ECL PROPERTY IMPORTED_LOCATION "${CMAKE_INSTALL_PREFIX}/lib/libecl.dylib")
endif(APPLE)
if(LINUX)
    set_property(TARGET ECL PROPERTY IMPORTED_LOCATION "${CMAKE_INSTALL_PREFIX}/lib/libecl.so")
endif(LINUX)

# Set up our program
add_executable(${THIS_TARGET}
               m+mCommonLispFilterServiceMain.cpp
               m+mCommonLispFilterInputHandler.cpp
               m+mCommonLispFilterService.cpp
               m+mCommonLispFilterThread.cpp
               ${VERS_RESOURCE})

# Note that the order of inclusion of libraries is critical in Linux, as they appear to only be
# processed once.
target_link_libraries(${THIS_TARGET} ECL ${MpM_LINK_LIBRARIES})

if(APPLE)
    add_custom_command(TARGET ${THIS_TARGET} POST_BUILD COMMAND install_name_tool -change
                        "${E_PATH}/libecl.dylib" "@rpath/libecl.dylib" ${THIS_TARGET})

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
        DESTINATION bin)

if(APPLE)
# Restore the architecture settings so that later builds will be correct.
    set(CMAKE_OSX_ARCHITECTURES "${OLD_CMAKE_OSX_ARCHITECTURES}" CACHE STRING
        "Build architectures for Mac OS X" FORCE)
endif(APPLE)

enable_testing()
