#--------------------------------------------------------------------------------------------------
#
#  File:       JavaScript/CMakeLists.txt
#
#  Project:    M+M
#
#  Contains:   The CMAKE definitions for the JavaScript input / output service application.
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
#  Created:    2015-01-05
#
#--------------------------------------------------------------------------------------------------

set(THIS_TARGET mpmJavaScriptService)

if(APPLE)
# There's a problem with building V8 / SpiderMonkey as a Universal - it acts as if we're building
# for Solaris and adds a '-KPIC' flag?!?!?
# So, we need to temporarily build for 64-bit only...
    set(OLD_CMAKE_OSX_ARCHITECTURES "${CMAKE_OSX_ARCHITECTURES}")
    set(CMAKE_OSX_ARCHITECTURES "x86_64" CACHE STRING "Build architectures for Mac OS X" FORCE)
    set(OLD_CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_RPATH}")
    set(CMAKE_INSTALL_RPATH "${OLD_CMAKE_INSTALL_RPATH};/opt/M+M/spidermonkey/lib" CACHE STRING
        "Fixes rpath" FORCE)
endif(APPLE)

if(WIN32)
    add_library(JS SHARED IMPORTED)
    include_directories("C:/Program Files (x86)/M+M/spidermonkey/include/mozjs-39a1")
    set_property(TARGET JS PROPERTY IMPORTED_LOCATION
                    "C:/Program Files (x86)/M+M/spidermonkey/lib/mozjs-39a1.dll")
    set(SPIDERMONKEY "JS")
endif(WIN32)
if(APPLE)
    add_library(JS SHARED IMPORTED)
    include_directories("/opt/M+M/spidermonkey/include/mozjs-39a1")
    set_property(TARGET JS PROPERTY IMPORTED_LOCATION
                    "/opt/M+M/spidermonkey/lib/libmozjs-39a1.dylib")
    set(SPIDERMONKEY "JS")
endif(APPLE)

# Set up our program
add_executable(${THIS_TARGET}
               M+MJavaScriptServiceMain.cpp
               M+MJavaScriptInputHandler.cpp
               M+MJavaScriptService.cpp
               M+MJavaScriptThread.cpp)

# Note that the order of inclusion of libraries is critical in Linux, as they appear to only be
# processed once.
target_link_libraries(${THIS_TARGET}
                      mpmCommon
                      odLogging
                      ${SPIDERMONKEY}
                      ${YARP_LIBRARIES}
                      ${ACE_LIBRARIES}
                      ${BONJOUR_LIB})

if(APPLE)
    add_custom_command(TARGET ${THIS_TARGET} POST_BUILD COMMAND install_name_tool -change
                        "@executable_path/libmozjs-39a1.dylib" "@rpath/libmozjs-39a1.dylib"
                        ${THIS_TARGET})
    add_custom_command(TARGET ${THIS_TARGET} POST_BUILD COMMAND install_name_tool -change
                        "@executable_path/libmozglue.dylib" "@rpath/libmozglue.dylib"
                        ${THIS_TARGET})

    add_custom_command(TARGET ${THIS_TARGET} POST_BUILD COMMAND install_name_tool -change
                        libACE.dylib "@rpath/libACE.dylib" ${THIS_TARGET})

    add_custom_command(TARGET ${THIS_TARGET} POST_BUILD COMMAND install_name_tool -change
                        libmpmCommon.dylib "@rpath/libmpmCommon.dylib" ${THIS_TARGET})

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
    set(CMAKE_INSTALL_RPATH "${OLD_CMAKE_INSTALL_RPATH}" CACHE STRING "Fixes rpath" FORCE)
endif(APPLE)

enable_testing()
