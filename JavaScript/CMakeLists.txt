#--------------------------------------------------------------------------------------------------
#
#  File:       JavaScript/CMakeLists.txt
#
#  Project:    m+m
#
#  Contains:   The CMAKE definitions for the JavaScript filter service application.
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

set(THIS_TARGET m+mJavaScriptFilterService)

if(WIN32)
    set(VERS_RESOURCE ${THIS_TARGET}.rc)
else()
    set(VERS_RESOURCE "")
endif()

configure_file(${THIS_TARGET}.rc.in ${THIS_TARGET}.rc)

set(SPIDER_VERSION "49a1")
if(WIN32)
    set(ICU_VERSION "56")
else()
    set(ICU_VERSION "561")
endif()

if(WIN32)
    if(WIN64)
        set(SPIDER_PATH "C:/Program Files/m+m/spidermonkey")
    else()
        set(SPIDER_PATH "C:/Program Files (x86)/m+m/spidermonkey")
    endif()
endif()
if(APPLE)
    set(SPIDER_PATH "${CMAKE_INSTALL_PREFIX}/spidermonkey")
endif()
if(LINUX)
    set(SPIDER_PATH "${CMAKE_INSTALL_PREFIX}/spidermonkey")
endif()

if(APPLE)
# There's a problem with building V8 / SpiderMonkey as a Universal - it acts as if we're building
# for Solaris and adds a '-KPIC' flag?!?!?
# So, we need to temporarily build for 64-bit only...
    set(OLD_CMAKE_OSX_ARCHITECTURES "${CMAKE_OSX_ARCHITECTURES}")
    set(CMAKE_OSX_DEPLOYMENT_TARGET "10.7" CACHE STRING "Deployment target for Mac OSX" FORCE)
    set(CMAKE_OSX_ARCHITECTURES "x86_64" CACHE STRING "Build architectures for Mac OS X" FORCE)
    set(OLD_CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_RPATH}")
    set(CMAKE_INSTALL_RPATH "${OLD_CMAKE_INSTALL_RPATH};${SPIDER_PATH}/lib" CACHE STRING
        "Fixes rpath" FORCE)
endif()
if(LINUX)
    set(OLD_CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_RPATH}")
    set(CMAKE_INSTALL_RPATH "${OLD_CMAKE_INSTALL_RPATH};${SPIDER_PATH}/lib" CACHE STRING
        "Fixes rpath" FORCE)
endif()

add_library(JS SHARED IMPORTED)

include_directories("${SPIDER_PATH}/include/mozjs-${SPIDER_VERSION}")

if(WIN32)
    set_property(TARGET JS PROPERTY IMPORTED_LOCATION
                "${SPIDER_PATH}/lib/mozjs-${SPIDER_VERSION}.dll")
    set_property(TARGET JS PROPERTY IMPORTED_IMPLIB
                "${SPIDER_PATH}/lib/mozjs-${SPIDER_VERSION}.lib")
    install(FILES "${SPIDER_PATH}/lib/mozjs-${SPIDER_VERSION}.dll"
            DESTINATION bin
            COMPONENT applications)
    install(FILES
            "${SPIDER_PATH}/lib/mozglue.dll"
            "${SPIDER_PATH}/lib/nspr4.dll"
            "${SPIDER_PATH}/lib/plds4.dll"
            "${SPIDER_PATH}/lib/plc4.dll"
            DESTINATION bin
            COMPONENT applications)
#    if(WIN64)
#        install(FILES
#                "${SPIDER_PATH}/lib/icudt${ICU_VERSION}.dll"
#                "${SPIDER_PATH}/lib/icuin${ICU_VERSION}.dll"
#                "${SPIDER_PATH}/lib/icuuc${ICU_VERSION}.dll"
#                DESTINATION bin
#                COMPONENT applications)
#    endif()
endif()
if(APPLE)
    set_property(TARGET JS PROPERTY IMPORTED_LOCATION
                "${SPIDER_PATH}/lib/libmozjs-${SPIDER_VERSION}.dylib")
endif()
if(LINUX)
    set_property(TARGET JS PROPERTY IMPORTED_LOCATION
                "${SPIDER_PATH}/lib/libmozjs-${SPIDER_VERSION}.so")
endif()

# Set up our program
add_executable(${THIS_TARGET}
               m+mJavaScriptFilterServiceMain.cpp
               m+mJavaScriptFilterInputHandler.cpp
               m+mJavaScriptFilterService.cpp
               m+mJavaScriptFilterThread.cpp
               ${VERS_RESOURCE})

# Note that the order of inclusion of libraries is critical in Linux, as they appear to only be
# processed once.
target_link_libraries(${THIS_TARGET} JS ${MpM_LINK_LIBRARIES})

fix_dynamic_libs(${THIS_TARGET})
if(APPLE)
    add_custom_command(TARGET ${THIS_TARGET} POST_BUILD COMMAND install_name_tool -change
                        "${E_PATH}/libmozjs-${SPIDER_VERSION}.dylib"
                        "@rpath/libmozjs-${SPIDER_VERSION}.dylib" ${THIS_TARGET})
    add_custom_command(TARGET ${THIS_TARGET} POST_BUILD COMMAND install_name_tool -change
                        "${E_PATH}/libmozglue.dylib" "@rpath/libmozglue.dylib" ${THIS_TARGET})

endif()

install(TARGETS ${THIS_TARGET}
        DESTINATION bin)

if(APPLE)
# Restore the architecture settings so that later builds will be correct.
    set(CMAKE_OSX_ARCHITECTURES "${OLD_CMAKE_OSX_ARCHITECTURES}" CACHE STRING
        "Build architectures for Mac OS X" FORCE)
    set(CMAKE_INSTALL_RPATH "${OLD_CMAKE_INSTALL_RPATH}" CACHE STRING "Fixes rpath" FORCE)
endif()
if(LINUX)
    # Restore the architecture settings so that later builds will be correct.
    set(CMAKE_INSTALL_RPATH "${OLD_CMAKE_INSTALL_RPATH}" CACHE STRING "Fixes rpath" FORCE)
endif()

enable_testing()
