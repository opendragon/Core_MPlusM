#--------------------------------------------------------------------------------------------------
#
#  File:       LeapMotionInputService/CMakeLists.txt
#
#  Project:    m+m
#
#  Contains:   The CMAKE definitions for the Leap Motion input service application.
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
#  Created:    2014-09-16
#
#--------------------------------------------------------------------------------------------------

set(THIS_TARGET m+mLeapMotionInputService)

if(WIN32)
    set(VERS_RESOURCE ${THIS_TARGET}.rc)
else()
    set(VERS_RESOURCE "")
endif()

configure_file(${THIS_TARGET}.rc.in ${THIS_TARGET}.rc)

# Set up our program
add_executable(${THIS_TARGET}
               m+mLeapMotionInputServiceMain.cpp
               m+mLeapMotionInputListener.cpp
               m+mLeapMotionInputService.cpp
               ${VERS_RESOURCE})

add_library(Leap SHARED IMPORTED)

if(WIN32)
    if(WIN64)
        set(MpMLEAPMOTION_LIB_DIR "${MpMLEAPMOTION_SOURCE_DIR}/LeapMotionInputService/win_lib/x64")
    else()
        set(MpMLEAPMOTION_LIB_DIR "${MpMLEAPMOTION_SOURCE_DIR}/LeapMotionInputService/win_lib/x86")
    endif()
    set_property(TARGET Leap PROPERTY IMPORTED_LOCATION "${MpMLEAPMOTION_LIB_DIR}/Leap.dll")
    set_property(TARGET Leap PROPERTY IMPORTED_IMPLIB "${MpMLEAPMOTION_LIB_DIR}/Leap.lib")
    add_custom_command(TARGET ${THIS_TARGET} POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy_directory
                       "${MpMLEAPMOTION_LIB_DIR}" $<TARGET_FILE_DIR:m+mLeapMotionInputService>)
    install(FILES "${MpMLEAPMOTION_LIB_DIR}/Leap.dll"
            DESTINATION bin
            COMPONENT applications)
endif()
fix_dynamic_libs(${THIS_TARGET})
if(APPLE)
    set_property(TARGET Leap PROPERTY IMPORTED_LOCATION
                "${MpMLEAPMOTION_SOURCE_DIR}/LeapMotionInputService/libLeap.dylib")

    add_custom_command(TARGET ${THIS_TARGET} POST_BUILD COMMAND install_name_tool -change
                        "@loader_path/libLeap.dylib" "@rpath/libLeap.dylib" ${THIS_TARGET})

    install(FILES "${MpMLEAPMOTION_SOURCE_DIR}/LeapMotionInputService/libLeap.dylib"
            DESTINATION ${LIB_DEST}
            COMPONENT libraries)
endif()
if(LINUX)
    if(CPU_64)
        set(MpMLEAPMOTION_LIB_DIR "${MpMLEAPMOTION_SOURCE_DIR}/LeapMotionInputService/x64")
    else()
        set(MpMLEAPMOTION_LIB_DIR "${MpMLEAPMOTION_SOURCE_DIR}/LeapMotionInputService/x86")
    endif()
    set_property(TARGET Leap PROPERTY IMPORTED_LOCATION "${MpMLEAPMOTION_LIB_DIR}/libLeap.so")
    install(FILES "${MpMLEAPMOTION_LIB_DIR}/libLeap.so"
            DESTINATION ${LIB_DEST}
            COMPONENT libraries)
endif()

# Note that the order of inclusion of libraries is critical in Linux, as they appear to only be
# processed once.
target_link_libraries(${THIS_TARGET} Leap ${MpM_LINK_LIBRARIES})

install(TARGETS ${THIS_TARGET}
        DESTINATION bin
        COMPONENT applications)

set(THIS_TARGET m+mLeapTwoFingersInputService)

if(WIN32)
    set(VERS_RESOURCE ${THIS_TARGET}.rc)
else()
    set(VERS_RESOURCE "")
endif()

configure_file(${THIS_TARGET}.rc.in ${THIS_TARGET}.rc)

# Set up our program
add_executable(${THIS_TARGET}
                m+mLeapTwoFingersInputServiceMain.cpp
                m+mLeapTwoFingersInputListener.cpp
                m+mLeapTwoFingersInputService.cpp
                ${VERS_RESOURCE})

fix_dynamic_libs(${THIS_TARGET})
if(APPLE)
    add_custom_command(TARGET ${THIS_TARGET} POST_BUILD COMMAND install_name_tool -change
                        "@loader_path/libLeap.dylib" "@rpath/libLeap.dylib" ${THIS_TARGET})

endif()

# Note that the order of inclusion of libraries is critical in Linux, as they appear to only be
# processed once.
target_link_libraries(${THIS_TARGET} Leap ${MpM_LINK_LIBRARIES})

install(TARGETS ${THIS_TARGET}
        DESTINATION bin
        COMPONENT applications)

set(THIS_TARGET m+mLeapBlobInputService)

if(WIN32)
    set(VERS_RESOURCE ${THIS_TARGET}.rc)
else()
    set(VERS_RESOURCE "")
endif()

configure_file(${THIS_TARGET}.rc.in ${THIS_TARGET}.rc)

# Set up our program
add_executable(${THIS_TARGET}
                m+mLeapBlobInputServiceMain.cpp
                m+mLeapBlobInputListener.cpp
                m+mLeapBlobInputService.cpp
                ${VERS_RESOURCE})

fix_dynamic_libs(${THIS_TARGET})
if(APPLE)
    add_custom_command(TARGET ${THIS_TARGET} POST_BUILD COMMAND install_name_tool -change
                       "@loader_path/libLeap.dylib" "@rpath/libLeap.dylib" ${THIS_TARGET})

endif()

# Note that the order of inclusion of libraries is critical in Linux, as they appear to only be
# processed once.
target_link_libraries(${THIS_TARGET} Leap ${MpM_LINK_LIBRARIES})

install(TARGETS ${THIS_TARGET}
        DESTINATION bin
        COMPONENT applications)

set(THIS_TARGET m+mLeapTwoPalmsInputService)

if(WIN32)
    set(VERS_RESOURCE ${THIS_TARGET}.rc)
else()
    set(VERS_RESOURCE "")
endif()

configure_file(${THIS_TARGET}.rc.in ${THIS_TARGET}.rc)

# Set up our program
add_executable(${THIS_TARGET}
                m+mLeapTwoPalmsInputServiceMain.cpp
                m+mLeapTwoPalmsInputListener.cpp
                m+mLeapTwoPalmsInputService.cpp
                ${VERS_RESOURCE})

fix_dynamic_libs(${THIS_TARGET})
if(APPLE)
    add_custom_command(TARGET ${THIS_TARGET} POST_BUILD COMMAND install_name_tool -change
                        "@loader_path/libLeap.dylib" "@rpath/libLeap.dylib" ${THIS_TARGET})

endif()

# Note that the order of inclusion of libraries is critical in Linux, as they appear to only be
# processed once.
target_link_libraries(${THIS_TARGET} Leap ${MpM_LINK_LIBRARIES})

install(TARGETS ${THIS_TARGET}
        DESTINATION bin
        COMPONENT applications)

set(THIS_TARGET m+mLeapFingerTipsInputService)

if(WIN32)
    set(VERS_RESOURCE ${THIS_TARGET}.rc)
else()
    set(VERS_RESOURCE "")
endif()

configure_file(${THIS_TARGET}.rc.in ${THIS_TARGET}.rc)

# Set up our program
add_executable(${THIS_TARGET}
               m+mLeapFingerTipsInputServiceMain.cpp
               m+mLeapFingerTipsInputListener.cpp
               m+mLeapFingerTipsInputService.cpp
               ${VERS_RESOURCE})

fix_dynamic_libs(${THIS_TARGET})
if(APPLE)
    add_custom_command(TARGET ${THIS_TARGET} POST_BUILD COMMAND install_name_tool -change
                        "@loader_path/libLeap.dylib" "@rpath/libLeap.dylib" ${THIS_TARGET})

endif()

# Note that the order of inclusion of libraries is critical in Linux, as they appear to only be
# processed once.
target_link_libraries(${THIS_TARGET} Leap ${MpM_LINK_LIBRARIES})

install(TARGETS ${THIS_TARGET}
        DESTINATION bin
        COMPONENT applications)

enable_testing()
