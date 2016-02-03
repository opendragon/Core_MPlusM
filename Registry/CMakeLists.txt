#--------------------------------------------------------------------------------------------------
#
#  File:       Registry/CMakeLists.txt
#
#  Project:    m+m
#
#  Contains:   The CMAKE definitions for the RegistryService application.
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
#  Created:    2014-03-14
#
#--------------------------------------------------------------------------------------------------

add_subdirectory(RegistryTests)

set(THIS_TARGET ${MpM_REGISTRY_EXECUTABLE_NAME})

if(WIN32)
    set(VERS_RESOURCE ${THIS_TARGET}.rc)
else(WIN32)
    set(VERS_RESOURCE "")
endif(WIN32)

configure_file(${THIS_TARGET}.rc.in ${THIS_TARGET}.rc)

include_directories("${MpM_SOURCE_DIR}"
                    "${MpM_SQLITE_DIR}")

# Set up our program
add_executable(${THIS_TARGET}
               m+mRegistryServiceMain.cpp
               m+mColumnNameValidator.cpp
               m+mMatchRequestHandler.cpp
               m+mNameServerReportingThread.cpp
               m+mPingRequestHandler.cpp
               m+mRegisterRequestHandler.cpp
               m+mRegistryCheckThread.cpp
               m+mRegistryService.cpp
               m+mUnregisterRequestHandler.cpp
               ${MpM_SQLITE_DIR}/sqlite3.c
               ${VERS_RESOURCE})

# Note that the order of inclusion of libraries is critical in Linux, as they appear to only be
# processed once.
target_link_libraries(${THIS_TARGET} ${MpM_LINK_LIBRARIES})

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

set(THIS_TARGET m+mRegistryTest)

if(WIN32)
    set(VERS_RESOURCE ${THIS_TARGET}.rc)
else(WIN32)
    set(VERS_RESOURCE "")
endif(WIN32)

configure_file(${THIS_TARGET}.rc.in ${THIS_TARGET}.rc)

add_executable(${THIS_TARGET}
               m+mRegistryTest.cpp
               m+mColumnNameValidator.cpp
               m+mMatchRequestHandler.cpp
               m+mPingRequestHandler.cpp
               m+mRegisterRequestHandler.cpp
               m+mRegistryCheckThread.cpp
               m+mRegistryService.cpp
               m+mUnregisterRequestHandler.cpp
               ${MpM_SQLITE_DIR}/sqlite3.c
               ${VERS_RESOURCE})

# Note that the order of inclusion of libraries is critical in Linux, as they appear to only be
# processed once.
target_link_libraries(${THIS_TARGET} m+mRegistryTestSupport ${MpM_LINK_LIBRARIES})

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

# Test create register service
add_test(NAME TestCreateRegistryService1 COMMAND ${THIS_TARGET} 13)
add_test(NAME TestCreateRegistryService2 COMMAND ${THIS_TARGET} 13 "12350")
# Test register request, second service
add_test(NAME TestRequestRegistryService1 COMMAND ${THIS_TARGET} 14)
add_test(NAME TestRequestRegistryService2 COMMAND ${THIS_TARGET} 14 "12351")
# Test unregister request, second service
add_test(NAME TestRequestUnregisterService1 COMMAND ${THIS_TARGET} 15)
add_test(NAME TestRequestUnregisterService2 COMMAND ${THIS_TARGET} 15 "12352")
# Test search for request / service - note the change in arguments
add_test(NAME TestRequestSearchService1 COMMAND ${THIS_TARGET} 16 0 ""
            "FAILED \"An error message\"")
add_test(NAME TestRequestSearchService2 COMMAND ${THIS_TARGET} 16 0 "Version: 42" "OK ()")
add_test(NAME TestRequestSearchService3 COMMAND ${THIS_TARGET} 16 0 "Details:Echo*"
            "OK (/_service_/test/requestsearchservice)")
add_test(NAME TestRequestSearchService4 COMMAND ${THIS_TARGET} 16 0
            "Details:Echo*,Keyword:(requests)" "OK (/_service_/test/requestsearchservice /$ervice)")
add_test(NAME TestRequestSearchService5 COMMAND ${THIS_TARGET} 16 0 "Description:*unit\\ tests"
            "OK (/_service_/test/requestsearchservice)")
add_test(NAME TestRequestSearchService6 COMMAND ${THIS_TARGET} 16 1 "Details:Echo*" "OK (Test16)")
add_test(NAME TestRequestSearchService7 COMMAND ${THIS_TARGET} 16 1
            "Details:Echo*,Keyword:(requests)" "OK (Registry Test16)")
add_test(NAME TestRequestSearchService8 COMMAND ${THIS_TARGET} 16 1 "Description:*unit\\ tests"
            "OK (Test16)")
