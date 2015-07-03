#--------------------------------------------------------------------------------------------------
#
#  File:       CommonTests/CMakeLists.txt
#
#  Project:    m+m
#
#  Contains:   The CMAKE definitions for the common unit test application.
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
#  Created:    2014-02-28
#
#--------------------------------------------------------------------------------------------------

include_directories("${MpM_SOURCE_DIR}")

set(THIS_TARGET m+mCommonTest)

if(WIN32)
    set(VERS_RESOURCE ${THIS_TARGET}.rc)
else(WIN32)
    set(VERS_RESOURCE "")
endif(WIN32)

configure_file(${THIS_TARGET}.rc.in ${THIS_TARGET}.rc)

add_library(m+mCommonTestSupport
            m+mTest03Handler.cpp
            m+mTest04Handler.cpp
            m+mTest05Handler.cpp
            m+mTest05HandlerCreator.cpp
            m+mTest08Handler.cpp
            m+mTest09DefaultRequestHandler.cpp
            m+mTest09Service.cpp
            m+mTest10DefaultRequestHandler.cpp
            m+mTest10Service.cpp
            m+mTest11EchoRequestHandler.cpp
            m+mTest11Service.cpp
            m+mTest12EchoRequestHandler.cpp
            m+mTest12Service.cpp)

enable_testing()

add_executable(${THIS_TARGET}
               m+mCommonTest.cpp
               ${VERS_RESOURCE})

# Note that the order of inclusion of libraries is critical in Linux, as they appear to only be
# processed once.
target_link_libraries(${THIS_TARGET}
                      m+mCommonTestSupport
                      m+mCommon
                      odLogging
                      ${YARP_LIBRARIES}
                      ${ACE_LIBRARIES}
                      ${BONJOUR_LIB})

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

# Argument order for test 1 = endpoint name [, IP address / name [, port]]
add_test(NAME TestCreateEndpoint1 COMMAND ${THIS_TARGET} 1 "/service/test/createendpoint_1")
add_test(NAME TestCreateEndpoint2 COMMAND ${THIS_TARGET} 1 "/service/test/createendpoint_2" "12340")
# Test connection to endpoints, same arguments as test 1
add_test(NAME TestConnectToEndpoint1 COMMAND ${THIS_TARGET} 2 "/service/test/connecttoendpoint_1")
add_test(NAME TestConnectToEndpoint2 COMMAND ${THIS_TARGET} 2 "/service/test/connecttoendpoint_2"
            "12341")
# Test data write to endpoints, same arguments as test 1
add_test(NAME TestWriteToEndpoint1 COMMAND ${THIS_TARGET} 3 "/service/test/writetoendpoint_1")
add_test(NAME TestWriteToEndpoint2 COMMAND ${THIS_TARGET} 3 "/service/test/writetoendpoint_2"
            "12342")
# Test data echo from endpoints, same arguments as test 1
add_test(NAME TestEchoFromEndpointWithReader1 COMMAND ${THIS_TARGET} 4
            "/service/test/echofromendpointwithreader_1")
add_test(NAME TestEchoFromEndpointWithReader2 COMMAND ${THIS_TARGET} 4
            "/service/test/echofromendpointwithreader_2" "12343")
# Test data echo from endpoints, using reader creator, same arguments as test 1
add_test(NAME TestEchoFromEndpointWithReaderCreator1 COMMAND ${THIS_TARGET} 5
            "/service/test/echofromendpointwithreadercreator_1")
add_test(NAME TestEchoFromEndpointWithReaderCreator2 COMMAND ${THIS_TARGET} 5
            "/service/test/echofromendpointwithreadercreator_2" "12344")
# Test service request creation
add_test(NAME TestCreateRequest1 COMMAND ${THIS_TARGET} 6 "echo")
add_test(NAME TestCreateRequest2 COMMAND ${THIS_TARGET} 6 "echo" "something")
# Test service response creation
add_test(NAME TestCreateResponse1 COMMAND ${THIS_TARGET} 7)
add_test(NAME TestCreateResponse2 COMMAND ${THIS_TARGET} 7 "back")
add_test(NAME TestCreateResponse3 COMMAND ${THIS_TARGET} 7 "back" "at" "you")
# Test echo via request/response, fixed message [echo something to send] but arguments as with test
# 1
add_test(NAME TestRequestEchoFromEndpoint1 COMMAND ${THIS_TARGET} 8
            "/service/test/requestechofromendpoint_1")
add_test(NAME TestRequestEchoFromEndpoint2 COMMAND ${THIS_TARGET} 8
            "/service/test/requestechofromendpoint_2" "12345")
# Test echo via service request/response, fixed message [echo something to send] but arguments as
# with test 1
add_test(NAME TestRequestEchoFromServiceUsingDefaultWithReader1 COMMAND ${THIS_TARGET} 9
            "/service/test/requestechofromserviceusingdefaultwithreader_1")
add_test(NAME TestRequestEchoFromServiceUsingDefaultWithReader2 COMMAND ${THIS_TARGET} 9
            "/service/test/requestechofromserviceusingdefaultwithreader_2" "12346")
# Test echo via service request/response, fixed message [echo something to send] but arguments as
# with test 1
add_test(NAME TestRequestEchoFromServiceUsingDefaultWithReaderCreator1 COMMAND ${THIS_TARGET} 10
            "/service/test/requestechofromserviceusingdefaultwithreadercreator_1")
add_test(NAME TestRequestEchoFromServiceUsingDefaultWithReaderCreator2 COMMAND ${THIS_TARGET} 10
            "/service/test/requestechofromserviceusingdefaultwithreadercreator_2" "12347")
# Test echo via service request/response, fixed message [echo something to send] but arguments as
# with test 1
add_test(NAME TestRequestEchoFromServiceWithRequestHandler1 COMMAND ${THIS_TARGET} 11
            "/service/test/requestechofromservicewithrequesthandler_1")
add_test(NAME TestRequestEchoFromServiceWithRequestHandler2 COMMAND ${THIS_TARGET} 11
            "/service/test/requestechofromservicewithrequesthandler_2" "12348")
# Test list request
add_test(NAME TestRequestEchoFromServiceWithRequestHandlerAndInfo1 COMMAND ${THIS_TARGET} 12
            "/service/test/requestechofromservicewithrequesthandlerandinfo_1")
add_test(NAME TestRequestEchoFromServiceWithRequestHandlerAndInfo2 COMMAND ${THIS_TARGET} 12
            "/service/test/requestechofromservicewithrequesthandlerandinfo_2" "12349")
