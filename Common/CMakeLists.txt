#--------------------------------------------------------------------------------------
#
#  File:       Common/CMakeLists.txt
#
#  Project:    YarpPlusPlus
#
#  Contains:   The CMAKE definitions for the Yarp++ common libraries.
#
#  Written by: Norman Jaffe
#
#  Copyright:  (c) 2014 by HPlus Technologies Ltd. and Simon Fraser University.
#
#              All rights reserved. Redistribution and use in source and binary forms,
#              with or without modification, are permitted provided that the following
#              conditions are met:
#                * Redistributions of source code must retain the above copyright
#                  notice, this list of conditions and the following disclaimer.
#                * Redistributions in binary form must reproduce the above copyright
#                  notice, this list of conditions and the following disclaimer in the
#                  documentation and/or other materials provided with the
#                  distribution.
#                * Neither the name of the copyright holders nor the names of its
#                  contributors may be used to endorse or promote products derived
#                  from this software without specific prior written permission.
#
#              THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#              "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
#              LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
#              PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
#              OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
#              SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
#              LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
#              DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
#              THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
#              (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
#              OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
#  Created:    2014-02-06
#
#--------------------------------------------------------------------------------------

set(YPP_COMMONTESTSUPPORT_DIR "${YPP_COMMON_DIR}/CommonTests")

add_subdirectory(CommonTests)

include_directories("${YPP_LOGGING_DIR}"
                    “${YPP_PARSER_DIR}”
                    "${YPP_COMMONTESTSUPPORT_DIR}")

add_library(YPPCommon
            YPPBaseAdapterData.cpp
            YPPBaseClient.cpp
            YPPBaseContext.cpp
            YPPBaseRequestHandler.cpp
            YPPBaseService.cpp
            YPPBaseServiceInputHandler.cpp
            YPPBaseServiceInputHandlerCreator.cpp
            YPPCommon.cpp
            YPPEndpoint.cpp
            YPPException.cpp
            YPPInfoRequestHandler.cpp
            YPPInputHandler.cpp
            YPPInputHandlerCreator.cpp
            YPPListRequestHandler.cpp
            YPPNameRequestHandler.cpp
            YPPRequestMap.cpp
            YPPServiceRequest.cpp
            YPPServiceResponse.cpp)

install(TARGETS YPPCommon DESTINATION bin)
install(FILES YPPCommon DESTINATION include)

enable_testing()

add_executable(CommonTest
               YPPCommonTest.cpp)

target_link_libraries(CommonTest
                      YPPCommon
                      YPPLogging
                      YPPParser
                      YPPCommonTestSupport
                      ${YARP_LIBRARIES})

# Argument order for test 1 = endpoint name [, IP address / name [, port]]
add_test(NAME TestCreateEndpoint1 COMMAND CommonTest 1 "/service/test/createendpoint_1")
add_test(NAME TestCreateEndpoint2 COMMAND CommonTest 1 "/service/test/createendpoint_2" "localhost")
add_test(NAME TestCreateEndpoint3 COMMAND CommonTest 1 "/service/test/createendpoint_3" "localhost" "12340")
# Test connection to endpoints, same arguments as test 1
add_test(NAME TestConnectToEndpoint1 COMMAND CommonTest 2 "/service/test/connecttoendpoint_1")
add_test(NAME TestConnectToEndpoint2 COMMAND CommonTest 2 "/service/test/connecttoendpoint_2" "localhost")
add_test(NAME TestConnectToEndpoint3 COMMAND CommonTest 2 "/service/test/connecttoendpoint_3" "localhost" "12341")
# Test data write to endpoints, same arguments as test 1
add_test(NAME TestWriteToEndpoint1 COMMAND CommonTest 3 "/service/test/writetoendpoint_1")
add_test(NAME TestWriteToEndpoint2 COMMAND CommonTest 3 "/service/test/writetoendpoint_2" "localhost")
add_test(NAME TestWriteToEndpoint3 COMMAND CommonTest 3 "/service/test/writetoendpoint_3" "localhost" "12342")
# Test data echo from endpoints, same arguments as test 1
add_test(NAME TestEchoFromEndpointWithReader1 COMMAND CommonTest 4 "/service/test/echofromendpointwithreader_1")
add_test(NAME TestEchoFromEndpointWithReader2 COMMAND CommonTest 4 "/service/test/echofromendpointwithreader_2" "localhost")
add_test(NAME TestEchoFromEndpointWithReader3 COMMAND CommonTest 4 "/service/test/echofromendpointwithreader_3" "localhost" "12343")
# Test data echo from endpoints, using reader creator, same arguments as test 1
add_test(NAME TestEchoFromEndpointWithReaderCreator1 COMMAND CommonTest 5 "/service/test/echofromendpointwithreadercreator_1")
add_test(NAME TestEchoFromEndpointWithReaderCreator2 COMMAND CommonTest 5 "/service/test/echofromendpointwithreadercreator_2" "localhost")
add_test(NAME TestEchoFromEndpointWithReaderCreator3 COMMAND CommonTest 5 "/service/test/echofromendpointwithreadercreator_3" "localhost" "12344")
# Test service request creation
add_test(NAME TestCreateRequest1 COMMAND CommonTest 6 "echo")
add_test(NAME TestCreateRequest2 COMMAND CommonTest 6 "echo" "something")
# Test service response creation
add_test(NAME TestCreateResponse1 COMMAND CommonTest 7)
add_test(NAME TestCreateResponse2 COMMAND CommonTest 7 "back")
add_test(NAME TestCreateResponse3 COMMAND CommonTest 7 "back" "at" "you")
# Test echo via request/response, fixed message [echo something to send] but arguments as with test 1
add_test(NAME TestRequestEchoFromEndpoint1 COMMAND CommonTest 8 "/service/test/requestechofromendpoint_1")
add_test(NAME TestRequestEchoFromEndpoint2 COMMAND CommonTest 8 "/service/test/requestechofromendpoint_2" "localhost")
add_test(NAME TestRequestEchoFromEndpoint3 COMMAND CommonTest 8 "/service/test/requestechofromendpoint_3" "localhost" "12345")
# Test echo via service request/response, fixed message [echo something to send] but arguments as with test 1
add_test(NAME TestRequestEchoFromServiceUsingDefaultWithReader1 COMMAND CommonTest 9 "/service/test/requestechofromserviceusingdefaultwithreader_1")
add_test(NAME TestRequestEchoFromServiceUsingDefaultWithReader2 COMMAND CommonTest 9 "/service/test/requestechofromserviceusingdefaultwithreader_2" "localhost")
add_test(NAME TestRequestEchoFromServiceUsingDefaultWithReader3 COMMAND CommonTest 9 "/service/test/requestechofromserviceusingdefaultwithreader_3" "localhost" "12346")
# Test echo via service request/response, fixed message [echo something to send] but arguments as with test 1
add_test(NAME TestRequestEchoFromServiceUsingDefaultWithReaderCreator1 COMMAND CommonTest 10 "/service/test/requestechofromserviceusingdefaultwithreadercreator_1")
add_test(NAME TestRequestEchoFromServiceUsingDefaultWithReaderCreator2 COMMAND CommonTest 10 "/service/test/requestechofromserviceusingdefaultwithreadercreator_2" "localhost")
add_test(NAME TestRequestEchoFromServiceUsingDefaultWithReaderCreator3 COMMAND CommonTest 10 "/service/test/requestechofromserviceusingdefaultwithreadercreator_3" "localhost" "12347")
# Test echo via service request/response, fixed message [echo something to send] but arguments as with test 1
add_test(NAME TestRequestEchoFromServiceWithRequestHandler1 COMMAND CommonTest 11 "/service/test/requestechofromservicewithrequesthandler_1")
add_test(NAME TestRequestEchoFromServiceWithRequestHandler2 COMMAND CommonTest 11 "/service/test/requestechofromservicewithrequesthandler_2" "localhost")
add_test(NAME TestRequestEchoFromServiceWithRequestHandler3 COMMAND CommonTest 11 "/service/test/requestechofromservicewithrequesthandler_3" "localhost" "12348")
# Test list request
add_test(NAME TestRequestEchoFromServiceWithRequestHandlerAndInfo1 COMMAND CommonTest 12 "/service/test/requestechofromservicewithrequesthandlerandinfo_1")
add_test(NAME TestRequestEchoFromServiceWithRequestHandlerAndInfo2 COMMAND CommonTest 12 "/service/test/requestechofromservicewithrequesthandlerandinfo_2" "localhost")
add_test(NAME TestRequestEchoFromServiceWithRequestHandlerAndInfo3 COMMAND CommonTest 12 "/service/test/requestechofromservicewithrequesthandlerandinfo_3" "localhost" "12349")
