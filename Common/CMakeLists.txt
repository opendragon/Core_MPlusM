#--------------------------------------------------------------------------------------------------
#
#  File:       Common/CMakeLists.txt
#
#  Project:    M+M
#
#  Contains:   The CMAKE definitions for the M+M common libraries.
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
#  Created:    2014-02-06
#
#--------------------------------------------------------------------------------------------------

set(MpM_COMMONTESTSUPPORT_DIR "${MpM_COMMON_DIR}/CommonTests")

add_subdirectory(CommonTests)

include_directories("${MpM_LOGGING_DIR}"
                    “${MpM_PARSER_DIR}”
                    "${MpM_COMMONTESTSUPPORT_DIR}")

add_library(mpmCommon
            M+MAdapterChannel.cpp
            M+MBailOut.cpp
            M+MBailOutThread.cpp
            M+MBaseAdapterData.cpp
            M+MBaseClient.cpp
            M+MBaseContext.cpp
            M+MBaseFilterService.cpp
            M+MBaseInputOutputService.cpp
            M+MBaseInputService.cpp
            M+MBaseOutputService.cpp
            M+MBaseRequestHandler.cpp
            M+MBaseService.cpp
            M+MBaseServiceInputHandler.cpp
            M+MBaseServiceInputHandlerCreator.cpp
            M+MChannelsRequestHandler.cpp
            M+MChannelStatusReporter.cpp
            M+MClientChannel.cpp
            M+MClientsRequestHandler.cpp
            M+MCommon.cpp
            M+MConfigureRequestHandler.cpp
            M+MCountRequestHandler.cpp
            M+MDetachRequestHandler.cpp
            M+MEndpoint.cpp
            M+MException.cpp
            M+MGeneralChannel.cpp
            M+MInfoRequestHandler.cpp
            M+MInputHandler.cpp
            M+MInputHandlerCreator.cpp
            M+MListRequestHandler.cpp
            M+MNameRequestHandler.cpp
            M+MPingThread.cpp
            M+MQuitRequestHandler.cpp
            M+MRequestMap.cpp
            M+MRestartStreamsRequestHandler.cpp
            M+MServiceChannel.cpp
            M+MServiceRequest.cpp
            M+MServiceResponse.cpp
            M+MStartStreamsRequestHandler.cpp
            M+MStopStreamsRequestHandler.cpp
            M+MUtilities.cpp)

install(TARGETS mpmCommon DESTINATION lib)
install(FILES
        M+MAdapterChannel.h
        M+MBailOut.h
        M+MBailOutThread.h
        M+MBaseAdapterData.h
        M+MBaseClient.h
        M+MBaseContext.h
        M+MBaseFilterService.h
        M+MBaseInputOutputService.h
        M+MBaseInputService.h
        M+MBaseOutputService.h
        M+MBaseRequestHandler.h
        M+MBaseService.h
        M+MBaseServiceInputHandler.h
        M+MBaseServiceInputHandlerCreator.h
        M+MChannelsRequestHandler.h
        M+MChannelStatusReporter.h
        M+MClientChannel.h
        M+MClientsRequestHandler.h
        M+MCommon.h
        M+MConfig.h
        M+MConfigureRequestHandler.h
        M+MCountRequestHandler.h
        M+MDetachRequestHandler.h
        M+MEndpoint.h
        M+MException.h
        M+MGeneralChannel.h
        M+MInfoRequestHandler.h
        M+MInputHandler.h
        M+MInputHandlerCreator.h
        M+MListRequestHandler.h
        M+MNameRequestHandler.h
        M+MPingThread.h
        M+MQuitRequestHandler.h
        M+MRequestMap.h
        M+MRequests.h
        M+MRestartStreamsRequestHandler.h
        M+MServiceChannel.h
        M+MServiceRequest.h
        M+MServiceResponse.h
        M+MStartStreamsRequestHandler.h
        M+MStopStreamsRequestHandler.h
        M+MUtilities.h
        DESTINATION include)

enable_testing()

add_executable(mpmCommonTest
               M+MCommonTest.cpp)

# Note that the order of inclusion of libraries is critical in Linux, as they appear to only be
# processed once.
target_link_libraries(mpmCommonTest
                      mpmCommonTestSupport
                      mpmParser
                      mpmCommon
                      odLogging
                      ${YARP_LIBRARIES})

# Argument order for test 1 = endpoint name [, IP address / name [, port]]
add_test(NAME TestCreateEndpoint1 COMMAND mpmCommonTest 1 "/service/test/createendpoint_1")
add_test(NAME TestCreateEndpoint2 COMMAND mpmCommonTest 1 "/service/test/createendpoint_2" "12340")
# Test connection to endpoints, same arguments as test 1
add_test(NAME TestConnectToEndpoint1 COMMAND mpmCommonTest 2 "/service/test/connecttoendpoint_1")
add_test(NAME TestConnectToEndpoint2 COMMAND mpmCommonTest 2 "/service/test/connecttoendpoint_2"
            "12341")
# Test data write to endpoints, same arguments as test 1
add_test(NAME TestWriteToEndpoint1 COMMAND mpmCommonTest 3 "/service/test/writetoendpoint_1")
add_test(NAME TestWriteToEndpoint2 COMMAND mpmCommonTest 3 "/service/test/writetoendpoint_2"
            "12342")
# Test data echo from endpoints, same arguments as test 1
add_test(NAME TestEchoFromEndpointWithReader1 COMMAND mpmCommonTest 4
            "/service/test/echofromendpointwithreader_1")
add_test(NAME TestEchoFromEndpointWithReader2 COMMAND mpmCommonTest 4
            "/service/test/echofromendpointwithreader_2" "12343")
# Test data echo from endpoints, using reader creator, same arguments as test 1
add_test(NAME TestEchoFromEndpointWithReaderCreator1 COMMAND mpmCommonTest 5
            "/service/test/echofromendpointwithreadercreator_1")
add_test(NAME TestEchoFromEndpointWithReaderCreator2 COMMAND mpmCommonTest 5
            "/service/test/echofromendpointwithreadercreator_2" "12344")
# Test service request creation
add_test(NAME TestCreateRequest1 COMMAND mpmCommonTest 6 "echo")
add_test(NAME TestCreateRequest2 COMMAND mpmCommonTest 6 "echo" "something")
# Test service response creation
add_test(NAME TestCreateResponse1 COMMAND mpmCommonTest 7)
add_test(NAME TestCreateResponse2 COMMAND mpmCommonTest 7 "back")
add_test(NAME TestCreateResponse3 COMMAND mpmCommonTest 7 "back" "at" "you")
# Test echo via request/response, fixed message [echo something to send] but arguments as with test
# 1
add_test(NAME TestRequestEchoFromEndpoint1 COMMAND mpmCommonTest 8
            "/service/test/requestechofromendpoint_1")
add_test(NAME TestRequestEchoFromEndpoint2 COMMAND mpmCommonTest 8
            "/service/test/requestechofromendpoint_2" "12345")
# Test echo via service request/response, fixed message [echo something to send] but arguments as
# with test 1
add_test(NAME TestRequestEchoFromServiceUsingDefaultWithReader1 COMMAND mpmCommonTest 9
            "/service/test/requestechofromserviceusingdefaultwithreader_1")
add_test(NAME TestRequestEchoFromServiceUsingDefaultWithReader2 COMMAND mpmCommonTest 9
            "/service/test/requestechofromserviceusingdefaultwithreader_2" "12346")
# Test echo via service request/response, fixed message [echo something to send] but arguments as
# with test 1
add_test(NAME TestRequestEchoFromServiceUsingDefaultWithReaderCreator1 COMMAND mpmCommonTest 10
            "/service/test/requestechofromserviceusingdefaultwithreadercreator_1")
add_test(NAME TestRequestEchoFromServiceUsingDefaultWithReaderCreator2 COMMAND mpmCommonTest 10
            "/service/test/requestechofromserviceusingdefaultwithreadercreator_2" "12347")
# Test echo via service request/response, fixed message [echo something to send] but arguments as
# with test 1
add_test(NAME TestRequestEchoFromServiceWithRequestHandler1 COMMAND mpmCommonTest 11
            "/service/test/requestechofromservicewithrequesthandler_1")
add_test(NAME TestRequestEchoFromServiceWithRequestHandler2 COMMAND mpmCommonTest 11
            "/service/test/requestechofromservicewithrequesthandler_2" "12348")
# Test list request
add_test(NAME TestRequestEchoFromServiceWithRequestHandlerAndInfo1 COMMAND mpmCommonTest 12
            "/service/test/requestechofromservicewithrequesthandlerandinfo_1")
add_test(NAME TestRequestEchoFromServiceWithRequestHandlerAndInfo2 COMMAND mpmCommonTest 12
            "/service/test/requestechofromservicewithrequesthandlerandinfo_2" "12349")
