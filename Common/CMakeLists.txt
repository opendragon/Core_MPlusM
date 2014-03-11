#
#  YarpPlusPlus/Common/CMakeLists.txt
#  YarpPlusPlus
#
#  Created by Norman Jaffe on 2014-02-06.
#  Copyright (c) 2014 OpenDragon. All rights reserved.
#

set(YPP_TESTSUPPORT_DIR "${YPP_COMMON_DIR}/Tests")

add_subdirectory(Tests)

include_directories("${YPP_DEBUGGING_DIR}"
                    “${YPP_PARSER_DIR}”
                    "${YPP_TESTSUPPORT_DIR}")

add_library(YPPBaseShared
            YPPEndpoint.cpp
            YPPException.cpp
            YPPInfoRequestHandler.cpp
            YPPInputHandler.cpp
            YPPInputHandlerCreator.cpp
            YPPListRequestHandler.cpp
            YPPMatchRequestHandler.cpp
            YPPRegisterRequestHandler.cpp
            YPPRegistryService.cpp
            YPPRequestHandler.cpp
            YPPRequestMap.cpp
            YPPServiceRequest.cpp
            YPPServiceResponse.cpp
            YPPUnregisterRequestHandler.cpp)
add_library(YPPBaseClient YPPBaseClient.cpp)
add_library(YPPBaseService YPPBaseService.cpp YPPBaseServiceInputHandler.cpp YPPBaseServiceInputHandlerCreator.cpp)

install(TARGETS YPPBaseClient YPPBaseService YPPBaseShared DESTINATION bin)
install(FILES YPPBaseClient YPPBaseService YPPBaseShared DESTINATION include)

enable_testing()

add_executable(CommonTest YPPCommonTest.cpp)

target_link_libraries(CommonTest YPPBaseShared YPPBaseClient YPPBaseService YPPDebugging YPPParser YPPTestSupport ${YARP_LIBRARIES})

# Argument order for test 1 = endpoint name [, IP address / name [, port]]
add_test(NAME TestCreateEndpoint1 COMMAND CommonTest 1 "/service/test1_1")
add_test(NAME TestCreateEndpoint2 COMMAND CommonTest 1 "/service/test1_2" "localhost")
add_test(NAME TestCreateEndpoint3 COMMAND CommonTest 1 "/service/test1_3" "localhost" "12340")
# Test connection to endpoints, same arguments as test 1
add_test(NAME TestConnectToEndpoint1 COMMAND CommonTest 2 "/service/test2_1")
add_test(NAME TestConnectToEndpoint2 COMMAND CommonTest 2 "/service/test2_2" "localhost")
add_test(NAME TestConnectToEndpoint3 COMMAND CommonTest 2 "/service/test2_3" "localhost" "12340")
# Test data write to endpoints, same arguments as test 1
add_test(NAME TestWriteToEndpoint1 COMMAND CommonTest 3 "/service/test3_1")
add_test(NAME TestWriteToEndpoint2 COMMAND CommonTest 3 "/service/test3_2" "localhost")
add_test(NAME TestWriteToEndpoint3 COMMAND CommonTest 3 "/service/test3_3" "localhost" "12340")
# Test data echo from endpoints, same arguments as test 1
add_test(NAME TestEchoFromEndpoint1 COMMAND CommonTest 4 "/service/test4_1")
add_test(NAME TestEchoFromEndpoint2 COMMAND CommonTest 4 "/service/test4_2" "localhost")
add_test(NAME TestEchoFromEndpoint3 COMMAND CommonTest 4 "/service/test4_3" "localhost" "12340")
# Test data echo from endpoints, using reader creator, same arguments as test 1
add_test(NAME TestEchoFromEndpoint4 COMMAND CommonTest 5 "/service/test5_1")
add_test(NAME TestEchoFromEndpoint5 COMMAND CommonTest 5 "/service/test5_2" "localhost")
add_test(NAME TestEchoFromEndpoint6 COMMAND CommonTest 5 "/service/test5_3" "localhost" "12340")
# Test service request creation
add_test(NAME TestCreateRequest1 COMMAND CommonTest 6 "echo")
add_test(NAME TestCreateRequest2 COMMAND CommonTest 6 "echo" "something")
# Test service response creation
add_test(NAME TestCreateResponse1 COMMAND CommonTest 7)
add_test(NAME TestCreateResponse2 COMMAND CommonTest 7 "back")
add_test(NAME TestCreateResponse3 COMMAND CommonTest 7 "back" "at" "you")
# Test echo via request/response, fixed message [echo something to send] but arguments as with test 1
add_test(NAME TestRequestEchoFromEndpoint1 COMMAND CommonTest 8 "/service/test8_1")
add_test(NAME TestRequestEchoFromEndpoint2 COMMAND CommonTest 8 "/service/test8_2" "localhost")
add_test(NAME TestRequestEchoFromEndpoint3 COMMAND CommonTest 8 "/service/test8_3" "localhost" "12340")
# Test echo via service request/response, fixed message [echo something to send] but arguments as with test 1
add_test(NAME TestRequestEchoFromService1 COMMAND CommonTest 9 "/service/test9_1")
add_test(NAME TestRequestEchoFromService2 COMMAND CommonTest 9 "/service/test9_2" "localhost")
add_test(NAME TestRequestEchoFromService3 COMMAND CommonTest 9 "/service/test9_3" "localhost" "12340")
# Test echo via service request/response, fixed message [echo something to send] but arguments as with test 1
add_test(NAME TestRequestEchoFromService4 COMMAND CommonTest 10 "/service/test10_1")
add_test(NAME TestRequestEchoFromService5 COMMAND CommonTest 10 "/service/test10_2" "localhost")
add_test(NAME TestRequestEchoFromService6 COMMAND CommonTest 10 "/service/test10_3" "localhost" "12340")
# Test echo via service request/response, fixed message [echo something to send] but arguments as with test 1
add_test(NAME TestRequestEchoFromService7 COMMAND CommonTest 11 "/service/test11_1")
add_test(NAME TestRequestEchoFromService8 COMMAND CommonTest 11 "/service/test11_2" "localhost")
add_test(NAME TestRequestEchoFromService9 COMMAND CommonTest 11 "/service/test11_3" "localhost" "12340")
# Test list request
add_test(NAME TestRequestListFromService1 COMMAND CommonTest 12 "/service/test12_1")
add_test(NAME TestRequestListFromService2 COMMAND CommonTest 12 "/service/test12_2" "localhost")
add_test(NAME TestRequestListFromService3 COMMAND CommonTest 12 "/service/test12_3" "localhost" "12340")
# Test register request
add_test(NAME TestRequestRegisterService1 COMMAND CommonTest 13)
add_test(NAME TestRequestRegisterService2 COMMAND CommonTest 13 "localhost")
add_test(NAME TestRequestRegisterService3 COMMAND CommonTest 13 "localhost" "12340")
# Test register request, second service
add_test(NAME TestRequestRegisterService4 COMMAND CommonTest 14)
add_test(NAME TestRequestRegisterService5 COMMAND CommonTest 14 "localhost")
add_test(NAME TestRequestRegisterService6 COMMAND CommonTest 14 "localhost" "12340")
# Test unregister request, second service
add_test(NAME TestRequestUnregisterService1 COMMAND CommonTest 15)
add_test(NAME TestRequestUnregisterService2 COMMAND CommonTest 15 "localhost")
add_test(NAME TestRequestUnregisterService3 COMMAND CommonTest 15 "localhost" "12340")
# Test search for request / service - note the change in arguments
add_test(NAME TestRequestSearchService1 COMMAND CommonTest 16 "" "FAILED \"An error message\"")
add_test(NAME TestRequestSearchService2 COMMAND CommonTest 16 "Version 42" "OK ()")
add_test(NAME TestRequestSearchService3 COMMAND CommonTest 16 "Description:Echo*" "OK (/service/test16)")
add_test(NAME TestRequestSearchService4 COMMAND CommonTest 16 "Description:Echo*,Keyword(requests)" "OK (/service/test16 /$ervice)")
