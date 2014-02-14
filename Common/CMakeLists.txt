#
#  YarpPlusPlus/Common/CMakeLists.txt
#  YarpPlusPlus
#
#  Created by Norman Jaffe on 2014-02-06.
#  Copyright (c) 2014 OpenDragon. All rights reserved.
#

include_directories("${DEBUGGING_DIR}")

add_library(YPPBaseShared
            YPPEndpoint.cpp
            YPPException.cpp
            YPPInputHandler.cpp
            YPPInputHandlerCreator.cpp
            YPPServiceRequest.cpp
            YPPServiceResponse.cpp)
add_library(YPPBaseClient YPPBaseClient.cpp)
add_library(YPPBaseService YPPBaseService.cpp)

install(TARGETS YPPBaseClient YPPBaseService YPPBaseShared DESTINATION bin)
install(FILES YPPBaseClient YPPBaseService YPPBaseShared DESTINATION include)

enable_testing()

add_executable(CommonTest YPPCommonTest.cpp)

target_link_libraries(CommonTest YPPBaseShared YPPBaseClient YPPBaseService YPPDebugging ${YARP_LIBRARIES})

# Argument order for test 1 = endpoint name [, IP address / name [, port [, carrier]]]
add_test(NAME TestCreateEndpoint1 COMMAND CommonTest 1 "/service/test1_1")
add_test(NAME TestCreateEndpoint2 COMMAND CommonTest 1 "/service/test1_2" "localhost")
add_test(NAME TestCreateEndpoint3 COMMAND CommonTest 1 "/service/test1_3" "localhost" "12340")
add_test(NAME TestCreateEndpoint4 COMMAND CommonTest 1 "/service/test1_4" "localhost" "23450" "tcp")
# Test connection to endpoints, same arguments as test 1
add_test(NAME TestConnectToEndpoint1 COMMAND CommonTest 2 "/service/test2_1")
add_test(NAME TestConnectToEndpoint2 COMMAND CommonTest 2 "/service/test2_2" "localhost")
add_test(NAME TestConnectToEndpoint3 COMMAND CommonTest 2 "/service/test2_3" "localhost" "12340")
add_test(NAME TestConnectToEndpoint4 COMMAND CommonTest 2 "/service/test2_4" "localhost" "23450" "tcp")
# Test data write to endpoints, same arguments as test 1
add_test(NAME TestWriteToEndpoint1 COMMAND CommonTest 3 "/service/test3_1")
add_test(NAME TestWriteToEndpoint2 COMMAND CommonTest 3 "/service/test3_2" "localhost")
add_test(NAME TestWriteToEndpoint3 COMMAND CommonTest 3 "/service/test3_3" "localhost" "12340")
add_test(NAME TestWriteToEndpoint4 COMMAND CommonTest 3 "/service/test3_4" "localhost" "23450" "tcp")
# Test data echo from endpoints, same arguments as test 1
add_test(NAME TestEchoFromEndpoint1 COMMAND CommonTest 4 "/service/test4_1")
add_test(NAME TestEchoFromEndpoint2 COMMAND CommonTest 4 "/service/test4_2" "localhost")
add_test(NAME TestEchoFromEndpoint3 COMMAND CommonTest 4 "/service/test4_3" "localhost" "12340")
add_test(NAME TestEchoFromEndpoint4 COMMAND CommonTest 4 "/service/test4_4" "localhost" "23450" "tcp")
# Test data echo from endpoints, using reader creator, same arguments as test 1
add_test(NAME TestEchoFromEndpoint5 COMMAND CommonTest 5 "/service/test5_1")
add_test(NAME TestEchoFromEndpoint6 COMMAND CommonTest 5 "/service/test5_2" "localhost")
add_test(NAME TestEchoFromEndpoint7 COMMAND CommonTest 5 "/service/test5_3" "localhost" "12340")
add_test(NAME TestEchoFromEndpoint8 COMMAND CommonTest 5 "/service/test5_4" "localhost" "23450" "tcp")

# First tests - creation of objects
add_test(NAME TestCreateRequest COMMAND CommonTest 10)
add_test(NAME TestCreateResponse COMMAND CommonTest 11)
add_test(NAME TestCreateService COMMAND CommonTest 12)
add_test(NAME TestCreateClient COMMAND CommonTest 13)
