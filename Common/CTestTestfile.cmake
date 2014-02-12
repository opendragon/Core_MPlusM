# CMake generated Testfile for 
# Source directory: /Users/M_M/Documents/YarpPlusPlus/Common
# Build directory: /Users/M_M/Documents/YarpPlusPlus/Common
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
ADD_TEST(TestCreateEndpoint1 "/Users/M_M/Documents/YarpPlusPlus/Common/CommonTest" "1" "/service/test5_1")
ADD_TEST(TestCreateEndpoint2 "/Users/M_M/Documents/YarpPlusPlus/Common/CommonTest" "1" "/service/test5_2" "localhost")
ADD_TEST(TestCreateEndpoint3 "/Users/M_M/Documents/YarpPlusPlus/Common/CommonTest" "1" "/service/test5_3" "localhost" "12340")
ADD_TEST(TestCreateEndpoint4 "/Users/M_M/Documents/YarpPlusPlus/Common/CommonTest" "1" "/service/test5_4" "localhost" "23450" "tcp")
ADD_TEST(TestConnectToEndpoint1 "/Users/M_M/Documents/YarpPlusPlus/Common/CommonTest" "2" "/service/test6_1")
ADD_TEST(TestConnectToEndpoint2 "/Users/M_M/Documents/YarpPlusPlus/Common/CommonTest" "2" "/service/test6_2" "localhost")
ADD_TEST(TestConnectToEndpoint3 "/Users/M_M/Documents/YarpPlusPlus/Common/CommonTest" "2" "/service/test6_3" "localhost" "12340")
ADD_TEST(TestConnectToEndpoint4 "/Users/M_M/Documents/YarpPlusPlus/Common/CommonTest" "2" "/service/test6_4" "localhost" "23450" "tcp")
ADD_TEST(TestWriteToEndpoint1 "/Users/M_M/Documents/YarpPlusPlus/Common/CommonTest" "3" "/service/test7_1")
ADD_TEST(TestWriteToEndpoint2 "/Users/M_M/Documents/YarpPlusPlus/Common/CommonTest" "3" "/service/test7_2" "localhost")
ADD_TEST(TestWriteToEndpoint3 "/Users/M_M/Documents/YarpPlusPlus/Common/CommonTest" "3" "/service/test7_3" "localhost" "12340")
ADD_TEST(TestWriteToEndpoint4 "/Users/M_M/Documents/YarpPlusPlus/Common/CommonTest" "3" "/service/test7_4" "localhost" "23450" "tcp")
ADD_TEST(TestEchoFromEndpoint1 "/Users/M_M/Documents/YarpPlusPlus/Common/CommonTest" "4" "/service/test8_1")
ADD_TEST(TestEchoFromEndpoint2 "/Users/M_M/Documents/YarpPlusPlus/Common/CommonTest" "4" "/service/test8_2" "localhost")
ADD_TEST(TestEchoFromEndpoint3 "/Users/M_M/Documents/YarpPlusPlus/Common/CommonTest" "4" "/service/test8_3" "localhost" "12340")
ADD_TEST(TestEchoFromEndpoint4 "/Users/M_M/Documents/YarpPlusPlus/Common/CommonTest" "4" "/service/test8_4" "localhost" "23450" "udp")
ADD_TEST(TestCreateRequest "/Users/M_M/Documents/YarpPlusPlus/Common/CommonTest" "10")
ADD_TEST(TestCreateResponse "/Users/M_M/Documents/YarpPlusPlus/Common/CommonTest" "11")
ADD_TEST(TestCreateService "/Users/M_M/Documents/YarpPlusPlus/Common/CommonTest" "12")
ADD_TEST(TestCreateClient "/Users/M_M/Documents/YarpPlusPlus/Common/CommonTest" "13")
