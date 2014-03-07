#
#  YarpPlusPlus/Parser/CMakeLists.txt
#  YarpPlusPlus
#
#  Created by Norman Jaffe on 2014-03-07.
#  Copyright (c) 2014 OpenDragon. All rights reserved.
#

include_directories("${YPP_COMMON_DIR}"
                    "${YPP_DEBUGGING_DIR}")

add_library(YPPParser
            YPPMatcher.cpp
            YPPMatchFieldName.cpp
            YPPMatchValue.cpp
            YPPMatchValueList.cpp)

install(TARGETS YPPParser DESTINATION bin)
install(FILES YPPParser DESTINATION include)

enable_testing()

add_executable(ParserTest YPPParserTest.cpp)

target_link_libraries(ParserTest YPPDebugging YPPParser ${YARP_LIBRARIES})

# Argument order for test 1 = expected result (t/f), string to match
add_test(NAME TestParseValue1 COMMAND ParserTest 1 f "")
add_test(NAME TestParseValue2 COMMAND ParserTest 1 t 42)
add_test(NAME TestParseValue3 COMMAND ParserTest 1 t alpha)
add_test(NAME TestParseValue4 COMMAND ParserTest 1 t "alpha beta")
add_test(NAME TestParseValue5 COMMAND ParserTest 1 t "alpha?beta")
add_test(NAME TestParseValue6 COMMAND ParserTest 1 t "alpha*beta")
add_test(NAME TestParseValue7 COMMAND ParserTest 1 t "alpha_beta")
add_test(NAME TestParseValue8 COMMAND ParserTest 1 t "alpha%beta")
add_test(NAME TestParseValue9 COMMAND ParserTest 1 t "'alpha beta'")
add_test(NAME TestParseValue10 COMMAND ParserTest 1 t "\"alpha beta\"")
add_test(NAME TestParseValue11 COMMAND ParserTest 1 f "\"alpha")
add_test(NAME TestParseValue12 COMMAND ParserTest 1 f "'alpha beta\"")
# Test value lists
add_test(NAME TestParseValueList1 COMMAND ParserTest 2 f "")
add_test(NAME TestParseValueList2 COMMAND ParserTest 2 f alpha)
add_test(NAME TestParseValueList3 COMMAND ParserTest 2 f "alpha)")
add_test(NAME TestParseValueList4 COMMAND ParserTest 2 f "(alpha")
add_test(NAME TestParseValueList5 COMMAND ParserTest 2 f "(alpha ")
add_test(NAME TestParseValueList6 COMMAND ParserTest 2 t "(alpha)")
add_test(NAME TestParseValueList7 COMMAND ParserTest 2 t "( alpha )")
add_test(NAME TestParseValueList8 COMMAND ParserTest 2 f "(alpha,")
add_test(NAME TestParseValueList9 COMMAND ParserTest 2 t "(alpha,beta)")
add_test(NAME TestParseValueList10 COMMAND ParserTest 2 f "(alpha,beta")
add_test(NAME TestParseValueList11 COMMAND ParserTest 2 t "  ( alpha , beta )")
# Test field names
add_test(NAME TestParseFieldName1 COMMAND ParserTest 3 f "")
add_test(NAME TestParseFieldName2 COMMAND ParserTest 3 f "blort ")
add_test(NAME TestParseFieldName3 COMMAND ParserTest 3 t "keyword ")
add_test(NAME TestParseFieldName4 COMMAND ParserTest 3 t "keyword:")
add_test(NAME TestParseFieldName5 COMMAND ParserTest 3 t "request ")
