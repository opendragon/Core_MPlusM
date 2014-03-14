#--------------------------------------------------------------------------------------
#
#  File:       Parser/CMakeLists.txt
#
#  Project:    YarpPlusPlus
#
#  Contains:   The CMAKE definitions for the Yarp++ parser library.
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
#  Created:    2014-03-07
#
#--------------------------------------------------------------------------------------

include_directories("${YPP_COMMON_DIR}"
                    "${YPP_DEBUGGING_DIR}")

add_library(YPPParser
            YPPBaseMatcher.cpp
            YPPMatchConstraint.cpp
            YPPMatchExpression.cpp
            YPPMatchFieldName.cpp
            YPPMatchFieldWithValues.cpp
            YPPMatchValue.cpp
            YPPMatchValueList.cpp)

install(TARGETS YPPParser DESTINATION bin)
install(FILES YPPParser DESTINATION include)

enable_testing()

add_executable(ParserTest
               YPPParserTest.cpp)

target_link_libraries(ParserTest
                      YPPDebugging
                      YPPParser
                      ${YARP_LIBRARIES})

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
add_test(NAME TestParseValue13 COMMAND ParserTest 1 t "'alpha(beta'")
add_test(NAME TestParseValue14 COMMAND ParserTest 1 t "'alpha)beta'")
add_test(NAME TestParseValue15 COMMAND ParserTest 1 t "'alpha&beta'")
add_test(NAME TestParseValue16 COMMAND ParserTest 1 t "'alpha,beta'")
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
add_test(NAME TestParseFieldName6 COMMAND ParserTest 3 t "description(")
# Test field with value(s)
add_test(NAME TestParseFieldWithValues1 COMMAND ParserTest 4 f "")
add_test(NAME TestParseFieldWithValues2 COMMAND ParserTest 4 f "keyword ")
add_test(NAME TestParseFieldWithValues3 COMMAND ParserTest 4 f "keyword:")
add_test(NAME TestParseFieldWithValues4 COMMAND ParserTest 4 t "keyword alpha")
add_test(NAME TestParseFieldWithValues5 COMMAND ParserTest 4 t "keyword:alpha")
add_test(NAME TestParseFieldWithValues6 COMMAND ParserTest 4 t "keyword (alpha,beta)")
add_test(NAME TestParseFieldWithValues7 COMMAND ParserTest 4 t "keyword:(alpha,beta)")
add_test(NAME TestParseFieldWithValues8 COMMAND ParserTest 4 f "keyword (alpha,)")
add_test(NAME TestParseFieldWithValues9 COMMAND ParserTest 4 f "keyword:(alpha")
add_test(NAME TestParseFieldWithValues10 COMMAND ParserTest 4 t "keyword:(alpha,\"beta gamma\")")
add_test(NAME TestParseFieldWithValues11 COMMAND ParserTest 4 t "keyword(alpha,\"beta gamma\")")
# Test constraint lists
add_test(NAME TestParseConstraintList1 COMMAND ParserTest 5 f "")
add_test(NAME TestParseConstraintList2 COMMAND ParserTest 5 t "keyword(alpha,\"beta gamma\")")
add_test(NAME TestParseConstraintList3 COMMAND ParserTest 5 f "keyword(alpha,\"beta gamma\")&")
add_test(NAME TestParseConstraintList4 COMMAND ParserTest 5 t "keyword(alpha,\"beta gamma\")& description:alpha")
# Test expressions
add_test(NAME TestParseExpression1 COMMAND ParserTest 6 f "")
add_test(NAME TestParseExpression2 COMMAND ParserTest 6 t "keyword(alpha,\"beta gamma\")")
add_test(NAME TestParseExpression3 COMMAND ParserTest 6 f "keyword(alpha,\"beta gamma\"),")
add_test(NAME TestParseExpression4 COMMAND ParserTest 6 t "keyword(alpha,\"beta gamma\"), description:alpha")
add_test(NAME TestParseExpression5 COMMAND ParserTest 6 t "keyword(alpha,\"beta gamma\") & description:alpha, request:echo")

