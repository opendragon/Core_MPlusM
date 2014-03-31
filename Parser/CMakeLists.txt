#--------------------------------------------------------------------------------------
#
#  File:       Parser/CMakeLists.txt
#
#  Project:    MoAndMe
#
#  Contains:   The CMAKE definitions for the MoAndMe parser library.
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

# Escape characters can't appear as literals in test regular expressions, since they get removed before the test!
set(ESCAPE_CHAR1 "\\")
set(ESCAPE_CHAR2 "\\\\")

include_directories("${MAM_COMMON_DIR}"
                    "${MAM_LOGGING_DIR}")

add_library(MoMeParser
            MoMeBaseMatcher.cpp
            MoMeBaseNameValidator.cpp
            MoMeMatchConstraint.cpp
            MoMeMatchExpression.cpp
            MoMeMatchFieldName.cpp
            MoMeMatchFieldWithValues.cpp
            MoMeMatchValue.cpp
            MoMeMatchValueList.cpp)

install(TARGETS MoMeParser DESTINATION bin)
install(FILES MoMeParser DESTINATION include)

enable_testing()

add_executable(ParserTest
               MoMeParserTest.cpp
               MoMeTestNameValidator.cpp)

target_link_libraries(ParserTest
                      MoMeLogging
                      MoMeParser
                      ${YARP_LIBRARIES})

# Argument order for test 1 = expected result (t/f), string to match
add_test(NAME TestParseValue1 COMMAND ParserTest 1 f "")
add_test(NAME TestParseValue2 COMMAND ParserTest 1 t 42)
set_property(TEST TestParseValue2
             PROPERTY PASS_REGULAR_EXPRESSION "^'42'\n$")
add_test(NAME TestParseValue3 COMMAND ParserTest 1 t alpha)
set_property(TEST TestParseValue3
             PROPERTY PASS_REGULAR_EXPRESSION "^'alpha'\n$")
add_test(NAME TestParseValue4 COMMAND ParserTest 1 t "alpha beta")
set_property(TEST TestParseValue4
             PROPERTY PASS_REGULAR_EXPRESSION "^'alpha'\n$")
add_test(NAME TestParseValue5 COMMAND ParserTest 1 t "alpha?beta")
set_property(TEST TestParseValue5
             PROPERTY PASS_REGULAR_EXPRESSION "^'alpha_beta'\n$")
add_test(NAME TestParseValue6 COMMAND ParserTest 1 t "alpha*beta")
set_property(TEST TestParseValue6
             PROPERTY PASS_REGULAR_EXPRESSION "^'alpha%beta'\n$")
add_test(NAME TestParseValue7 COMMAND ParserTest 1 t "alpha_beta")
set_property(TEST TestParseValue7
             PROPERTY PASS_REGULAR_EXPRESSION "^'alpha${ESCAPE_CHAR2}_beta' ESCAPE '${ESCAPE_CHAR2}'\n$")
add_test(NAME TestParseValue8 COMMAND ParserTest 1 t "alpha%beta")
set_property(TEST TestParseValue8
             PROPERTY PASS_REGULAR_EXPRESSION "^'alpha${ESCAPE_CHAR2}%beta' ESCAPE '${ESCAPE_CHAR2}'\n$")
add_test(NAME TestParseValue9 COMMAND ParserTest 1 t "'alpha beta'")
set_property(TEST TestParseValue9
             PROPERTY PASS_REGULAR_EXPRESSION "^'alpha beta'\n$")
add_test(NAME TestParseValue10 COMMAND ParserTest 1 t "\"alpha beta\"")
set_property(TEST TestParseValue10
             PROPERTY PASS_REGULAR_EXPRESSION "^'alpha beta'\n$")
add_test(NAME TestParseValue11 COMMAND ParserTest 1 f "\"alpha")
add_test(NAME TestParseValue12 COMMAND ParserTest 1 f "'alpha beta\"")
add_test(NAME TestParseValue13 COMMAND ParserTest 1 t "'alpha(beta'")
set_property(TEST TestParseValue13
             PROPERTY PASS_REGULAR_EXPRESSION "'alpha${ESCAPE_CHAR1}(beta'\n$")
add_test(NAME TestParseValue14 COMMAND ParserTest 1 t "'alpha)beta'")
set_property(TEST TestParseValue14
             PROPERTY PASS_REGULAR_EXPRESSION "'alpha${ESCAPE_CHAR1})beta'\n$")
add_test(NAME TestParseValue15 COMMAND ParserTest 1 t "'alpha&beta'")
set_property(TEST TestParseValue15
             PROPERTY PASS_REGULAR_EXPRESSION "'alpha&beta'\n$")
add_test(NAME TestParseValue16 COMMAND ParserTest 1 t "'alpha,beta'\n$")
set_property(TEST TestParseValue16
             PROPERTY PASS_REGULAR_EXPRESSION "^'alpha,beta'\n$")
# Test value lists
add_test(NAME TestParseValueList1 COMMAND ParserTest 2 f "")
add_test(NAME TestParseValueList2 COMMAND ParserTest 2 f "(")
add_test(NAME TestParseValueList3 COMMAND ParserTest 2 f "()")
add_test(NAME TestParseValueList4 COMMAND ParserTest 2 f "( )")
add_test(NAME TestParseValueList5 COMMAND ParserTest 2 f alpha)
add_test(NAME TestParseValueList6 COMMAND ParserTest 2 f "alpha)")
add_test(NAME TestParseValueList7 COMMAND ParserTest 2 f "(alpha")
add_test(NAME TestParseValueList8 COMMAND ParserTest 2 f "(alpha ")
add_test(NAME TestParseValueList9 COMMAND ParserTest 2 t "(alpha)")
set_property(TEST TestParseValueList9
             PROPERTY PASS_REGULAR_EXPRESSION "^field = 'alpha'\n$")
add_test(NAME TestParseValueList10 COMMAND ParserTest 2 t "( alpha )")
set_property(TEST TestParseValueList10
             PROPERTY PASS_REGULAR_EXPRESSION "^field = 'alpha'\n$")
add_test(NAME TestParseValueList11 COMMAND ParserTest 2 f "(alpha,")
add_test(NAME TestParseValueList12 COMMAND ParserTest 2 t "(alpha,beta)")
set_property(TEST TestParseValueList12
             PROPERTY PASS_REGULAR_EXPRESSION "^field IN ${ESCAPE_CHAR1}('alpha', 'beta'${ESCAPE_CHAR1})\n$")
add_test(NAME TestParseValueList13 COMMAND ParserTest 2 f "(alpha,beta")
add_test(NAME TestParseValueList14 COMMAND ParserTest 2 t "  ( alpha , beta )")
set_property(TEST TestParseValueList14
             PROPERTY PASS_REGULAR_EXPRESSION "^field IN ${ESCAPE_CHAR1}('alpha', 'beta'${ESCAPE_CHAR1})\n$")
# Test field names
add_test(NAME TestParseFieldName1 COMMAND ParserTest 3 f "")
add_test(NAME TestParseFieldName2 COMMAND ParserTest 3 f "blort ")
add_test(NAME TestParseFieldName3 COMMAND ParserTest 3 t "keyword ")
set_property(TEST TestParseFieldName3
             PROPERTY PASS_REGULAR_EXPRESSION "^keyword\n$")
add_test(NAME TestParseFieldName4 COMMAND ParserTest 3 t "keyword:")
set_property(TEST TestParseFieldName4
             PROPERTY PASS_REGULAR_EXPRESSION "^keyword\n$")
add_test(NAME TestParseFieldName5 COMMAND ParserTest 3 t "request ")
set_property(TEST TestParseFieldName5
             PROPERTY PASS_REGULAR_EXPRESSION "^request\n$")
add_test(NAME TestParseFieldName6 COMMAND ParserTest 3 t "details(")
set_property(TEST TestParseFieldName6
             PROPERTY PASS_REGULAR_EXPRESSION "^details\n$")
# Test field with value(s)
add_test(NAME TestParseFieldWithValues1 COMMAND ParserTest 4 f "")
add_test(NAME TestParseFieldWithValues2 COMMAND ParserTest 4 f "keyword ")
add_test(NAME TestParseFieldWithValues3 COMMAND ParserTest 4 f "keyword:")
add_test(NAME TestParseFieldWithValues4 COMMAND ParserTest 4 t "keyword alpha")
set_property(TEST TestParseFieldWithValues4
             PROPERTY PASS_REGULAR_EXPRESSION "^keyword = 'alpha'\n$")
add_test(NAME TestParseFieldWithValues5 COMMAND ParserTest 4 t "keyword(alpha)")
set_property(TEST TestParseFieldWithValues5
             PROPERTY PASS_REGULAR_EXPRESSION "^keyword = 'alpha'\n$")
add_test(NAME TestParseFieldWithValues6 COMMAND ParserTest 4 t "keyword alpha${ESCAPE_CHAR1}*")
set_property(TEST TestParseFieldWithValues6
             PROPERTY PASS_REGULAR_EXPRESSION "^keyword = 'alpha${ESCAPE_CHAR1}*'\n$")
add_test(NAME TestParseFieldWithValues7 COMMAND ParserTest 4 t "keyword:alpha")
set_property(TEST TestParseFieldWithValues7
             PROPERTY PASS_REGULAR_EXPRESSION "^keyword = 'alpha'\n$")
add_test(NAME TestParseFieldWithValues8 COMMAND ParserTest 4 t "keyword (alpha,beta)")
set_property(TEST TestParseFieldWithValues8
             PROPERTY PASS_REGULAR_EXPRESSION "^keyword IN ${ESCAPE_CHAR1}('alpha', 'beta'${ESCAPE_CHAR1})\n$")
add_test(NAME TestParseFieldWithValues9 COMMAND ParserTest 4 t "keyword:(alpha,beta)")
set_property(TEST TestParseFieldWithValues9
             PROPERTY PASS_REGULAR_EXPRESSION "^keyword IN ${ESCAPE_CHAR1}('alpha', 'beta'${ESCAPE_CHAR1})\n$")
add_test(NAME TestParseFieldWithValues10 COMMAND ParserTest 4 f "keyword (alpha,)")
add_test(NAME TestParseFieldWithValues11 COMMAND ParserTest 4 f "keyword:(alpha")
add_test(NAME TestParseFieldWithValues12 COMMAND ParserTest 4 t "keyword:(alpha,\"beta gamma\")")
set_property(TEST TestParseFieldWithValues12
             PROPERTY PASS_REGULAR_EXPRESSION "^keyword IN ${ESCAPE_CHAR1}('alpha', 'beta gamma'${ESCAPE_CHAR1})\n$")
add_test(NAME TestParseFieldWithValues13 COMMAND ParserTest 4 t "keyword(alpha,\"beta gamma\")")
set_property(TEST TestParseFieldWithValues13
             PROPERTY PASS_REGULAR_EXPRESSION "^keyword IN ${ESCAPE_CHAR1}('alpha', 'beta gamma'${ESCAPE_CHAR1})\n$")
add_test(NAME TestParseFieldWithValues14 COMMAND ParserTest 4 t "keyword:(alpha,beta*,gamma)")
set_property(TEST TestParseFieldWithValues14
             PROPERTY PASS_REGULAR_EXPRESSION "^${ESCAPE_CHAR1}(keyword IN ${ESCAPE_CHAR1}('alpha', 'gamma'${ESCAPE_CHAR1}) OR keyword LIKE 'beta%'${ESCAPE_CHAR1})\n$")
# Test constraint lists
add_test(NAME TestParseConstraintList1 COMMAND ParserTest 5 f "")
add_test(NAME TestParseConstraintList2 COMMAND ParserTest 5 t "keyword(alpha,\"beta gamma\")")
set_property(TEST TestParseConstraintList2
             PROPERTY PASS_REGULAR_EXPRESSION "^keyword IN ${ESCAPE_CHAR1}('alpha', 'beta gamma'${ESCAPE_CHAR1})\n$")
add_test(NAME TestParseConstraintList3 COMMAND ParserTest 5 f "keyword(alpha,\"beta gamma\")&")
add_test(NAME TestParseConstraintList4 COMMAND ParserTest 5 t "keyword(alpha,\"beta gamma\")& details:alpha")
set_property(TEST TestParseConstraintList4
             PROPERTY PASS_REGULAR_EXPRESSION "^keyword IN ${ESCAPE_CHAR1}('alpha', 'beta gamma'${ESCAPE_CHAR1}) AND details = 'alpha'\n$")
# Test expressions
add_test(NAME TestParseExpression1 COMMAND ParserTest 6 f "")
add_test(NAME TestParseExpression2 COMMAND ParserTest 6 t "keyword(alpha,\"beta gamma\")")
set_property(TEST TestParseExpression2
             PROPERTY PASS_REGULAR_EXPRESSION "^SELECT keyword IN ${ESCAPE_CHAR1}('alpha', 'beta gamma'${ESCAPE_CHAR1})\n$")
add_test(NAME TestParseExpression3 COMMAND ParserTest 6 f "keyword(alpha,\"beta gamma\"),")
add_test(NAME TestParseExpression4 COMMAND ParserTest 6 t "keyword(alpha,\"beta gamma\"), details:alpha")
set_property(TEST TestParseExpression4
             PROPERTY PASS_REGULAR_EXPRESSION "^SELECT keyword IN ${ESCAPE_CHAR1}('alpha', 'beta gamma'${ESCAPE_CHAR1}) UNION SELECT details = 'alpha'\n$")
add_test(NAME TestParseExpression5 COMMAND ParserTest 6 t "keyword(alpha,\"beta gamma\") & details:alpha, request:echo")
set_property(TEST TestParseExpression5
             PROPERTY PASS_REGULAR_EXPRESSION "^SELECT keyword IN ${ESCAPE_CHAR1}('alpha', 'beta gamma'${ESCAPE_CHAR1}) AND details = 'alpha' UNION SELECT request = 'echo'\n$")
