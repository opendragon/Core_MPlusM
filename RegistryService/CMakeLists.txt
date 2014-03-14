#--------------------------------------------------------------------------------------
#
#  File:       RegistryService/CMakeLists.txt
#
#  Project:    YarpPlusPlus
#
#  Contains:   The CMAKE definitions for the RegistryService application.
#
#  Written by: Norman Jaffe
#
#  Copyright:  (c) 2014 by OpenDragon.
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

set(YPP_REGISTRYTESTSUPPORT_DIR "${YPP_COMMON_DIR}/RegistryTests")

add_subdirectory(RegistryTests)

include_directories("${YPP_DEBUGGING_DIR}"
                    “${YPP_COMMON_DIR}”
                    "${YPP_REGISTRYTESTSUPPORT_DIR}")

# set up our program
add_executable(RegistryService
               main.cpp
               YPPMatchRequestHandler.cpp
               YPPRegisterRequestHandler.cpp
               YPPRegistryService.cpp
               YPPUnregisterRequestHandler.cpp)

# link with YARP libraries and our interface classes
target_link_libraries(RegistryService
                      YPPCommon
                      YPPDebugging
                      YPPParser
                      ${YARP_LIBRARIES})

enable_testing()

add_executable(RegistryTest
               YPPRegistryTest.cpp
               YPPMatchRequestHandler.cpp
               YPPRegisterRequestHandler.cpp
               YPPRegistryService.cpp
               YPPUnregisterRequestHandler.cpp)

target_link_libraries(RegistryTest
                      YPPCommon
                      YPPDebugging
                      YPPParser
                      YPPRegistryTestSupport
                      ${YARP_LIBRARIES})

# Test register request
add_test(NAME TestRequestRegisterService1 COMMAND RegistryTest 13)
add_test(NAME TestRequestRegisterService2 COMMAND RegistryTest 13 "localhost")
add_test(NAME TestRequestRegisterService3 COMMAND RegistryTest 13 "localhost" "12340")
# Test register request, second service
add_test(NAME TestRequestRegisterService4 COMMAND RegistryTest 14)
add_test(NAME TestRequestRegisterService5 COMMAND RegistryTest 14 "localhost")
add_test(NAME TestRequestRegisterService6 COMMAND RegistryTest 14 "localhost" "12340")
# Test unregister request, second service
add_test(NAME TestRequestUnregisterService1 COMMAND RegistryTest 15)
add_test(NAME TestRequestUnregisterService2 COMMAND RegistryTest 15 "localhost")
add_test(NAME TestRequestUnregisterService3 COMMAND RegistryTest 15 "localhost" "12340")
# Test search for request / service - note the change in arguments
add_test(NAME TestRequestSearchService1 COMMAND RegistryTest 16 "" "FAILED \"An error message\"")
add_test(NAME TestRequestSearchService2 COMMAND RegistryTest 16 "Version 42" "OK ()")
add_test(NAME TestRequestSearchService3 COMMAND RegistryTest 16 "Description:Echo*" "OK (/service/test16)")
add_test(NAME TestRequestSearchService4 COMMAND RegistryTest 16 "Description:Echo*,Keyword(requests)" "OK (/service/test16 /$ervice)")
