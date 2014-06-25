#--------------------------------------------------------------------------------------
#
#  File:       RegistryService/CMakeLists.txt
#
#  Project:    M+M
#
#  Contains:   The CMAKE definitions for the RegistryService application.
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
#  Created:    2014-03-14
#
#--------------------------------------------------------------------------------------

set(MpM_REGISTRYTESTSUPPORT_DIR "${MpM_COMMON_DIR}/RegistryTests")

add_subdirectory(RegistryTests)

include_directories("${MpM_LOGGING_DIR}"
                    “${MpM_COMMON_DIR}”
                    "${MpM_SQLITE_DIR}"
                    "${MpM_REGISTRYTESTSUPPORT_DIR}")

# Set up our program
add_executable(mpmRegistryService
               main.cpp
               M+MAssociateRequestHandler.cpp
               M+MColumnNameValidator.cpp
               M+MDisassociateRequestHandler.cpp
               M+MGetAssociatesRequestHandler.cpp
               M+MMatchRequestHandler.cpp
               M+MPingRequestHandler.cpp
               M+MRegisterRequestHandler.cpp
               M+MRegistryCheckThread.cpp
               M+MRegistryService.cpp
               M+MUnregisterRequestHandler.cpp
               ${MpM_SQLITE_DIR}/sqlite3.c)

# Note that the order of inclusion of libraries is critical in Linux, as they appear to only be processed once.
target_link_libraries(mpmRegistryService
                      mpmParser
                      mpmCommon
                      odLogging
                      ${YARP_LIBRARIES})

install(TARGETS mpmRegistryService DESTINATION bin)

enable_testing()

add_executable(mpmRegistryTest
               M+MRegistryTest.cpp
               M+MAssociateRequestHandler.cpp
               M+MColumnNameValidator.cpp
               M+MDisassociateRequestHandler.cpp
               M+MGetAssociatesRequestHandler.cpp
               M+MMatchRequestHandler.cpp
               M+MPingRequestHandler.cpp
               M+MRegisterRequestHandler.cpp
               M+MRegistryCheckThread.cpp
               M+MRegistryService.cpp
               M+MUnregisterRequestHandler.cpp
               ${MpM_SQLITE_DIR}/sqlite3.c)

# Note that the order of inclusion of libraries is critical in Linux, as they appear to only be processed once.
target_link_libraries(mpmRegistryTest
                      mpmRegistryTestSupport
                      mpmParser
                      mpmCommon
                      odLogging
                      ${YARP_LIBRARIES})

# Test create register service
add_test(NAME TestCreateRegisterService1 COMMAND mpmRegistryTest 13)
add_test(NAME TestCreateRegisterService2 COMMAND mpmRegistryTest 13 "12350")
# Test register request, second service
add_test(NAME TestRequestRegisterService1 COMMAND mpmRegistryTest 14)
add_test(NAME TestRequestRegisterService2 COMMAND mpmRegistryTest 14 "12351")
# Test unregister request, second service
add_test(NAME TestRequestUnregisterService1 COMMAND mpmRegistryTest 15)
add_test(NAME TestRequestUnregisterService2 COMMAND mpmRegistryTest 15 "12352")
# Test search for request / service - note the change in arguments
add_test(NAME TestRequestSearchService1 COMMAND mpmRegistryTest 16 0 "" "FAILED \"An error message\"")
add_test(NAME TestRequestSearchService2 COMMAND mpmRegistryTest 16 0 "Version 42" "OK ()")
add_test(NAME TestRequestSearchService3 COMMAND mpmRegistryTest 16 0 "Details:Echo*" "OK (/_service_/test/requestsearchservice)")
add_test(NAME TestRequestSearchService4 COMMAND mpmRegistryTest 16 0 "Details:Echo*,Keyword(requests)" "OK (/_service_/test/requestsearchservice /$ervice)")
add_test(NAME TestRequestSearchService5 COMMAND mpmRegistryTest 16 0 "Description:*unit\\ tests" "OK (/_service_/test/requestsearchservice)")
add_test(NAME TestRequestSearchService6 COMMAND mpmRegistryTest 16 1 "Details:Echo*" "OK (Test16)")
add_test(NAME TestRequestSearchService7 COMMAND mpmRegistryTest 16 1 "Details:Echo*,Keyword(requests)" "OK (Registry Test16)")
add_test(NAME TestRequestSearchService8 COMMAND mpmRegistryTest 16 1 "Description:*unit\\ tests" "OK (Test16)")
