#--------------------------------------------------------------------------------------
#
#  File:       RunningSumAdapter/CMakeLists.txt
#
#  Project:    MoAndMe
#
#  Contains:   The CMAKE definitions for the example running sum adapter application.
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
#  Created:    2014-03-18
#
#--------------------------------------------------------------------------------------

include_directories("${MAM_LOGGING_DIR}"
                    "${MAM_COMMON_DIR}"
                    "${MAM_EXAMPLES_DIR}/RunningSumService"
                    "${MAM_EXAMPLES_DIR}/RunningSumClient")

# set up our program
add_executable(RunningSumAdapter
               main.cpp
               MoMeRunningSumAdapterData.cpp
               MoMeRunningSumControlInputHandler.cpp
               MoMeRunningSumDataInputHandler.cpp
               ${MAM_EXAMPLES_DIR}/RunningSumClient/MoMeRunningSumClient.cpp)

# link with YARP libraries and our interface classes
target_link_libraries(RunningSumAdapter
                      MoMeCommon
                      MoMeLogging
                      ${YARP_LIBRARIES})

enable_testing()
