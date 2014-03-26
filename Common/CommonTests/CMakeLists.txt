#--------------------------------------------------------------------------------------
#
#  File:       CommonTests/CMakeLists.txt
#
#  Project:    YarpPlusPlus
#
#  Contains:   The CMAKE definitions for the common unit test application.
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
#  Created:    2014-02-28
#
#--------------------------------------------------------------------------------------

include_directories("${YPP_LOGGING_DIR}"
                    "${YPP_COMMON_DIR}"
                    “${YPP_PARSER_DIR}”)

add_library(YPPCommonTestSupport
            YPPTEndpointStatusReporter.cpp
            YPPTTest03Handler.cpp
            YPPTTest04Handler.cpp
            YPPTTest05Handler.cpp
            YPPTTest05HandlerCreator.cpp
            YPPTTest08Handler.cpp
            YPPTTest09DefaultRequestHandler.cpp
            YPPTTest09Service.cpp
            YPPTTest10DefaultRequestHandler.cpp
            YPPTTest10Service.cpp
            YPPTTest11EchoRequestHandler.cpp
            YPPTTest11Service.cpp
            YPPTTest12EchoRequestHandler.cpp
            YPPTTest12Service.cpp)

install(TARGETS YPPCommonTestSupport DESTINATION bin)
install(FILES YPPCommonTestSupport DESTINATION include)
