#--------------------------------------------------------------------------------------------------
#
#  File:       mpm/CMakeLists.txt
#
#  Project:    M+M
#
#  Contains:   The CMAKE definitions for the M+M common libraries.
#
#  Written by: Norman Jaffe
#
#  Copyright:  (c) 2014 by HPlus Technologies Ltd. and Simon Fraser University.
#
#              All rights reserved. Redistribution and use in source and binary forms, with or
#              without modification, are permitted provided that the following conditions are met:
#                * Redistributions of source code must retain the above copyright notice, this list
#                  of conditions and the following disclaimer.
#                * Redistributions in binary form must reproduce the above copyright notice, this
#                  list of conditions and the following disclaimer in the documentation and/or
#                  other materials provided with the distribution.
#                * Neither the name of the copyright holders nor the names of its contributors may
#                  be used to endorse or promote products derived from this software without
#                  specific prior written permission.
#
#              THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY
#              EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
#              OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT
#              SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
#              INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
#              TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
#              BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
#              CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
#              ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
#              DAMAGE.
#
#  Created:    2014-02-06
#
#--------------------------------------------------------------------------------------------------

include_directories("${MpM_SOURCE_DIR}")

add_library(mpmCommon
            "${MpM_SOURCE_DIR}/mpm/M+MAdapterChannel.cpp"
            "${MpM_SOURCE_DIR}/mpm/M+MBailOut.cpp"
            "${MpM_SOURCE_DIR}/mpm/M+MBailOutThread.cpp"
            "${MpM_SOURCE_DIR}/mpm/M+MBaseAdapterData.cpp"
            "${MpM_SOURCE_DIR}/mpm/M+MBaseClient.cpp"
            "${MpM_SOURCE_DIR}/mpm/M+MBaseContext.cpp"
            "${MpM_SOURCE_DIR}/mpm/M+MBaseFilterService.cpp"
            "${MpM_SOURCE_DIR}/mpm/M+MBaseInputOutputService.cpp"
            "${MpM_SOURCE_DIR}/mpm/M+MBaseInputService.cpp"
            "${MpM_SOURCE_DIR}/mpm/M+MBaseMatcher.cpp"
            "${MpM_SOURCE_DIR}/mpm/M+MBaseNameValidator.cpp"
            "${MpM_SOURCE_DIR}/mpm/M+MBaseOutputService.cpp"
            "${MpM_SOURCE_DIR}/mpm/M+MBaseRequestHandler.cpp"
            "${MpM_SOURCE_DIR}/mpm/M+MBaseService.cpp"
            "${MpM_SOURCE_DIR}/mpm/M+MBaseServiceInputHandler.cpp"
            "${MpM_SOURCE_DIR}/mpm/M+MBaseServiceInputHandlerCreator.cpp"
            "${MpM_SOURCE_DIR}/mpm/M+MChannelsRequestHandler.cpp"
            "${MpM_SOURCE_DIR}/mpm/M+MChannelStatusReporter.cpp"
            "${MpM_SOURCE_DIR}/mpm/M+MClientChannel.cpp"
            "${MpM_SOURCE_DIR}/mpm/M+MClientsRequestHandler.cpp"
            "${MpM_SOURCE_DIR}/mpm/M+MCommon.cpp"
            "${MpM_SOURCE_DIR}/mpm/M+MConfigureRequestHandler.cpp"
            "${MpM_SOURCE_DIR}/mpm/M+MCountRequestHandler.cpp"
            "${MpM_SOURCE_DIR}/mpm/M+MDetachRequestHandler.cpp"
            "${MpM_SOURCE_DIR}/mpm/M+MEndpoint.cpp"
            "${MpM_SOURCE_DIR}/mpm/M+MException.cpp"
            "${MpM_SOURCE_DIR}/mpm/M+MGeneralChannel.cpp"
            "${MpM_SOURCE_DIR}/mpm/M+MInfoRequestHandler.cpp"
            "${MpM_SOURCE_DIR}/mpm/M+MInputHandler.cpp"
            "${MpM_SOURCE_DIR}/mpm/M+MInputHandlerCreator.cpp"
            "${MpM_SOURCE_DIR}/mpm/M+MListRequestHandler.cpp"
            "${MpM_SOURCE_DIR}/mpm/M+MMatchConstraint.cpp"
            "${MpM_SOURCE_DIR}/mpm/M+MMatchExpression.cpp"
            "${MpM_SOURCE_DIR}/mpm/M+MMatchFieldName.cpp"
            "${MpM_SOURCE_DIR}/mpm/M+MMatchFieldWithValues.cpp"
            "${MpM_SOURCE_DIR}/mpm/M+MMatchValue.cpp"
            "${MpM_SOURCE_DIR}/mpm/M+MMatchValueList.cpp"
            "${MpM_SOURCE_DIR}/mpm/M+MNameRequestHandler.cpp"
            "${MpM_SOURCE_DIR}/mpm/M+MPingThread.cpp"
            "${MpM_SOURCE_DIR}/mpm/M+MQuitRequestHandler.cpp"
            "${MpM_SOURCE_DIR}/mpm/M+MRequestMap.cpp"
            "${MpM_SOURCE_DIR}/mpm/M+MRestartStreamsRequestHandler.cpp"
            "${MpM_SOURCE_DIR}/mpm/M+MServiceChannel.cpp"
            "${MpM_SOURCE_DIR}/mpm/M+MServiceRequest.cpp"
            "${MpM_SOURCE_DIR}/mpm/M+MServiceResponse.cpp"
            "${MpM_SOURCE_DIR}/mpm/M+MStartStreamsRequestHandler.cpp"
            "${MpM_SOURCE_DIR}/mpm/M+MStopStreamsRequestHandler.cpp"
            "${MpM_SOURCE_DIR}/mpm/M+MUtilities.cpp")

install(TARGETS mpmCommon DESTINATION lib)

install(FILES
        "${MpM_SOURCE_DIR}/mpm/M+MAdapterChannel.h"
        "${MpM_SOURCE_DIR}/mpm/M+MBailOut.h"
        "${MpM_SOURCE_DIR}/mpm/M+MBailOutThread.h"
        "${MpM_SOURCE_DIR}/mpm/M+MBaseAdapterData.h"
        "${MpM_SOURCE_DIR}/mpm/M+MBaseClient.h"
        "${MpM_SOURCE_DIR}/mpm/M+MBaseContext.h"
        "${MpM_SOURCE_DIR}/mpm/M+MBaseFilterService.h"
        "${MpM_SOURCE_DIR}/mpm/M+MBaseInputOutputService.h"
        "${MpM_SOURCE_DIR}/mpm/M+MBaseInputService.h"
        "${MpM_SOURCE_DIR}/mpm/M+MBaseMatcher.h"
        "${MpM_SOURCE_DIR}/mpm/M+MBaseNameValidator.h"
        "${MpM_SOURCE_DIR}/mpm/M+MBaseOutputService.h"
        "${MpM_SOURCE_DIR}/mpm/M+MBaseRequestHandler.h"
        "${MpM_SOURCE_DIR}/mpm/M+MBaseService.h"
        "${MpM_SOURCE_DIR}/mpm/M+MBaseServiceInputHandler.h"
        "${MpM_SOURCE_DIR}/mpm/M+MBaseServiceInputHandlerCreator.h"
        "${MpM_SOURCE_DIR}/mpm/M+MChannelStatusReporter.h"
        "${MpM_SOURCE_DIR}/mpm/M+MClientChannel.h"
        "${MpM_SOURCE_DIR}/mpm/M+MCommon.h"
        "${MpM_SOURCE_DIR}/mpm/M+MConfig.h"
        "${MpM_SOURCE_DIR}/mpm/M+MEndpoint.h"
        "${MpM_SOURCE_DIR}/mpm/M+MException.h"
        "${MpM_SOURCE_DIR}/mpm/M+MGeneralChannel.h"
        "${MpM_SOURCE_DIR}/mpm/M+MInputHandler.h"
        "${MpM_SOURCE_DIR}/mpm/M+MInputHandlerCreator.h"
        "${MpM_SOURCE_DIR}/mpm/M+MMatchConstraint.h"
        "${MpM_SOURCE_DIR}/mpm/M+MMatchExpression.h"
        "${MpM_SOURCE_DIR}/mpm/M+MMatchFieldName.h"
        "${MpM_SOURCE_DIR}/mpm/M+MMatchFieldWithValues.h"
        "${MpM_SOURCE_DIR}/mpm/M+MMatchValue.h"
        "${MpM_SOURCE_DIR}/mpm/M+MMatchValueList.h"
        "${MpM_SOURCE_DIR}/mpm/M+MRequestMap.h"
        "${MpM_SOURCE_DIR}/mpm/M+MRequests.h"
        "${MpM_SOURCE_DIR}/mpm/M+MServiceChannel.h"
        "${MpM_SOURCE_DIR}/mpm/M+MServiceRequest.h"
        "${MpM_SOURCE_DIR}/mpm/M+MServiceResponse.h"
        "${MpM_SOURCE_DIR}/mpm/M+MUtilities.h"
        DESTINATION include/mpm)
enable_testing()
