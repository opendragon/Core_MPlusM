#--------------------------------------------------------------------------------------------------
#
#  File:       m+m/CMakeLists.txt
#
#  Project:    m+m
#
#  Contains:   The CMAKE definitions for the m+m common libraries.
#
#  Written by: Norman Jaffe
#
#  Copyright:  (c) 2014 by H Plus Technologies Ltd. and Simon Fraser University.
#
#              All rights reserved. Redistribution and use in source and binary forms, with or
#              without modification, are permitted provided that the following conditions are met:
#                * Redistributions of source code must retain the above copyright notice, this list
#                  of conditions and the following disclaimer.
#                * Redistributions in binary form must reproduce the above copyright notice, this
#                  list of conditions and the following disclaimer in the documentation and / or
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

set(THIS_TARGET m+mCommon)
set(OTHER_TARGET_CSHARP m+mCommon_swig_csharp)
set(OTHER_TARGET_PYTHON m+mCommon_swig_python)

if(APPLE)
    set(DO_SHARED "SHARED")
    set(THIS_TARGET_LIB "lib${THIS_TARGET}.dylib")
else(APPLE)
    set(DO_SHARED "")
endif(APPLE)

add_library(${THIS_TARGET}
            "${DO_SHARED}"
            "${MpM_SOURCE_DIR}/m+m/optionparser.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mAddressArgumentDescriptor.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mArgumentDescriptionsRequestHandler.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mArgumentsRequestHandler.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mBailOut.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mBailOutThread.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mBaseAdapterData.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mBaseAdapterService.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mBaseArgumentDescriptor.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mBaseChannel.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mBaseClient.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mBaseContext.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mBaseFilterService.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mBaseInputHandler.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mBaseInputHandlerCreator.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mBaseInputOutputService.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mBaseInputService.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mBaseMatcher.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mBaseNameValidator.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mBaseOutputService.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mBaseRequestHandler.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mBaseService.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mChannelArgumentDescriptor.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mChannelsRequestHandler.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mChannelStatusReporter.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mClientChannel.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mClientsRequestHandler.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mCommon.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mConfigurationRequestHandler.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mConfigureRequestHandler.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mDetachRequestHandler.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mDoubleArgumentDescriptor.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mEndpoint.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mException.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mExtraArgumentDescriptor.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mExtraInfoRequestHandler.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mFilePathArgumentDescriptor.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mGeneralChannel.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mInfoRequestHandler.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mIntArgumentDescriptor.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mListRequestHandler.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mMatchConstraint.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mMatchExpression.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mMatchFieldName.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mMatchFieldWithValues.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mMatchValue.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mMatchValueList.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mMetricsRequestHandler.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mMetricsStateRequestHandler.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mNameRequestHandler.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mPingThread.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mPortArgumentDescriptor.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mRequestMap.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mRestartStreamsRequestHandler.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mSendReceiveCounters.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mServiceChannel.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mServiceInputHandler.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mServiceInputHandlerCreator.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mServiceRequest.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mServiceResponse.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mSetMetricsStateRequestHandler.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mStartStreamsRequestHandler.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mStopRequestHandler.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mStopStreamsRequestHandler.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mStringArgumentDescriptor.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mUtilities.cpp")

target_link_libraries(${THIS_TARGET}
                        odLogging
                        ${YARP_LIBRARIES}
                        ${ACE_LIBRARIES}
						${BONJOUR_LIB})

if(APPLE)
    add_custom_command(TARGET ${THIS_TARGET} POST_BUILD COMMAND install_name_tool -change
                        libACE.dylib "@rpath/libACE.dylib" ${THIS_TARGET_LIB})

    add_custom_command(TARGET ${THIS_TARGET} POST_BUILD COMMAND install_name_tool -change
                        libodLogging.dylib "@rpath/libodLogging.dylib" ${THIS_TARGET_LIB})

    add_custom_command(TARGET ${THIS_TARGET} POST_BUILD COMMAND install_name_tool -change
                        libYARP_OS.1.dylib "@rpath/libYARP_OS.1.dylib" ${THIS_TARGET_LIB})
    add_custom_command(TARGET ${THIS_TARGET} POST_BUILD COMMAND install_name_tool -change
                        libYARP_sig.1.dylib "@rpath/libYARP_sig.1.dylib" ${THIS_TARGET_LIB})
    add_custom_command(TARGET ${THIS_TARGET} POST_BUILD COMMAND install_name_tool -change
                        libYARP_dev.1.dylib "@rpath/libYARP_dev.1.dylib" ${THIS_TARGET_LIB})
    add_custom_command(TARGET ${THIS_TARGET} POST_BUILD COMMAND install_name_tool -change
                        libYARP_name.1.dylib "@rpath/libYARP_name.1.dylib" ${THIS_TARGET_LIB})
    add_custom_command(TARGET ${THIS_TARGET} POST_BUILD COMMAND install_name_tool -change
                        libYARP_init.1.dylib "@rpath/libYARP_init.1.dylib" ${THIS_TARGET_LIB})
endif(APPLE)

install(TARGETS ${THIS_TARGET}
        DESTINATION lib
        COMPONENT libraries)

install(FILES
        "${MpM_SOURCE_DIR}/m+m/optionparser.h"
        "${MpM_SOURCE_DIR}/m+m/m+mAddressArgumentDescriptor.h"
        "${MpM_SOURCE_DIR}/m+m/m+mBailOut.h"
        "${MpM_SOURCE_DIR}/m+m/m+mBailOutThread.h"
        "${MpM_SOURCE_DIR}/m+m/m+mBaseAdapterData.h"
        "${MpM_SOURCE_DIR}/m+m/m+mBaseAdapterService.h"
        "${MpM_SOURCE_DIR}/m+m/m+mBaseArgumentDescriptor.h"
        "${MpM_SOURCE_DIR}/m+m/m+mBaseChannel.h"
        "${MpM_SOURCE_DIR}/m+m/m+mBaseClient.h"
        "${MpM_SOURCE_DIR}/m+m/m+mBaseContext.h"
        "${MpM_SOURCE_DIR}/m+m/m+mBaseFilterService.h"
        "${MpM_SOURCE_DIR}/m+m/m+mBaseInputHandler.h"
        "${MpM_SOURCE_DIR}/m+m/m+mBaseInputHandlerCreator.h"
        "${MpM_SOURCE_DIR}/m+m/m+mBaseInputOutputService.h"
        "${MpM_SOURCE_DIR}/m+m/m+mBaseInputService.h"
        "${MpM_SOURCE_DIR}/m+m/m+mBaseMatcher.h"
        "${MpM_SOURCE_DIR}/m+m/m+mBaseNameValidator.h"
        "${MpM_SOURCE_DIR}/m+m/m+mBaseOutputService.h"
        "${MpM_SOURCE_DIR}/m+m/m+mBaseRequestHandler.h"
        "${MpM_SOURCE_DIR}/m+m/m+mBaseService.h"
        "${MpM_SOURCE_DIR}/m+m/m+mChannelArgumentDescriptor.h"
        "${MpM_SOURCE_DIR}/m+m/m+mChannelStatusReporter.h"
        "${MpM_SOURCE_DIR}/m+m/m+mClientChannel.h"
        "${MpM_SOURCE_DIR}/m+m/m+mCommon.h"
        "${MpM_SOURCE_DIR}/m+m/m+mConfig.h"
        "${MpM_SOURCE_DIR}/m+m/m+mDoubleArgumentDescriptor.h"
        "${MpM_SOURCE_DIR}/m+m/m+mEndpoint.h"
        "${MpM_SOURCE_DIR}/m+m/m+mException.h"
        "${MpM_SOURCE_DIR}/m+m/m+mExtraArgumentDescriptor.h"
        "${MpM_SOURCE_DIR}/m+m/m+mFilePathArgumentDescriptor.h"
        "${MpM_SOURCE_DIR}/m+m/m+mGeneralChannel.h"
        "${MpM_SOURCE_DIR}/m+m/m+mIntArgumentDescriptor.h"
        "${MpM_SOURCE_DIR}/m+m/m+mMatchConstraint.h"
        "${MpM_SOURCE_DIR}/m+m/m+mMatchExpression.h"
        "${MpM_SOURCE_DIR}/m+m/m+mMatchFieldName.h"
        "${MpM_SOURCE_DIR}/m+m/m+mMatchFieldWithValues.h"
        "${MpM_SOURCE_DIR}/m+m/m+mMatchValue.h"
        "${MpM_SOURCE_DIR}/m+m/m+mMatchValueList.h"
        "${MpM_SOURCE_DIR}/m+m/m+mPortArgumentDescriptor.h"
        "${MpM_SOURCE_DIR}/m+m/m+mRequestMap.h"
        "${MpM_SOURCE_DIR}/m+m/m+mRequests.h"
        "${MpM_SOURCE_DIR}/m+m/m+mSendReceiveCounters.h"
        "${MpM_SOURCE_DIR}/m+m/m+mServiceChannel.h"
        "${MpM_SOURCE_DIR}/m+m/m+mServiceInputHandler.h"
        "${MpM_SOURCE_DIR}/m+m/m+mServiceInputHandlerCreator.h"
        "${MpM_SOURCE_DIR}/m+m/m+mServiceRequest.h"
        "${MpM_SOURCE_DIR}/m+m/m+mServiceResponse.h"
        "${MpM_SOURCE_DIR}/m+m/m+mStringArgumentDescriptor.h"
        "${MpM_SOURCE_DIR}/m+m/m+mUtilities.h"
        DESTINATION include/m+m
		COMPONENT headers)

if(MpM_DO_SWIG)
    set(MPM_SWIG_SOURCES swig_m+m_in/m+mCommon.i
        M+MAddressArgumentDescriptor.h M+MAddressArgumentDescriptor.cpp
        M+MBaseArgumentDescriptor.h M+MBaseArgumentDescriptor.cpp
        M+MBailOut.h M+MBailOut.cpp
        M+MBailOutThread.h M+MBailOutThread.cpp
        M+MBaseAdapterData.h M+MBaseAdapterData.cpp
        M+MBaseAdapterService.h M+MBaseAdapterService.cpp
        M+MBaseChannel.h M+MBaseChannel.cpp
        M+MBaseClient.h M+MBaseClient.cpp
        M+MBaseContext.h M+MBaseContext.cpp
        M+MBaseFilterService.h M+MBaseFilterService.cpp
        M+MBaseInputHandler.h M+MBaseInputHandler.cpp
        M+MBaseInputHandlerCreator.h M+MBaseInputHandlerCreator.cpp
        M+MBaseInputOutputService.h M+MBaseInputOutputService.cpp
        M+MBaseInputService.h M+MBaseInputService.cpp
        M+MBaseMatcher.h M+MBaseMatcher.cpp
        M+MBaseNameValidator.h M+MBaseNameValidator.cpp
        M+MBaseOutputService.h M+MBaseOutputService.cpp
        M+MBaseRequestHandler.h M+MBaseRequestHandler.cpp
        M+MBaseService.h M+MBaseService.cpp
        M+MChannelArgumentDescriptor.h M+MChannelArgumentDescriptor.cpp
        M+MChannelStatusReporter.h M+MChannelStatusReporter.cpp
        M+MClientChannel.h M+MClientChannel.cpp
        M+MCommon.h M+MCommon.cpp
        M+MDoubleArgumentDescriptor.h M+MDoubleArgumentDescriptor.cpp
        M+MEndpoint.h M+MEndpoint.cpp
        M+MException.h M+MException.cpp
        M+MExtraArgumentDescriptor.h M+MExtraArgumentDescriptor.cpp
        M+MFilePathArgumentDescriptor.h M+MFilePathArgumentDescriptor.cpp
        M+MGeneralChannel.h M+MGeneralChannel.cpp
        M+MIntArgumentDescriptor.h M+MIntArgumentDescriptor.cpp
        M+MMatchConstraint.h M+MMatchConstraint.cpp
        M+MMatchExpression.h M+MMatchExpression.cpp
        M+MMatchFieldName.h M+MMatchFieldName.cpp
        M+MMatchFieldWithValues.h M+MMatchFieldWithValues.cpp
        M+MMatchValue.h M+MMatchValue.cpp
        M+MMatchValueList.h M+MMatchValueList.cpp
        M+MPortArgumentDescriptor.h M+MPortArgumentDescriptor.cpp
        M+MRequestMap.h M+MRequestMap.cpp
        M+MSendReceiveCounters.h M+MSendReceiveCounters.cpp
        M+MServiceChannel.h M+MServiceChannel.cpp
        M+MServiceInputHandler.h M+MServiceInputHandler.cpp
        M+MServiceInputHandlerCreator.h M+MServiceInputHandlerCreator.cpp
        M+MServiceRequest.h M+MServiceRequest.cpp
        M+MServiceResponse.h M+MServiceResponse.cpp
        M+MStringArgumentDescriptor.h M+MStringArgumentDescriptor.cpp
        M+MUtilities.h M+MUtilities.cpp)

    include_directories("${MpM_SOURCE_DIR}/m+m")
    set_source_files_properties(swig_m+m_in/m+mCommon.i PROPERTIES CPLUSPLUS ON)
    set_source_files_properties(swig_m+m_in/m+mCommon.i PROPERTIES SWIG_FLAGS "-includeall")

    set(CMAKE_SWIG_OUTDIR "${MpM_SOURCE_DIR}/m+m/swig_m+m_out_csharp")

    swig_add_module(${OTHER_TARGET_CSHARP} csharp ${MPM_SWIG_SOURCES})

    swig_link_libraries(${OTHER_TARGET_CSHARP}
                        ${THIS_TARGET}
                        odLogging
                        ${YARP_LIBRARIES}
                        ${ACE_LIBRARIES})

    install(TARGETS ${OTHER_TARGET_CSHARP}
            DESTINATION lib
            COMPONENT libraries)

    install(FILES
            swig_m+m_out_csharp/m+m.cs
            swig_m+m_out_csharp/m+mPINVOKE.cs
            DESTINATION csharp/m+m
            COMPONENT headers)

    set(CMAKE_SWIG_OUTDIR "${MpM_SOURCE_DIR}/m+m/swig_m+m_out_python")

    find_package(PythonLibs)
    include_directories(${PYTHON_INCLUDE_PATH})

    swig_add_module(${OTHER_TARGET_PYTHON} python ${MPM_SWIG_SOURCES})

    swig_link_libraries(${OTHER_TARGET_PYTHON}
                        ${THIS_TARGET}
                        odLogging
                        ${YARP_LIBRARIES}
                        ${ACE_LIBRARIES}
                        ${PYTHON_LIBRARIES})

# Note the modified target name!
    install(TARGETS _${OTHER_TARGET_PYTHON}
            DESTINATION lib
            COMPONENT libraries)

install(FILES
        swig_m+m_out_python/m+m.py
        swig_m+m_out_python/m+mCommonPYTHON_wrap.cxx
        DESTINATION python/m+m
        COMPONENT headers)

endif(MpM_DO_SWIG)

enable_testing()

#packing stuff