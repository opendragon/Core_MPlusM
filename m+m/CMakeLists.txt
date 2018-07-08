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
else()
    set(DO_SHARED "")
endif()

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
            "${MpM_SOURCE_DIR}/m+m/m+mBaseThread.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mBoolArgumentDescriptor.cpp"
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
            "${MpM_SOURCE_DIR}/m+m/m+mStringBuffer.cpp"
            "${MpM_SOURCE_DIR}/m+m/m+mUtilities.cpp")

target_link_libraries(${THIS_TARGET}
                        ${ODL_LIBRARY}
                        ${YARP_LIBRARIES}
                        ${ACE_LIBRARIES}
                        ${BONJOUR_LIB})

if(LINUX)
    target_link_libraries(${THIS_TARGET}
                          pthread)
endif()

if(APPLE)
    add_custom_command(TARGET ${THIS_TARGET} POST_BUILD COMMAND install_name_tool -change
                        libACE.dylib "@rpath/libACE.dylib" ${THIS_TARGET_LIB})

    add_custom_command(TARGET ${THIS_TARGET} POST_BUILD COMMAND install_name_tool -change
                        ${ODL_LIBRARY_ROOT} ${ODL_LIBRARY} ${THIS_TARGET_LIB})

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
endif()

install(TARGETS ${THIS_TARGET}
        EXPORT ${PROJECT_NAME}
        DESTINATION ${LIB_DEST}
        COMPONENT libraries)

install(FILES
        "${MpM_SOURCE_DIR}/m+m/optionparser.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mAddressArgumentDescriptor.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mBailOut.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mBailOutThread.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mBaseAdapterData.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mBaseAdapterService.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mBaseArgumentDescriptor.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mBaseChannel.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mBaseClient.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mBaseContext.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mBaseFilterService.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mBaseInputHandler.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mBaseInputHandlerCreator.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mBaseInputOutputService.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mBaseInputService.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mBaseMatcher.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mBaseNameValidator.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mBaseOutputService.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mBaseRequestHandler.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mBaseService.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mBaseThread.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mBoolArgumentDescriptor.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mChannelArgumentDescriptor.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mChannelStatusReporter.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mClientChannel.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mCommon.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mConfig.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mDoubleArgumentDescriptor.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mEndpoint.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mException.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mExtraArgumentDescriptor.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mFilePathArgumentDescriptor.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mGeneralChannel.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mIntArgumentDescriptor.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mMatchConstraint.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mMatchExpression.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mMatchFieldName.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mMatchFieldWithValues.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mMatchValue.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mMatchValueList.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mPortArgumentDescriptor.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mRequestMap.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mRequests.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mSendReceiveCounters.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mServiceChannel.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mServiceInputHandler.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mServiceInputHandlerCreator.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mServiceRequest.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mServiceResponse.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mStringArgumentDescriptor.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mStringBuffer.hpp"
        "${MpM_SOURCE_DIR}/m+m/m+mUtilities.hpp"
        DESTINATION ${INCLUDE_DEST}/m+m
        COMPONENT headers)

if(MpM_DO_SWIG)
    set(MPM_SWIG_SOURCES swig_m+m_in/m+mCommon.i
        m+mAddressArgumentDescriptor.hpp m+mAddressArgumentDescriptor.cpp
        m+mBaseArgumentDescriptor.hpp m+mBaseArgumentDescriptor.cpp
        m+mBailOut.hpp m+mBailOut.cpp
        m+mBailOutThread.hpp m+mBailOutThread.cpp
        m+mBaseAdapterData.hpp m+mBaseAdapterData.cpp
        m+mBaseAdapterService.hpp m+mBaseAdapterService.cpp
        m+mBaseChannel.hpp m+mBaseChannel.cpp
        m+mBaseClient.hpp m+mBaseClient.cpp
        m+mBaseContext.hpp m+mBaseContext.cpp
        m+mBaseFilterService.hpp m+mBaseFilterService.cpp
        m+mBaseInputHandler.hpp m+mBaseInputHandler.cpp
        m+mBaseInputHandlerCreator.hpp m+mBaseInputHandlerCreator.cpp
        m+mBaseInputOutputService.hpp m+mBaseInputOutputService.cpp
        m+mBaseInputService.hpp m+mBaseInputService.cpp
        m+mBaseMatcher.hpp m+mBaseMatcher.cpp
        m+mBaseNameValidator.hpp m+mBaseNameValidator.cpp
        m+mBaseOutputService.hpp m+mBaseOutputService.cpp
        m+mBaseRequestHandler.hpp m+mBaseRequestHandler.cpp
        m+mBaseService.hpp m+mBaseService.cpp
        m+mBaseThread.hpp m+mBaseThread.cpp
        m+mBoolArgumentDescriptor.hpp m+mBoolArgumentDescriptor.cpp
        m+mChannelArgumentDescriptor.hpp m+mChannelArgumentDescriptor.cpp
        m+mChannelStatusReporter.hpp m+mChannelStatusReporter.cpp
        m+mClientChannel.hpp m+mClientChannel.cpp
        m+mCommon.hpp m+mCommon.cpp
        m+mDoubleArgumentDescriptor.hpp m+mDoubleArgumentDescriptor.cpp
        m+mEndpoint.hpp m+mEndpoint.cpp
        m+mException.hpp m+mException.cpp
        m+mExtraArgumentDescriptor.hpp m+mExtraArgumentDescriptor.cpp
        m+mFilePathArgumentDescriptor.hpp m+mFilePathArgumentDescriptor.cpp
        m+mGeneralChannel.hpp m+mGeneralChannel.cpp
        m+mIntArgumentDescriptor.hpp m+mIntArgumentDescriptor.cpp
        m+mMatchConstraint.hpp m+mMatchConstraint.cpp
        m+mMatchExpression.hpp m+mMatchExpression.cpp
        m+mMatchFieldName.hpp m+mMatchFieldName.cpp
        m+mMatchFieldWithValues.hpp m+mMatchFieldWithValues.cpp
        m+mMatchValue.hpp m+mMatchValue.cpp
        m+mMatchValueList.hpp m+mMatchValueList.cpp
        m+mPortArgumentDescriptor.hpp m+mPortArgumentDescriptor.cpp
        m+mRequestMap.hpp m+mRequestMap.cpp
        m+mSendReceiveCounters.hpp m+mSendReceiveCounters.cpp
        m+mServiceChannel.hpp m+mServiceChannel.cpp
        m+mServiceInputHandler.h m+mServiceInputHandler.cpp
        m+mServiceInputHandlerCreator.hpp m+mServiceInputHandlerCreator.cpp
        m+mServiceRequest.hpp m+mServiceRequest.cpp
        m+mServiceResponse.hpp m+mServiceResponse.cpp
        m+mStringArgumentDescriptor.hpp m+mStringArgumentDescriptor.cpp
        m+mStringBuffer.hpp m+mStringBuffer.cpp
        m+mUtilities.hpp m+mUtilities.cpp)

    include_directories("${MpM_SOURCE_DIR}/m+m")
    set_source_files_properties(swig_m+m_in/m+mCommon.i PROPERTIES CPLUSPLUS ON)
    set_source_files_properties(swig_m+m_in/m+mCommon.i PROPERTIES SWIG_FLAGS "-includeall")

    set(CMAKE_SWIG_OUTDIR "${MpM_SOURCE_DIR}/m+m/swig_m+m_out_csharp")

    swig_add_module(${OTHER_TARGET_CSHARP} csharp ${MPM_SWIG_SOURCES})

    swig_link_libraries(${OTHER_TARGET_CSHARP}
                        ${THIS_TARGET}
                        ${ODL_LIBRARY}
                        ${YARP_LIBRARIES}
                        ${ACE_LIBRARIES})

    install(TARGETS ${OTHER_TARGET_CSHARP}
            DESTINATION ${LIB_DEST}
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
                        ${ODL_LIBRARY}
                        ${YARP_LIBRARIES}
                        ${ACE_LIBRARIES}
                        ${PYTHON_LIBRARIES})

# Note the modified target name!
    install(TARGETS _${OTHER_TARGET_PYTHON}
            DESTINATION ${LIB_DEST}
            COMPONENT libraries)

    install(FILES
            swig_m+m_out_python/m+m.py
            swig_m+m_out_python/m+mCommonPYTHON_wrap.cxx
            DESTINATION python/m+m
            COMPONENT headers)

endif()

enable_testing()

#packing stuff
