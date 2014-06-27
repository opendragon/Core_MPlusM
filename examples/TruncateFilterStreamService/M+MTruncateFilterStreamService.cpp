//--------------------------------------------------------------------------------------
//
//  File:       M+MTruncateFilterStreamService.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for a simple M+M service.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2014 by HPlus Technologies Ltd. and Simon Fraser University.
//
//              All rights reserved. Redistribution and use in source and binary forms,
//              with or without modification, are permitted provided that the following
//              conditions are met:
//                * Redistributions of source code must retain the above copyright
//                  notice, this list of conditions and the following disclaimer.
//                * Redistributions in binary form must reproduce the above copyright
//                  notice, this list of conditions and the following disclaimer in the
//                  documentation and/or other materials provided with the
//                  distribution.
//                * Neither the name of the copyright holders nor the names of its
//                  contributors may be used to endorse or promote products derived
//                  from this software without specific prior written permission.
//
//              THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
//              "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
//              LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
//              PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
//              OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
//              SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
//              LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
//              DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
//              THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
//              (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
//              OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
//  Created:    2014-06-24
//
//--------------------------------------------------------------------------------------

#include "M+MTruncateFilterStreamService.h"
#include "M+MEndpoint.h"
#include "M+MTruncateFilterStreamRequests.h"

//#include "ODEnableLogging.h"
#include "ODLogging.h"

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The class definition for a simple M+M service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Example;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and destructors
#endif // defined(__APPLE__)

TruncateFilterStreamService::TruncateFilterStreamService(const yarp::os::ConstString & launchPath,
                                                         const yarp::os::ConstString & serviceEndpointName,
                                                         const yarp::os::ConstString & servicePortNumber) :
        inherited(launchPath, true, MpM_TRUNCATE_CANONICAL_NAME, "An example truncate filter service", "",
                  serviceEndpointName, servicePortNumber)
{
    OD_LOG_ENTER();//####
    OD_LOG_S3("launchPath = ", launchPath.c_str(), "serviceEndpointName = ", serviceEndpointName.c_str(),//####
              "servicePortNumber = ", servicePortNumber.c_str());//####
    OD_LOG_EXIT_P(this);//####
} // TruncateFilterStreamService::TruncateFilterStreamService

TruncateFilterStreamService::~TruncateFilterStreamService(void)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_OBJEXIT();//####
} // TruncateFilterStreamService::~TruncateFilterStreamService

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

bool TruncateFilterStreamService::configure(const Common::Package & details)
{
    OD_LOG_OBJENTER();//####
    bool result = false;
    
    try
    {
        // Nothing needs to be done.
        std::cerr << "configure" << std::endl;//$$$$
        result = true;
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT_B();//####
    return result;
} // TruncateFilterStreamService::configure

void TruncateFilterStreamService::restartStreams(void)
{
    OD_LOG_OBJENTER();//####
    try
    {
        // No special processing needed.
        startStreams();
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT();//####
} // TruncateFilterStreamService::restartStreams

bool TruncateFilterStreamService::setUpStreamDescriptions(void)
{
    OD_LOG_OBJENTER();//####
    bool                       result = true;
    Common::ChannelDescription description;
    
    _inDescriptions.clear();
    description._portName = "example/truncatefilterstream/input_";
    description._portProtocol = "d+";
    _inDescriptions.push_back(description);
    _outDescriptions.clear();
    description._portName = "example/truncatefilterstream/output_";
    description._portProtocol = "i+";
    _outDescriptions.push_back(description);
    OD_LOG_OBJEXIT_B(result);//####
    return result;
} // TruncateFilterStreamService::setUpStreamDescriptions

bool TruncateFilterStreamService::start(void)
{
    OD_LOG_OBJENTER();//####
    try
    {
        if (! isStarted())
        {
            inherited::start();
            if (isStarted())
            {
                
            }
            else
            {
                OD_LOG("! (isStarted())");//####
            }
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT_B(isStarted());//####
    return isStarted();
} // TruncateFilterStreamService::start

void TruncateFilterStreamService::startStreams(void)
{
    OD_LOG_OBJENTER();//####
    try
    {
        if (! isActive())
        {
            std::cerr << "startStreams" << std::endl;//$$$$
            
            
            
            setActive();
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT();//####
} // TruncateFilterStreamService::startStreams

bool TruncateFilterStreamService::stop(void)
{
    OD_LOG_OBJENTER();//####
    bool result;
    
    try
    {
        result = inherited::stop();
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT_B(result);//####
    return result;
} // TruncateFilterStreamService::stop

void TruncateFilterStreamService::stopStreams(void)
{
    OD_LOG_OBJENTER();//####
    try
    {
        if (isActive())
        {
            std::cerr << "stopStreams" << std::endl;//$$$$
            
            
            
            clearActive();
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT();//####
} // TruncateFilterStreamService::stopStreams
