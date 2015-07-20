//--------------------------------------------------------------------------------------------------
//
//  File:       m+mRunningSumAltAdapterService.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the Running Sum alternative adapter service.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2015 by H Plus Technologies Ltd. and Simon Fraser University.
//
//              All rights reserved. Redistribution and use in source and binary forms, with or
//              without modification, are permitted provided that the following conditions are met:
//                * Redistributions of source code must retain the above copyright notice, this list
//                  of conditions and the following disclaimer.
//                * Redistributions in binary form must reproduce the above copyright notice, this
//                  list of conditions and the following disclaimer in the documentation and / or
//                  other materials provided with the distribution.
//                * Neither the name of the copyright holders nor the names of its contributors may
//                  be used to endorse or promote products derived from this software without
//                  specific prior written permission.
//
//              THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY
//              EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
//              OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT
//              SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
//              INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
//              TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
//              BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
//              CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
//              ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
//              DAMAGE.
//
//  Created:    2015-05-27
//
//--------------------------------------------------------------------------------------------------

#include "m+mRunningSumAltAdapterService.h"

#include "m+mRunningSumAdapterData.h"
#include "m+mRunningSumAltAdapterRequests.h"
#include "m+mRunningSumInputHandler.h"

#include <m+m/m+mEndpoint.h>
#include <m+m/m+mGeneralChannel.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the Running Sum alternative adapter service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Example;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

RunningSumAltAdapterService::RunningSumAltAdapterService(const Utilities::DescriptorVector &
                                                                                     argumentList,
                                                         const YarpString &
                                                                                     launchPath,
                                                         const int                           argc,
                                                         char * *                            argv,
                                                         const YarpString &                  tag,
                                                         const YarpString &
                                                                             serviceEndpointName,
                                                         const YarpString &
                                                                             servicePortNumber) :
    inherited(argumentList, launchPath, argc, argv, tag, true,
              MpM_RUNNINGSUMALTADAPTER_CANONICAL_NAME_, RUNNINGSUMALTADAPTER_SERVICE_DESCRIPTION_,
              "", serviceEndpointName, servicePortNumber), _inHandler(NULL)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("argumentList = ", &argumentList, "argv = ", argv); //####
    OD_LOG_S4s("launchPath = ", launchPath, "tag = ", tag, "serviceEndpointName = ", //####
               serviceEndpointName, "servicePortNumber = ", servicePortNumber); //####
    OD_LOG_LL1("argc = ", argc); //####
    OD_LOG_EXIT_P(this); //####
} // RunningSumAltAdapterService::RunningSumAltAdapterService

RunningSumAltAdapterService::~RunningSumAltAdapterService(void)
{
    OD_LOG_OBJENTER(); //####
    stopStreams();
    delete _inHandler;
    OD_LOG_OBJEXIT(); //####
} // RunningSumAltAdapterService::~RunningSumAltAdapterService

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
bool RunningSumAltAdapterService::configure(const yarp::os::Bottle & details)
{
#if (! defined(OD_ENABLE_LOGGING))
# if MAC_OR_LINUX_
#  pragma unused(details)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING)
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("details = ", &details); //####
    bool result = false;
    
    try
    {
        // Nothing needs to be done.
        result = true;
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // RunningSumAltAdapterService::configure
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

bool RunningSumAltAdapterService::getConfiguration(yarp::os::Bottle & details)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("details = ", &details); //####
    bool result = true;

    details.clear();
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // RunningSumAltAdapterService::getConfiguration

void RunningSumAltAdapterService::restartStreams(void)
{
    OD_LOG_OBJENTER(); //####
    try
    {
        // No special processing needed.
        stopStreams();
        startStreams();
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT(); //####
} // RunningSumAltAdapterService::restartStreams

void RunningSumAltAdapterService::setUpInputHandlers(RunningSumAdapterData & sharedData)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("sharedData = ", &sharedData); //####
    if (! _inHandler)
    {
        _inHandler = new RunningSumInputHandler(sharedData);
    }
    OD_LOG_OBJEXIT(); //####
} // RunningSumAltAdapterService::setUpInputHandlers

bool RunningSumAltAdapterService::setUpStreamDescriptions(void)
{
    OD_LOG_OBJENTER(); //####
    bool               result = true;
    ChannelDescription description;
    YarpString         rootName(getEndpoint().getName() + "/");
    
    _inDescriptions.clear();
    description._portName = rootName + "input";
    description._portProtocol = "d+";
    description._protocolDescription = "One or more numeric values";
    _inDescriptions.push_back(description);
    _outDescriptions.clear();
    description._portName = rootName + "output";
    description._portProtocol = "i+";
    description._protocolDescription = "One or more integer values";
    _outDescriptions.push_back(description);
    _clientDescriptions.clear();
    description._portName = rootName + "client";
    description._portProtocol = "";
    description._protocolDescription = "";
    _clientDescriptions.push_back(description);
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // RunningSumAltAdapterService::setUpStreamDescriptions

bool RunningSumAltAdapterService::start(void)
{
    OD_LOG_OBJENTER(); //####
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
                OD_LOG("! (isStarted())"); //####
            }
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(isStarted()); //####
    return isStarted();
} // RunningSumAltAdapterService::start

void RunningSumAltAdapterService::startStreams(void)
{
    OD_LOG_OBJENTER(); //####
    try
    {
        if (! isActive())
        {
            if (_inHandler)
            {
                getInletStream(0)->setReader(*_inHandler);
                setActive();
            }
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT(); //####
} // RunningSumAltAdapterService::startStreams

bool RunningSumAltAdapterService::stop(void)
{
    OD_LOG_OBJENTER(); //####
    bool result;
    
    try
    {
        result = inherited::stop();
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // RunningSumAltAdapterService::stop

void RunningSumAltAdapterService::stopStreams(void)
{
    OD_LOG_OBJENTER(); //####
    try
    {
        if (isActive())
        {
            clearActive();
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT(); //####
} // RunningSumAltAdapterService::stopStreams

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
