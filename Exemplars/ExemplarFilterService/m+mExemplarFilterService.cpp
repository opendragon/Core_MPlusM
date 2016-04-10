//--------------------------------------------------------------------------------------------------
//
//  File:       m+mExemplarFilterService.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the exemplar filter service.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2014 by H Plus Technologies Ltd. and Simon Fraser University.
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
//  Created:    2014-09-15
//
//--------------------------------------------------------------------------------------------------

#include "m+mExemplarFilterService.h"

#include "m+mExemplarFilterInputHandler.h"
#include "m+mExemplarFilterRequests.h"

#include <m+m/m+mEndpoint.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the exemplar filter service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Exemplar;

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

ExemplarFilterService::ExemplarFilterService(const Utilities::DescriptorVector & argumentList,
                                             const YarpString &                  launchPath,
                                             const int                           argc,
                                             char * *                            argv,
                                             const YarpString &                  tag,
                                             const YarpString &
                                                                             serviceEndpointName,
                                             const YarpString &
                                                                             servicePortNumber) :
    inherited(argumentList, launchPath, argc, argv, tag, true, MpM_EXEMPLARFILTER_CANONICAL_NAME_,
              EXEMPLARFILTER_SERVICE_DESCRIPTION_, "", serviceEndpointName, servicePortNumber),
    _inHandler(new ExemplarFilterInputHandler)
{
    ODL_ENTER(); //####
    ODL_P2("argumentList = ", &argumentList, "argv = ", argv); //####
    ODL_S4s("launchPath = ", launchPath, "tag = ", tag, "serviceEndpointName = ", //####
               serviceEndpointName, "servicePortNumber = ", servicePortNumber); //####
    ODL_LL1("argc = ", argc); //####
    ODL_EXIT_P(this); //####
} // ExemplarFilterService::ExemplarFilterService

ExemplarFilterService::~ExemplarFilterService(void)
{
    ODL_OBJENTER(); //####
    stopStreams();
    delete _inHandler;
    ODL_OBJEXIT(); //####
} // ExemplarFilterService::~ExemplarFilterService

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
DEFINE_CONFIGURE_(ExemplarFilterService)
{
#if (! defined(MpM_DoExplicitDisconnect))
# if MAC_OR_LINUX_
#  pragma unused(details)
# endif // MAC_OR_LINUX_
#endif // ! defined(MpM_DoExplicitDisconnect)
    ODL_OBJENTER(); //####
    ODL_P1("details = ", &details); //####
    bool result = false;

    try
    {
        // Nothing needs to be done.
        result = true;
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT_B(); //####
    return result;
} // ExemplarFilterService::configure
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

DEFINE_DISABLEMETRICS_(ExemplarFilterService)
{
    ODL_OBJENTER(); //####
    inherited::disableMetrics();
    if (_inHandler)
    {
        _inHandler->disableMetrics();
    }
    ODL_OBJEXIT(); //####
} // ExemplarFilterService::disableMetrics

DEFINE_ENABLEMETRICS_(ExemplarFilterService)
{
    ODL_OBJENTER(); //####
    inherited::enableMetrics();
    if (_inHandler)
    {
        _inHandler->enableMetrics();
    }
    ODL_OBJEXIT(); //####
} // ExemplarFilterService::enableMetrics

DEFINE_GETCONFIGURATION_(ExemplarFilterService)
{
    ODL_OBJENTER(); //####
    ODL_P1("details = ", &details); //####
    bool result = true;

    details.clear();
    ODL_OBJEXIT_B(result); //####
    return result;
} // ExemplarFilterService::getConfiguration

DEFINE_RESTARTSTREAMS_(ExemplarFilterService)
{
    ODL_OBJENTER(); //####
    try
    {
        // No special processing needed.
        stopStreams();
        startStreams();
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT(); //####
} // ExemplarFilterService::restartStreams

DEFINE_SETUPSTREAMDESCRIPTIONS_(ExemplarFilterService)
{
    ODL_OBJENTER(); //####
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
    ODL_OBJEXIT_B(result); //####
    return result;
} // ExemplarFilterService::setUpStreamDescriptions

DEFINE_STARTSERVICE_(ExemplarFilterService)
{
    ODL_OBJENTER(); //####
    try
    {
        if (! isStarted())
        {
            inherited::startService();
            if (isStarted())
            {

            }
            else
            {
                ODL_LOG("! (isStarted())"); //####
            }
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT_B(isStarted()); //####
    return isStarted();
} // ExemplarFilterService::startService

DEFINE_STARTSTREAMS_(ExemplarFilterService)
{
    ODL_OBJENTER(); //####
    try
    {
        if (! isActive())
        {
            if (_inHandler)
            {
                _inHandler->setOutput(getOutletStream(0));
                _inHandler->setChannel(getInletStream(0));
                getInletStream(0)->setReader(*_inHandler);
                setActive();
            }
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT(); //####
} // ExemplarFilterService::startStreams

DEFINE_STOPSERVICE_(ExemplarFilterService)
{
    ODL_OBJENTER(); //####
    bool result;

    try
    {
        result = inherited::stopService();
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT_B(result); //####
    return result;
} // ExemplarFilterService::stopService

DEFINE_STOPSTREAMS_(ExemplarFilterService)
{
    ODL_OBJENTER(); //####
    try
    {
        if (isActive())
        {
            if (_inHandler)
            {
                _inHandler->setOutput(NULL);
            }
            clearActive();
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT(); //####
} // ExemplarFilterService::stopStreams

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
