//--------------------------------------------------------------------------------------------------
//
//  File:       m+mRunningSumAdapterService.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the Running Sum adapter service.
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

#include "m+mRunningSumAdapterService.hpp"

#include "m+mRunningSumAdapterData.hpp"
#include "m+mRunningSumAdapterRequests.hpp"
#include "m+mRunningSumControlInputHandler.hpp"
#include "m+mRunningSumDataInputHandler.hpp"

#include <m+m/m+mEndpoint.hpp>
#include <m+m/m+mGeneralChannel.hpp>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the Running Sum adapter service. */
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

RunningSumAdapterService::RunningSumAdapterService(const Utilities::DescriptorVector & argumentList,
                                                   const YarpString &                  launchPath,
                                                   const int                           argc,
                                                   char * *                            argv,
                                                   const YarpString &                  tag,
                                                   const YarpString &
                                                                               serviceEndpointName,
                                                   const YarpString &
                                                                               servicePortNumber) :
    inherited(argumentList, launchPath, argc, argv, tag, true,
              MpM_RUNNINGSUMADAPTER_CANONICAL_NAME_, RUNNINGSUMADAPTER_SERVICE_DESCRIPTION_, "",
              serviceEndpointName, servicePortNumber), _controlHandler(NULL), _dataHandler(NULL)
{
    ODL_ENTER(); //####
    ODL_P2("argumentList = ", &argumentList, "argv = ", argv); //####
    ODL_S4s("launchPath = ", launchPath, "tag = ", tag, "serviceEndpointName = ", //####
            serviceEndpointName, "servicePortNumber = ", servicePortNumber); //####
    ODL_LL1("argc = ", argc); //####
    ODL_EXIT_P(this); //####
} // RunningSumAdapterService::RunningSumAdapterService

RunningSumAdapterService::~RunningSumAdapterService(void)
{
    ODL_OBJENTER(); //####
    stopStreams();
    delete _controlHandler;
    delete _dataHandler;
    ODL_OBJEXIT(); //####
} // RunningSumAdapterService::~RunningSumAdapterService

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void
RunningSumAdapterService::disableMetrics(void)
{
    ODL_OBJENTER(); //####
    inherited::disableMetrics();
    if (_controlHandler)
    {
        _controlHandler->disableMetrics();
    }
    if (_dataHandler)
    {
        _dataHandler->disableMetrics();
    }
    ODL_OBJEXIT(); //####
} // RunningSumAdapterService::disableMetrics

void
RunningSumAdapterService::enableMetrics(void)
{
    ODL_OBJENTER(); //####
    inherited::enableMetrics();
    if (_controlHandler)
    {
        _controlHandler->enableMetrics();
    }
    if (_dataHandler)
    {
        _dataHandler->enableMetrics();
    }
    ODL_OBJEXIT(); //####
} // RunningSumAdapterService::enableMetrics

void
RunningSumAdapterService::setUpInputHandlers(RunningSumAdapterData & sharedData)
{
    ODL_OBJENTER(); //####
    ODL_P1("sharedData = ", &sharedData); //####
    if (! _controlHandler)
    {
        _controlHandler = new RunningSumControlInputHandler(sharedData);
    }
    if (! _dataHandler)
    {
        _dataHandler = new RunningSumDataInputHandler(sharedData);
    }
    ODL_OBJEXIT(); //####
} // RunningSumAdapterService::setUpInputHandlers

bool
RunningSumAdapterService::setUpStreamDescriptions(void)
{
    ODL_OBJENTER(); //####
    bool               result = true;
    ChannelDescription description;
    YarpString         rootName(getEndpoint().getName() + "/");

    _inDescriptions.clear();
    description._portName = rootName + "control";
    description._portProtocol = "s";
    description._protocolDescription = "Command strings";
    _inDescriptions.push_back(description);
    description._portName = rootName + "data";
    description._portProtocol = "d+";
    description._protocolDescription = "One or more numeric values";
    _inDescriptions.push_back(description);
    _outDescriptions.clear();
    description._portName = rootName + "output";
    description._portProtocol = "d";
    description._protocolDescription = "A floating-point value";
    _outDescriptions.push_back(description);
    _clientDescriptions.clear();
    description._portName = rootName + "client";
    description._portProtocol = "";
    description._protocolDescription = "";
    _clientDescriptions.push_back(description);
    ODL_OBJEXIT_B(result); //####
    return result;
} // RunningSumAdapterService::setUpStreamDescriptions

void
RunningSumAdapterService::startStreams(void)
{
    ODL_OBJENTER(); //####
    try
    {
        if (! isActive())
        {
            if (_controlHandler && _dataHandler)
            {
                _controlHandler->setChannel(getInletStream(0));
                getInletStream(0)->setReader(*_controlHandler);
                _dataHandler->setChannel(getInletStream(1));
                getInletStream(1)->setReader(*_dataHandler);
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
} // RunningSumAdapterService::startStreams

void
RunningSumAdapterService::stopStreams(void)
{
    ODL_OBJENTER(); //####
    try
    {
        if (isActive())
        {
            clearActive();
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT(); //####
} // RunningSumAdapterService::stopStreams

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
