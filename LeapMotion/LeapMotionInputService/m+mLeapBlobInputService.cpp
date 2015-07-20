//--------------------------------------------------------------------------------------------------
//
//  File:       m+mLeapBlobInputService.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the Leap Blob input service.
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
//  Created:    2015-06-24
//
//--------------------------------------------------------------------------------------------------

#include "m+mLeapBlobInputService.h"

#include "m+mLeapBlobInputListener.h"
#include "m+mLeapBlobInputRequests.h"

#include <m+m/m+mEndpoint.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the %Leap %Blob input service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::LeapBlob;

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

LeapBlobInputService::LeapBlobInputService(const Utilities::DescriptorVector & argumentList,
                                           const YarpString &                  launchPath,
                                           const int                           argc,
                                           char * *                            argv,
                                           const YarpString &                  tag,
                                           const YarpString &                  serviceEndpointName,
                                           const YarpString &                  servicePortNumber) :
    inherited(argumentList, launchPath, argc, argv, tag, true, MpM_LEAPBLOBINPUT_CANONICAL_NAME_,
              LEAPBLOBINPUT_SERVICE_DESCRIPTION_, "", serviceEndpointName, servicePortNumber),
    _translationScale(1), _controller(new Leap::Controller), _listener(NULL)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("argumentList = ", &argumentList, "argv = ", argv); //####
    OD_LOG_S4s("launchPath = ", launchPath, "tag = ", tag, "serviceEndpointName = ", //####
               serviceEndpointName, "servicePortNumber = ", servicePortNumber); //####
    OD_LOG_LL1("argc = ", argc); //####
    OD_LOG_EXIT_P(this); //####
} // LeapBlobInputService::LeapBlobInputService

LeapBlobInputService::~LeapBlobInputService(void)
{
    OD_LOG_OBJENTER(); //####
    stopStreams();
    if (_controller)
    {
        delete _controller;
        _controller = NULL;
    }
    OD_LOG_OBJEXIT(); //####
} // LeapBlobInputService::~LeapBlobInputService

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

bool LeapBlobInputService::configure(const yarp::os::Bottle & details)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("details = ", &details); //####
    bool result = false;
    
    try
    {
        if (1 == details.size())
        {
            yarp::os::Value firstValue(details.get(0));
            
            if (firstValue.isDouble() || firstValue.isInt())
            {
                std::stringstream buff;
                
                if (firstValue.isDouble())
                {
                    _translationScale = firstValue.asDouble();
                }
                else
                {
                    _translationScale = firstValue.asInt();
                }
                buff << "Translation scale is " << _translationScale;
                setExtraInformation(buff.str());
                result = true;
            }
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // LeapBlobInputService::configure

bool LeapBlobInputService::getConfiguration(yarp::os::Bottle & details)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("details = ", &details); //####
    bool result = true;

    details.clear();
    details.addDouble(_translationScale);
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // LeapBlobInputService::getConfiguration

void LeapBlobInputService::restartStreams(void)
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
} // LeapBlobInputService::restartStreams

bool LeapBlobInputService::setUpStreamDescriptions(void)
{
    OD_LOG_OBJENTER(); //####
    bool               result = true;
    ChannelDescription description;
    YarpString         rootName(getEndpoint().getName() + "/");
    
    _outDescriptions.clear();
    description._portName = rootName + "output";
    description._portProtocol = "b";
    description._protocolDescription = T_("A binary blob containing the finger positions and "
                                          "directions");
    _outDescriptions.push_back(description);
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // LeapBlobInputService::setUpStreamDescriptions

bool LeapBlobInputService::shutDownOutputStreams(void)
{
    OD_LOG_OBJENTER(); //####
    bool result = inherited::shutDownOutputStreams();
    
    if (_listener)
    {
        _listener->clearOutputChannel();
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // LeapBlobInputService::shutDownOutputStreams

bool LeapBlobInputService::start(void)
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
} // LeapBlobInputService::start

void LeapBlobInputService::startStreams(void)
{
    OD_LOG_OBJENTER(); //####
    try
    {
        if (! isActive())
        {
            if (_controller)
            {
                _listener = new LeapBlobInputListener(getOutletStream(0));
                _listener->setScale(_translationScale);
                _controller->addListener(*_listener);
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
} // LeapBlobInputService::startStreams

bool LeapBlobInputService::stop(void)
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
} // LeapBlobInputService::stop

void LeapBlobInputService::stopStreams(void)
{
    OD_LOG_OBJENTER(); //####
    try
    {
        if (isActive())
        {
            if (_controller && _listener)
            {
                _controller->removeListener(*_listener);
                delete _listener;
                _listener = NULL;
            }
            clearActive();
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT(); //####
} // LeapBlobInputService::stopStreams

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
