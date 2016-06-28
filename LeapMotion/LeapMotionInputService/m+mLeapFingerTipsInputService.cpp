//--------------------------------------------------------------------------------------------------
//
//  File:       m+mLeapFingerTipsInputService.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the Leap Finger Tips input service.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2016 by OpenDragon.
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
//  Created:    2016-05-15
//
//--------------------------------------------------------------------------------------------------

#include "m+mLeapFingerTipsInputService.hpp"
#include "m+mLeapFingerTipsInputListener.hpp"
#include "m+mLeapFingerTipsInputRequests.hpp"

#include <m+m/m+mEndpoint.hpp>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the %Leap Finger Tips input service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::LeapFingerTips;

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

LeapFingerTipsInputService::LeapFingerTipsInputService(const Utilities::DescriptorVector &
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
              MpM_LEAPFINGERTIPSINPUT_CANONICAL_NAME_, LEAPFINGERTIPSINPUT_SERVICE_DESCRIPTION_, "",
              serviceEndpointName, servicePortNumber), _controller(new Leap::Controller),
    _listener(NULL)
{
    ODL_ENTER(); //####
    ODL_P2("argumentList = ", &argumentList, "argv = ", argv); //####
    ODL_S4s("launchPath = ", launchPath, "tag = ", tag, "serviceEndpointName = ", //####
            serviceEndpointName, "servicePortNumber = ", servicePortNumber); //####
    ODL_LL1("argc = ", argc); //####
    ODL_EXIT_P(this); //####
} // LeapFingerTipsInputService::LeapFingerTipsInputService

LeapFingerTipsInputService::~LeapFingerTipsInputService(void)
{
    ODL_OBJENTER(); //####
    stopStreams();
    if (_controller)
    {
        delete _controller;
        _controller = NULL;
    }
    ODL_OBJEXIT(); //####
} // LeapFingerTipsInputService::~LeapFingerTipsInputService

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

bool
LeapFingerTipsInputService::setUpStreamDescriptions(void)
{
    ODL_OBJENTER(); //####
    bool               result = true;
    ChannelDescription description;
    YarpString         rootName(getEndpoint().getName() + "/");

    _outDescriptions.clear();
    description._portName = rootName + "output";
    description._portProtocol = "FINGERTIPS";
    description._protocolDescription = T_("A flag for which fingers have data followed by the tip "
                                          "positions\nof the each finger of each hand, if "
                                          "present");
    _outDescriptions.push_back(description);
    ODL_OBJEXIT_B(result); //####
    return result;
} // LeapFingerTipsInputService::setUpStreamDescriptions

bool
LeapFingerTipsInputService::shutDownOutputStreams(void)
{
    ODL_OBJENTER(); //####
    bool result = inherited::shutDownOutputStreams();

    if (_listener)
    {
        _listener->clearOutputChannel();
    }
    ODL_EXIT_B(result); //####
    return result;
} // LeapFingerTipsInputService::shutDownOutputStreams

void
LeapFingerTipsInputService::startStreams(void)
{
    ODL_OBJENTER(); //####
    try
    {
        if (! isActive())
        {
            if (_controller)
            {
                _listener = new LeapFingerTipsInputListener(getOutletStream(0));
                _controller->addListener(*_listener);
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
} // LeapFingerTipsInputService::startStreams

void
LeapFingerTipsInputService::stopStreams(void)
{
    ODL_OBJENTER(); //####
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
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT(); //####
} // LeapFingerTipsInputService::stopStreams

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)