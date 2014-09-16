//--------------------------------------------------------------------------------------------------
//
//  File:       M+MLeapMotionInputListener.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for a Leap Motion listener.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2014 by HPlus Technologies Ltd. and Simon Fraser University.
//
//              All rights reserved. Redistribution and use in source and binary forms, with or
//              without modification, are permitted provided that the following conditions are met:
//                * Redistributions of source code must retain the above copyright notice, this list
//                  of conditions and the following disclaimer.
//                * Redistributions in binary form must reproduce the above copyright notice, this
//                  list of conditions and the following disclaimer in the documentation and/or
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
//  Created:    2014-09-16
//
//--------------------------------------------------------------------------------------------------

#include <mpm/M+MLeapMotionInputListener.h>

#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The class definition for a Leap Motion listener. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;

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
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

LeapMotionInputListener::LeapMotionInputListener(void) :
    inherited()
{
    OD_LOG_ENTER(); //####
    OD_LOG_EXIT_P(this); //####
} // LeapMotionInputListener::LeapMotionInputListener

LeapMotionInputListener::~LeapMotionInputListener(void)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_OBJEXIT(); //####
} // LeapMotionInputListener::~LeapMotionInputListener

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void LeapMotionInputListener::onConnect(const Leap::Controller & theController)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("theController = ", theController); //####
    OD_LOG_OBJEXIT(); //####
} // LeapMotionInputListener::onConnect

void LeapMotionInputListener::onDeviceChange(const Leap::Controller & theController)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("theController = ", theController); //####
    OD_LOG_OBJEXIT(); //####
} // LeapMotionInputListener::onDeviceChange

void LeapMotionInputListener::onDisconnect(const Leap::Controller & theController)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("theController = ", theController); //####
    OD_LOG_OBJEXIT(); //####
} // LeapMotionInputListener::onDisconnect

void LeapMotionInputListener::onExit(const Leap::Controller & theController)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("theController = ", theController); //####
    OD_LOG_OBJEXIT(); //####
} // LeapMotionInputListener::onExit

void LeapMotionInputListener::onFocusGained(const Leap::Controller & theController)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("theController = ", theController); //####
    OD_LOG_OBJEXIT(); //####
} // LeapMotionInputListener::onFocusGained

void LeapMotionInputListener::onFocusLost(const Leap::Controller & theController)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("theController = ", theController); //####
    OD_LOG_OBJEXIT(); //####
} // LeapMotionInputListener::onFocusLost

void LeapMotionInputListener::onFrame(const Leap::Controller & theController)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("theController = ", theController); //####
    OD_LOG_OBJEXIT(); //####
} // LeapMotionInputListener::onFrame

void LeapMotionInputListener::onInit(const Leap::Controller & theController)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("theController = ", theController); //####
    OD_LOG_OBJEXIT(); //####
} // LeapMotionInputListener::onInit

void LeapMotionInputListener::onServiceConnect(const Leap::Controller & theController)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("theController = ", theController); //####
    OD_LOG_OBJEXIT(); //####
} // LeapMotionInputListener::onServiceConnect

void LeapMotionInputListener::onServiceDisconnect(const Leap::Controller & theController)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("theController = ", theController); //####
    OD_LOG_OBJEXIT(); //####
} // LeapMotionInputListener::onServiceDisconnect

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
