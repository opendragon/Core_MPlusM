//--------------------------------------------------------------------------------------------------
//
//  File:       m+mLeapTwoPalmsInputListener.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for a Leap Two Palms listener.
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
//  Created:    2015-07-08
//
//--------------------------------------------------------------------------------------------------

#include "m+mLeapTwoPalmsInputListener.h"

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for a %Leap Two Palms listener. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::LeapTwoPalms;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief A bitmask for what data is present in a message. */
enum HandMask
{
    /*! @brief No data for either hand is present. */
    kNoHands   = 0x00,
    
    /*! @brief Data for the left hand is present. */
    kLeftHand  = 0x01,
    
    /*! @brief Data for the right hand is present. */
    kRightHand = 0x02
    
}; // HandMask

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

LeapTwoPalmsInputListener::LeapTwoPalmsInputListener(GeneralChannel * outChannel) :
    inherited(), _outChannel(outChannel)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P1("outChannel = ", outChannel); //####
    OD_LOG_EXIT_P(this); //####
} // LeapTwoPalmsInputListener::LeapTwoPalmsInputListener

LeapTwoPalmsInputListener::~LeapTwoPalmsInputListener(void)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_OBJEXIT(); //####
} // LeapTwoPalmsInputListener::~LeapTwoPalmsInputListener

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void LeapTwoPalmsInputListener::clearOutputChannel(void)
{
    OD_LOG_OBJENTER(); //####
    _outChannel = NULL;
    OD_LOG_OBJEXIT(); //####
} // LeapTwoPalmsInputListener::clearOutputChannel

void LeapTwoPalmsInputListener::onConnect(const Leap::Controller & theController)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("theController = ", &theController); //####
    theController.setPolicyFlags(Leap::Controller::POLICY_DEFAULT);
    OD_LOG_OBJEXIT(); //####
} // LeapTwoPalmsInputListener::onConnect

void LeapTwoPalmsInputListener::onDeviceChange(const Leap::Controller & theController)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("theController = ", &theController); //####
    OD_LOG_OBJEXIT(); //####
} // LeapTwoPalmsInputListener::onDeviceChange

void LeapTwoPalmsInputListener::onDisconnect(const Leap::Controller & theController)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("theController = ", &theController); //####
    OD_LOG_OBJEXIT(); //####
} // LeapTwoPalmsInputListener::onDisconnect

void LeapTwoPalmsInputListener::onExit(const Leap::Controller & theController)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("theController = ", &theController); //####
    OD_LOG_OBJEXIT(); //####
} // LeapTwoPalmsInputListener::onExit

void LeapTwoPalmsInputListener::onFocusGained(const Leap::Controller & theController)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("theController = ", &theController); //####
    OD_LOG_OBJEXIT(); //####
} // LeapTwoPalmsInputListener::onFocusGained

void LeapTwoPalmsInputListener::onFocusLost(const Leap::Controller & theController)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("theController = ", &theController); //####
    OD_LOG_OBJEXIT(); //####
} // LeapTwoPalmsInputListener::onFocusLost

void LeapTwoPalmsInputListener::onFrame(const Leap::Controller & theController)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("theController = ", &theController); //####
    Leap::Frame latestFrame(theController.frame());
    
    if (latestFrame.isValid())
    {
        Leap::HandList hands(latestFrame.hands());
        int            handCount = hands.count();
        
        if (0 < handCount)
        {
            HandMask handsPresent = kNoHands;
            double   palmNormals[6];
            double   palmPositions[6];
            double   palmVelocities[6];
            
            memset(palmNormals, 0, sizeof(palmNormals));
            memset(palmPositions, 0, sizeof(palmPositions));
            memset(palmVelocities, 0, sizeof(palmVelocities));
            for (Leap::HandList::const_iterator handWalker(hands.begin());
                 hands.end() != handWalker; ++handWalker)
            {
                Leap::Hand aHand(*handWalker);
                
                if (aHand.isValid())
                {
                    HandMask thisHand;
                    int      offset;
                    
                    if (aHand.isLeft())
                    {
                        offset = 0;
                        thisHand = kLeftHand;
                    }
                    else if (aHand.isRight())
                    {
                        offset = 3;
                        thisHand = kRightHand;
                    }
                    else
                    {
                        offset = -1;
                        thisHand = kNoHands;
                    }
                    if (0 <= offset)
                    {
                        const Leap::Vector & normal = aHand.palmNormal();
                        const Leap::Vector & position = aHand.palmPosition();
                        const Leap::Vector & velocities = aHand.palmVelocity();
                        
                        palmNormals[offset] = normal.x;
                        palmNormals[offset + 1] = normal.y;
                        palmNormals[offset + 2] = normal.z;
                        palmPositions[offset] = position.x;
                        palmPositions[offset + 1] = position.y;
                        palmPositions[offset + 2] = position.z;
                        palmVelocities[offset] = velocities.x;
                        palmVelocities[offset + 1] = velocities.y;
                        palmVelocities[offset + 2] = velocities.z;
                        handsPresent = static_cast<HandMask>(handsPresent | thisHand);
                    }
                }
            }
            if (kNoHands != handsPresent)
            {
                yarp::os::Bottle message;
                
                message.addInt(handsPresent);
                for (int ii = 0; ii < (sizeof(palmPositions) / sizeof(*palmPositions)); ++ii)
                {
                    message.addDouble(palmPositions[ii]);
                }
                for (int ii = 0; ii < (sizeof(palmNormals) / sizeof(*palmNormals)); ++ii)
                {
                    message.addDouble(palmNormals[ii]);
                }
                for (int ii = 0; ii < (sizeof(palmVelocities) / sizeof(*palmVelocities)); ++ii)
                {
                    message.addDouble(palmVelocities[ii]);
                }
                if (_outChannel)
                {
                    if (! _outChannel->write(message))
                    {
                        OD_LOG("(! _outChannel->write(message))"); //####
#if defined(MpM_StallOnSendProblem)
                        Stall();
#endif // defined(MpM_StallOnSendProblem)
                    }
                }
            }
        }
    }
    OD_LOG_OBJEXIT(); //####
} // LeapTwoPalmsInputListener::onFrame

void LeapTwoPalmsInputListener::onInit(const Leap::Controller & theController)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("theController = ", &theController); //####
	theController.setPolicyFlags(Leap::Controller::POLICY_BACKGROUND_FRAMES);
    OD_LOG_OBJEXIT(); //####
} // LeapTwoPalmsInputListener::onInit

void LeapTwoPalmsInputListener::onServiceConnect(const Leap::Controller & theController)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("theController = ", &theController); //####
    OD_LOG_OBJEXIT(); //####
} // LeapTwoPalmsInputListener::onServiceConnect

void LeapTwoPalmsInputListener::onServiceDisconnect(const Leap::Controller & theController)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("theController = ", &theController); //####
    OD_LOG_OBJEXIT(); //####
} // LeapTwoPalmsInputListener::onServiceDisconnect

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)