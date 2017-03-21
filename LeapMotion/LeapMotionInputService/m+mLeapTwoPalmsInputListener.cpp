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

#include "m+mLeapTwoPalmsInputListener.hpp"

//#include <odlEnable.h>
#include <odlInclude.h>

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
    ODL_ENTER(); //####
    ODL_P1("outChannel = ", outChannel); //####
    ODL_EXIT_P(this); //####
} // LeapTwoPalmsInputListener::LeapTwoPalmsInputListener

LeapTwoPalmsInputListener::~LeapTwoPalmsInputListener(void)
{
    ODL_OBJENTER(); //####
    ODL_OBJEXIT(); //####
} // LeapTwoPalmsInputListener::~LeapTwoPalmsInputListener

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void
LeapTwoPalmsInputListener::clearOutputChannel(void)
{
    ODL_OBJENTER(); //####
    _outChannel = NULL;
    ODL_OBJEXIT(); //####
} // LeapTwoPalmsInputListener::clearOutputChannel

void
LeapTwoPalmsInputListener::onConnect(const Leap::Controller & theController)
{
    ODL_OBJENTER(); //####
    ODL_P1("theController = ", &theController); //####
    //theController.setPolicyFlags(Leap::Controller::POLICY_DEFAULT);
    theController.setPolicyFlags(Leap::Controller::POLICY_BACKGROUND_FRAMES);
    ODL_OBJEXIT(); //####
} // LeapTwoPalmsInputListener::onConnect

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
void
LeapTwoPalmsInputListener::onDeviceChange(const Leap::Controller & theController)
{
#if (! defined(OD_ENABLE_LOGGING_))
# if MAC_OR_LINUX_
#  pragma unused(theController)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING_)
    ODL_OBJENTER(); //####
    ODL_P1("theController = ", &theController); //####
    ODL_OBJEXIT(); //####
} // LeapTwoPalmsInputListener::onDeviceChange
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
void
LeapTwoPalmsInputListener::onDisconnect(const Leap::Controller & theController)
{
#if (! defined(OD_ENABLE_LOGGING_))
# if MAC_OR_LINUX_
#  pragma unused(theController)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING_)
    ODL_OBJENTER(); //####
    ODL_P1("theController = ", &theController); //####
    ODL_OBJEXIT(); //####
} // LeapTwoPalmsInputListener::onDisconnect
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
void
LeapTwoPalmsInputListener::onExit(const Leap::Controller & theController)
{
#if (! defined(OD_ENABLE_LOGGING_))
# if MAC_OR_LINUX_
#  pragma unused(theController)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING_)
    ODL_OBJENTER(); //####
    ODL_P1("theController = ", &theController); //####
    ODL_OBJEXIT(); //####
} // LeapTwoPalmsInputListener::onExit
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
void
LeapTwoPalmsInputListener::onFocusGained(const Leap::Controller & theController)
{
#if (! defined(OD_ENABLE_LOGGING_))
# if MAC_OR_LINUX_
#  pragma unused(theController)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING_)
    ODL_OBJENTER(); //####
    ODL_P1("theController = ", &theController); //####
    ODL_OBJEXIT(); //####
} // LeapTwoPalmsInputListener::onFocusGained
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
void
LeapTwoPalmsInputListener::onFocusLost(const Leap::Controller & theController)
{
#if (! defined(OD_ENABLE_LOGGING_))
# if MAC_OR_LINUX_
#  pragma unused(theController)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING_)
    ODL_OBJENTER(); //####
    ODL_P1("theController = ", &theController); //####
    ODL_OBJEXIT(); //####
} // LeapTwoPalmsInputListener::onFocusLost
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

void
LeapTwoPalmsInputListener::onFrame(const Leap::Controller & theController)
{
    ODL_OBJENTER(); //####
    ODL_P1("theController = ", &theController); //####
    Leap::Frame latestFrame(theController.frame());

    if (latestFrame.isValid())
    {
        Leap::HandList   hands(latestFrame.hands());
        HandMask         handsPresent = kNoHands;
        int              handCount = hands.count();
        yarp::os::Bottle message;
        double           palmNormals[6];
        double           palmPositions[6];
        double           palmVelocities[6];
        
        memset(palmNormals, 0, sizeof(palmNormals));
        memset(palmPositions, 0, sizeof(palmPositions));
        memset(palmVelocities, 0, sizeof(palmVelocities));
        if (0 < handCount)
        {
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
                        const Leap::Vector & position = aHand.stabilizedPalmPosition();
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
        }
        else
        {
            ODL_LOG("! (0 < handCount)"); //####
        }
    }
    else
    {
        ODL_LOG("! (latestFrame.isValid())"); //####
    }
    ODL_OBJEXIT(); //####
} // LeapTwoPalmsInputListener::onFrame

void
LeapTwoPalmsInputListener::onInit(const Leap::Controller & theController)
{
    ODL_OBJENTER(); //####
    ODL_P1("theController = ", &theController); //####
    theController.setPolicyFlags(Leap::Controller::POLICY_BACKGROUND_FRAMES);
    ODL_OBJEXIT(); //####
} // LeapTwoPalmsInputListener::onInit

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
void
LeapTwoPalmsInputListener::onServiceConnect(const Leap::Controller & theController)
{
#if (! defined(OD_ENABLE_LOGGING_))
# if MAC_OR_LINUX_
#  pragma unused(theController)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING_)
    ODL_OBJENTER(); //####
    ODL_P1("theController = ", &theController); //####
    ODL_OBJEXIT(); //####
} // LeapTwoPalmsInputListener::onServiceConnect
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
void
LeapTwoPalmsInputListener::onServiceDisconnect(const Leap::Controller & theController)
{
#if (! defined(OD_ENABLE_LOGGING_))
# if MAC_OR_LINUX_
#  pragma unused(theController)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING_)
    ODL_OBJENTER(); //####
    ODL_P1("theController = ", &theController); //####
    ODL_OBJEXIT(); //####
} // LeapTwoPalmsInputListener::onServiceDisconnect
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
