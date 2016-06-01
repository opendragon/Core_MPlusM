//--------------------------------------------------------------------------------------------------
//
//  File:       m+mLeapTwoFingersInputListener.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for a Leap Two Fingers listener.
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
//  Created:    2015-02-23
//
//--------------------------------------------------------------------------------------------------

#include "m+mLeapTwoFingersInputListener.hpp"

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for a %Leap Two Fingers listener. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::LeapTwoFingers;

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

LeapTwoFingersInputListener::LeapTwoFingersInputListener(GeneralChannel * outChannel) :
    inherited(), _outChannel(outChannel)
{
    ODL_ENTER(); //####
    ODL_P1("outChannel = ", outChannel); //####
    ODL_EXIT_P(this); //####
} // LeapTwoFingersInputListener::LeapTwoFingersInputListener

LeapTwoFingersInputListener::~LeapTwoFingersInputListener(void)
{
    ODL_OBJENTER(); //####
    ODL_OBJEXIT(); //####
} // LeapTwoFingersInputListener::~LeapTwoFingersInputListener

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void
LeapTwoFingersInputListener::clearOutputChannel(void)
{
    ODL_OBJENTER(); //####
    _outChannel = NULL;
    ODL_OBJEXIT(); //####
} // LeapTwoFingersInputListener::clearOutputChannel

void
LeapTwoFingersInputListener::onConnect(const Leap::Controller & theController)
{
    ODL_OBJENTER(); //####
    ODL_P1("theController = ", &theController); //####
    //theController.setPolicyFlags(Leap::Controller::POLICY_DEFAULT);
    theController.setPolicyFlags(Leap::Controller::POLICY_BACKGROUND_FRAMES);
    ODL_OBJEXIT(); //####
} // LeapTwoFingersInputListener::onConnect

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
void
LeapTwoFingersInputListener::onDeviceChange(const Leap::Controller & theController)
{
#if (! defined(OD_ENABLE_LOGGING_))
# if MAC_OR_LINUX_
#  pragma unused(theController)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING_)
    ODL_OBJENTER(); //####
    ODL_P1("theController = ", &theController); //####
    ODL_OBJEXIT(); //####
} // LeapTwoFingersInputListener::onDeviceChange
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
void
LeapTwoFingersInputListener::onDisconnect(const Leap::Controller & theController)
{
#if (! defined(OD_ENABLE_LOGGING_))
# if MAC_OR_LINUX_
#  pragma unused(theController)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING_)
    ODL_OBJENTER(); //####
    ODL_P1("theController = ", &theController); //####
    ODL_OBJEXIT(); //####
} // LeapTwoFingersInputListener::onDisconnect
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
void
LeapTwoFingersInputListener::onExit(const Leap::Controller & theController)
{
#if (! defined(OD_ENABLE_LOGGING_))
# if MAC_OR_LINUX_
#  pragma unused(theController)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING_)
    ODL_OBJENTER(); //####
    ODL_P1("theController = ", &theController); //####
    ODL_OBJEXIT(); //####
} // LeapTwoFingersInputListener::onExit
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
void
LeapTwoFingersInputListener::onFocusGained(const Leap::Controller & theController)
{
#if (! defined(OD_ENABLE_LOGGING_))
# if MAC_OR_LINUX_
#  pragma unused(theController)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING_)
    ODL_OBJENTER(); //####
    ODL_P1("theController = ", &theController); //####
    ODL_OBJEXIT(); //####
} // LeapTwoFingersInputListener::onFocusGained
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
void
LeapTwoFingersInputListener::onFocusLost(const Leap::Controller & theController)
{
#if (! defined(OD_ENABLE_LOGGING_))
# if MAC_OR_LINUX_
#  pragma unused(theController)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING_)
    ODL_OBJENTER(); //####
    ODL_P1("theController = ", &theController); //####
    ODL_OBJEXIT(); //####
} // LeapTwoFingersInputListener::onFocusLost
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

void
LeapTwoFingersInputListener::onFrame(const Leap::Controller & theController)
{
    ODL_OBJENTER(); //####
    ODL_P1("theController = ", &theController); //####
    Leap::Frame latestFrame(theController.frame());

    if (latestFrame.isValid())
    {
        Leap::HandList hands(latestFrame.hands());
        int            handCount = hands.count();

        if (0 < handCount)
        {
            HandMask         handsPresent = kNoHands;
            double           fingerPositions[6];
            yarp::os::Bottle message;
            
            memset(fingerPositions, 0, sizeof(fingerPositions));
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
                        // fingers
                        Leap::FingerList fingers(aHand.fingers());

                        for (Leap::FingerList::const_iterator fingerWalker(fingers.begin());
                             fingers.end() != fingerWalker; ++fingerWalker)
                        {
                            Leap::Finger aFinger(*fingerWalker);

                            if (aFinger.isValid())
                            {
                                const Leap::Vector & position = aFinger.tipPosition();

                                fingerPositions[offset] = position.x;
                                fingerPositions[offset + 1] = position.y;
                                fingerPositions[offset + 2] = position.z;
                                handsPresent = static_cast<HandMask>(handsPresent | thisHand);
                                break;
                            }

                        }
                    }
                }
            }
            message.addInt(handsPresent);
            for (int ii = 0; ii < (sizeof(fingerPositions) / sizeof(*fingerPositions)); ++ii)
            {
                message.addDouble(fingerPositions[ii]);
            }
            if (_outChannel)
            {
                if (! _outChannel->write(message))
                {
                    ODL_LOG("(! _outChannel->write(message))"); //####
#if defined(MpM_StallOnSendProblem)
                    Stall();
#endif // defined(MpM_StallOnSendProblem)
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
} // LeapTwoFingersInputListener::onFrame

void
LeapTwoFingersInputListener::onInit(const Leap::Controller & theController)
{
    ODL_OBJENTER(); //####
    ODL_P1("theController = ", &theController); //####
    theController.setPolicyFlags(Leap::Controller::POLICY_BACKGROUND_FRAMES);
    ODL_OBJEXIT(); //####
} // LeapTwoFingersInputListener::onInit

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
void
LeapTwoFingersInputListener::onServiceConnect(const Leap::Controller & theController)
{
#if (! defined(OD_ENABLE_LOGGING_))
# if MAC_OR_LINUX_
#  pragma unused(theController)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING_)
    ODL_OBJENTER(); //####
    ODL_P1("theController = ", &theController); //####
    ODL_OBJEXIT(); //####
} // LeapTwoFingersInputListener::onServiceConnect
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
void
LeapTwoFingersInputListener::onServiceDisconnect(const Leap::Controller & theController)
{
#if (! defined(OD_ENABLE_LOGGING_))
# if MAC_OR_LINUX_
#  pragma unused(theController)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING_)
    ODL_OBJENTER(); //####
    ODL_P1("theController = ", &theController); //####
    ODL_OBJEXIT(); //####
} // LeapTwoFingersInputListener::onServiceDisconnect
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
