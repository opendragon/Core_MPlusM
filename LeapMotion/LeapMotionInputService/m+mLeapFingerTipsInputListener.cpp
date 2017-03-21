//--------------------------------------------------------------------------------------------------
//
//  File:       m+mLeapFingerTipsInputListener.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for a Leap Finger Tips listener.
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

#include "m+mLeapFingerTipsInputListener.hpp"

//#include <ODEnableLogging.h>
#include <ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for a %Leap Finger Tips listener. */
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

/*! @brief A bitmask for what data is present in a message. */
enum FingerMask
{
    /*! @brief No data for any fingers are present. */
    kNoFingers = 0x0000,
    
    /*! @brief The mask for the left hand values. */
    kLeftMask = 0x003F,
    
    /*! @brief The mask for the right hand values. */
    kRightMask = 0x0FC0,
    
    /*! @brief The number of positions to shift the finger values to the left for the left hand. */
    kLeftShift = 0,
    
    /*! @brief The number of positions to shift the finger values to the left for the right hand. */
    kRightShift = 6,

    /*! @brief Data for the palm is present. */
    kPalm = 0x0001,

    /*! @brief Data for the thumb is present. */
    kThumb = 0x0002,

    /*! @brief Data for the index finger is present. */
    kIndex = 0x0004,
    
    /*! @brief Data for the middle finger is present. */
    kMiddle = 0x0008,
    
    /*! @brief Data for the ring finger is present. */
    kRing = 0x0010,
    
    /*! @brief Data for the pinky is present. */
    kPinky = 0x0020
    
}; // FingerMask

/*! @brief The number of values for each finger. */
static const int kValuesPerFinger = 3; // 3D positions

/*! @brief The number of values per hand. */
static const int kValuesPerHand = (kValuesPerFinger * 6);

/*! @brief The total number of values. */
static const int kTotalValues = (kValuesPerHand * 2);

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

LeapFingerTipsInputListener::LeapFingerTipsInputListener(GeneralChannel * outChannel) :
    inherited(), _outChannel(outChannel)
{
    ODL_ENTER(); //####
    ODL_P1("outChannel = ", outChannel); //####
    ODL_EXIT_P(this); //####
} // LeapFingerTipsInputListener::LeapFingerTipsInputListener

LeapFingerTipsInputListener::~LeapFingerTipsInputListener(void)
{
    ODL_OBJENTER(); //####
    ODL_OBJEXIT(); //####
} // LeapFingerTipsInputListener::~LeapFingerTipsInputListener

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void
LeapFingerTipsInputListener::clearOutputChannel(void)
{
    ODL_OBJENTER(); //####
    _outChannel = NULL;
    ODL_OBJEXIT(); //####
} // LeapFingerTipsInputListener::clearOutputChannel

void
LeapFingerTipsInputListener::onConnect(const Leap::Controller & theController)
{
    ODL_OBJENTER(); //####
    ODL_P1("theController = ", &theController); //####
    //theController.setPolicyFlags(Leap::Controller::POLICY_DEFAULT);
    theController.setPolicyFlags(Leap::Controller::POLICY_BACKGROUND_FRAMES);
    ODL_OBJEXIT(); //####
} // LeapFingerTipsInputListener::onConnect

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
void
LeapFingerTipsInputListener::onDeviceChange(const Leap::Controller & theController)
{
#if (! defined(OD_ENABLE_LOGGING_))
# if MAC_OR_LINUX_
#  pragma unused(theController)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING_)
    ODL_OBJENTER(); //####
    ODL_P1("theController = ", &theController); //####
    ODL_OBJEXIT(); //####
} // LeapFingerTipsInputListener::onDeviceChange
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
void
LeapFingerTipsInputListener::onDisconnect(const Leap::Controller & theController)
{
#if (! defined(OD_ENABLE_LOGGING_))
# if MAC_OR_LINUX_
#  pragma unused(theController)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING_)
    ODL_OBJENTER(); //####
    ODL_P1("theController = ", &theController); //####
    ODL_OBJEXIT(); //####
} // LeapFingerTipsInputListener::onDisconnect
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
void
LeapFingerTipsInputListener::onExit(const Leap::Controller & theController)
{
#if (! defined(OD_ENABLE_LOGGING_))
# if MAC_OR_LINUX_
#  pragma unused(theController)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING_)
    ODL_OBJENTER(); //####
    ODL_P1("theController = ", &theController); //####
    ODL_OBJEXIT(); //####
} // LeapFingerTipsInputListener::onExit
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
void
LeapFingerTipsInputListener::onFocusGained(const Leap::Controller & theController)
{
#if (! defined(OD_ENABLE_LOGGING_))
# if MAC_OR_LINUX_
#  pragma unused(theController)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING_)
    ODL_OBJENTER(); //####
    ODL_P1("theController = ", &theController); //####
    ODL_OBJEXIT(); //####
} // LeapFingerTipsInputListener::onFocusGained
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
void
LeapFingerTipsInputListener::onFocusLost(const Leap::Controller & theController)
{
#if (! defined(OD_ENABLE_LOGGING_))
# if MAC_OR_LINUX_
#  pragma unused(theController)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING_)
    ODL_OBJENTER(); //####
    ODL_P1("theController = ", &theController); //####
    ODL_OBJEXIT(); //####
} // LeapFingerTipsInputListener::onFocusLost
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

void
LeapFingerTipsInputListener::onFrame(const Leap::Controller & theController)
{
    ODL_OBJENTER(); //####
    ODL_P1("theController = ", &theController); //####
    Leap::Frame latestFrame(theController.frame());

    if (latestFrame.isValid())
    {
        Leap::HandList   hands(latestFrame.hands());
        double           fingerPositions[kTotalValues];
        FingerMask       fingersPresent = kNoFingers;
        int              handCount = hands.count();
        yarp::os::Bottle message;

        memset(fingerPositions, 0, sizeof(fingerPositions));
        if (0 < handCount)
        {
            for (Leap::HandList::const_iterator handWalker(hands.begin());
                 hands.end() != handWalker; ++handWalker)
            {
                Leap::Hand aHand(*handWalker);

                if (aHand.isValid())
                {
                    int baseOffset;
                    int bitShift;

                    if (aHand.isLeft())
                    {
                        baseOffset = 0;
                        bitShift = kLeftShift;
                    }
                    else if (aHand.isRight())
                    {
                        baseOffset = kValuesPerHand;
                        bitShift = kRightShift;
                    }
                    else
                    {
                        baseOffset = -1;
                        bitShift = -1;
                    }
                    if (0 <= baseOffset)
                    {
                        // fingers
                        Leap::FingerList fingers(aHand.fingers());

                        fingerPositions[baseOffset] = aHand.stabilizedPalmPosition().x;
                        fingerPositions[baseOffset + 1] = aHand.stabilizedPalmPosition().y;
                        fingerPositions[baseOffset + 2] = aHand.stabilizedPalmPosition().z;
                        fingersPresent = static_cast<FingerMask>(fingersPresent |
                                                                 (kPalm << bitShift));
                        for (Leap::FingerList::const_iterator fingerWalker(fingers.begin());
                             fingers.end() != fingerWalker; ++fingerWalker)
                        {
                            Leap::Finger aFinger(*fingerWalker);

                            if (aFinger.isValid())
                            {
                                const Leap::Vector & position = aFinger.stabilizedTipPosition();
                                int                  fingerBit = 0;
                                int                  fingerOffset = 0;
                                
                                switch (aFinger.type())
                                {
                                    case Leap::Finger::TYPE_THUMB :
                                        fingerOffset = (1 * kValuesPerFinger);
                                        fingerBit = kThumb;
                                        break;
                                        
                                    case Leap::Finger::TYPE_INDEX :
                                        fingerOffset = (2 * kValuesPerFinger);
                                        fingerBit = kIndex;
                                        break;
                                        
                                    case Leap::Finger::TYPE_MIDDLE :
                                        fingerOffset = (3 * kValuesPerFinger);
                                        fingerBit = kMiddle;
                                        break;
                                        
                                    case Leap::Finger::TYPE_RING :
                                        fingerOffset = (4 * kValuesPerFinger);
                                        fingerBit = kRing;
                                        break;
                                        
                                    case Leap::Finger::TYPE_PINKY :
                                        fingerOffset = (5 * kValuesPerFinger);
                                        fingerBit = kPinky;
                                        break;

                                    default:
                                        fingerOffset = fingerBit = -1;
                                        break;

                                }
                                if (0 <= fingerBit)
                                {
                                    fingerPositions[baseOffset + fingerOffset] = position.x;
                                    fingerPositions[baseOffset + fingerOffset + 1] = position.y;
                                    fingerPositions[baseOffset + fingerOffset + 2] = position.z;
                                    fingersPresent = static_cast<FingerMask>(fingersPresent |
                                                                         (fingerBit << bitShift));
                                }
                            }
                        }
                    }
                }
            }
        }
        else
        {
            ODL_LOG("! (0 < handCount)"); //####
        }
        message.addInt(fingersPresent);
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
        ODL_LOG("! (latestFrame.isValid())"); //####
    }
    ODL_OBJEXIT(); //####
} // LeapFingerTipsInputListener::onFrame

void
LeapFingerTipsInputListener::onInit(const Leap::Controller & theController)
{
    ODL_OBJENTER(); //####
    ODL_P1("theController = ", &theController); //####
    theController.setPolicyFlags(Leap::Controller::POLICY_BACKGROUND_FRAMES);
    ODL_OBJEXIT(); //####
} // LeapFingerTipsInputListener::onInit

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
void
LeapFingerTipsInputListener::onServiceConnect(const Leap::Controller & theController)
{
#if (! defined(OD_ENABLE_LOGGING_))
# if MAC_OR_LINUX_
#  pragma unused(theController)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING_)
    ODL_OBJENTER(); //####
    ODL_P1("theController = ", &theController); //####
    ODL_OBJEXIT(); //####
} // LeapFingerTipsInputListener::onServiceConnect
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
void
LeapFingerTipsInputListener::onServiceDisconnect(const Leap::Controller & theController)
{
#if (! defined(OD_ENABLE_LOGGING_))
# if MAC_OR_LINUX_
#  pragma unused(theController)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING_)
    ODL_OBJENTER(); //####
    ODL_P1("theController = ", &theController); //####
    ODL_OBJEXIT(); //####
} // LeapFingerTipsInputListener::onServiceDisconnect
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
