//--------------------------------------------------------------------------------------------------
//
//  File:       M+MLeapBlobInputListener.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for a Leap Motion listener.
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

#include "M+MLeapBlobInputListener.h"

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for a %Leap Blob listener. */
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

LeapBlobInputListener::LeapBlobInputListener(GeneralChannel * outChannel) :
    inherited(), _outChannel(outChannel)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P1("outChannel = ", outChannel); //####
    OD_LOG_EXIT_P(this); //####
} // LeapBlobInputListener::LeapBlobInputListener

LeapBlobInputListener::~LeapBlobInputListener(void)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_OBJEXIT(); //####
} // LeapBlobInputListener::~LeapBlobInputListener

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void LeapBlobInputListener::clearOutputChannel(void)
{
    OD_LOG_OBJENTER(); //####
    _outChannel = NULL;
    OD_LOG_OBJEXIT(); //####
} // LeapBlobInputListener::clearOutputChannel

void LeapBlobInputListener::onConnect(const Leap::Controller & theController)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("theController = ", &theController); //####
    theController.setPolicyFlags(Leap::Controller::POLICY_DEFAULT);
    OD_LOG_OBJEXIT(); //####
} // LeapBlobInputListener::onConnect

void LeapBlobInputListener::onDeviceChange(const Leap::Controller & theController)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("theController = ", &theController); //####
    OD_LOG_OBJEXIT(); //####
} // LeapBlobInputListener::onDeviceChange

void LeapBlobInputListener::onDisconnect(const Leap::Controller & theController)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("theController = ", &theController); //####
    OD_LOG_OBJEXIT(); //####
} // LeapBlobInputListener::onDisconnect

void LeapBlobInputListener::onExit(const Leap::Controller & theController)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("theController = ", &theController); //####
    OD_LOG_OBJEXIT(); //####
} // LeapBlobInputListener::onExit

void LeapBlobInputListener::onFocusGained(const Leap::Controller & theController)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("theController = ", &theController); //####
    OD_LOG_OBJEXIT(); //####
} // LeapBlobInputListener::onFocusGained

void LeapBlobInputListener::onFocusLost(const Leap::Controller & theController)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("theController = ", &theController); //####
    OD_LOG_OBJEXIT(); //####
} // LeapBlobInputListener::onFocusLost

void LeapBlobInputListener::onFrame(const Leap::Controller & theController)
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
            int realHandCount = 0;

            for (Leap::HandList::const_iterator handWalker(hands.begin());
                 hands.end() != handWalker; ++handWalker)
            {
                Leap::Hand aHand(*handWalker);

                if (aHand.isValid())
                {
                    Leap::FingerList fingers(aHand.fingers());
                    const char *     side;
                    int              fingersCount = 0;

                    for (Leap::FingerList::const_iterator fingerWalker(fingers.begin());
                         fingers.end() != fingerWalker; ++fingerWalker)
                    {
                        Leap::Finger aFinger(*fingerWalker);

                        if (aFinger.isValid())
                        {
                            ++fingersCount;
                        }
                    }
                    if (0 < fingersCount)
                    {
                        ++realHandCount;
                    }
                }
            }
            if (0 < realHandCount)
            {
                std::stringstream outBuffer;
                yarp::os::Bottle  message;

                // Write out the number of hands == bodies.
                outBuffer << realHandCount << LINE_END_;
                for (Leap::HandList::const_iterator handWalker(hands.begin());
                     hands.end() != handWalker; ++handWalker)
                {
                    Leap::Hand aHand(*handWalker);

                    if (aHand.isValid())
                    {
                        Leap::FingerList fingers(aHand.fingers());
                        int              fingersCount = 0;

                        for (Leap::FingerList::const_iterator fingerWalker(fingers.begin());
                             fingers.end() != fingerWalker; ++fingerWalker)
                        {
                            Leap::Finger aFinger(*fingerWalker);

                            if (aFinger.isValid())
                            {
                                ++fingersCount;
                            }
                        }
                        if (0 < fingersCount)
                        {
                            const char * side;

                            if (aHand.isLeft())
                            {
                                side = "left";
                            }
                            else if (aHand.isRight())
                            {
                                side = "right";
                            }
                            else
                            {
                                side = "unknown";
                            }
                            outBuffer << side << "\t" << fingersCount << "\t0" << LINE_END_;
                            for (Leap::FingerList::const_iterator fingerWalker(fingers.begin());
                                 fingers.end() != fingerWalker; ++fingerWalker)
                            {
                                Leap::Finger aFinger(*fingerWalker);

                                if (aFinger.isValid())
                                {
                                    const char * fingerType;
                                    Leap::Vector fingerPosition(aFinger.tipPosition());
                                    Leap::Vector fingerDirection(aFinger.direction());

                                    switch (aFinger.type())
                                    {
                                        case Leap::Finger::TYPE_THUMB :
                                            fingerType = "thumb";
                                            break;

                                        case Leap::Finger::TYPE_INDEX :
                                            fingerType = "index";
                                            break;

                                        case Leap::Finger::TYPE_MIDDLE :
                                            fingerType = "middle";
                                            break;

                                        case Leap::Finger::TYPE_RING :
                                            fingerType = "ring";
                                            break;

                                        case Leap::Finger::TYPE_PINKY :
                                            fingerType = "pinky";
                                            break;

                                        default :
                                            fingerType = "unknown";
                                            break;

                                    }
                                    outBuffer << fingerType << "\t" <<
                                                (fingerPosition.x * _scale) << "\t" <<
                                                (fingerPosition.y * _scale) << "\t" <<
                                                (fingerPosition.z * _scale) << "\t" <<
                                                fingerDirection.x << "\t" << fingerDirection.y <<
                                                "\t" << fingerDirection.z << "\t" << LINE_END_;
                                }
                            }
                        }
                    }
                }
                outBuffer << "END" << LINE_END_;
                std::string       buffAsString(outBuffer.str());
                yarp::os::Value * blobValue =
                yarp::os::Value::makeBlob(const_cast<char *>(buffAsString.c_str()),
                                          buffAsString.length());
                
                message.add(blobValue);
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
                delete[] blobValue;
            }
        }
    }
    OD_LOG_OBJEXIT(); //####
} // LeapBlobInputListener::onFrame

void LeapBlobInputListener::onInit(const Leap::Controller & theController)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("theController = ", &theController); //####
	theController.setPolicyFlags(Leap::Controller::POLICY_BACKGROUND_FRAMES);
    OD_LOG_OBJEXIT(); //####
} // LeapBlobInputListener::onInit

void LeapBlobInputListener::onServiceConnect(const Leap::Controller & theController)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("theController = ", &theController); //####
    OD_LOG_OBJEXIT(); //####
} // LeapBlobInputListener::onServiceConnect

void LeapBlobInputListener::onServiceDisconnect(const Leap::Controller & theController)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("theController = ", &theController); //####
    OD_LOG_OBJEXIT(); //####
} // LeapBlobInputListener::onServiceDisconnect

void LeapBlobInputListener::setScale(const double newScale)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_D1("newScale = ", newScale); //####
    _scale = newScale;
    OD_LOG_OBJEXIT(); //####
} // LeapBlobInputListener::setScale

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
