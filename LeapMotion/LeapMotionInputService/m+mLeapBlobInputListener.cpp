//--------------------------------------------------------------------------------------------------
//
//  File:       m+mLeapBlobInputListener.cpp
//
//  Project:    m+m
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

#include "m+mLeapBlobInputListener.hpp"

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for a %Leap %Blob listener. */
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
    ODL_ENTER(); //####
    ODL_P1("outChannel = ", outChannel); //####
    ODL_EXIT_P(this); //####
} // LeapBlobInputListener::LeapBlobInputListener

LeapBlobInputListener::~LeapBlobInputListener(void)
{
    ODL_OBJENTER(); //####
    ODL_OBJEXIT(); //####
} // LeapBlobInputListener::~LeapBlobInputListener

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void
LeapBlobInputListener::clearOutputChannel(void)
{
    ODL_OBJENTER(); //####
    _outChannel = NULL;
    ODL_OBJEXIT(); //####
} // LeapBlobInputListener::clearOutputChannel

void
LeapBlobInputListener::onConnect(const Leap::Controller & theController)
{
    ODL_OBJENTER(); //####
    ODL_P1("theController = ", &theController); //####
    theController.setPolicyFlags(Leap::Controller::POLICY_DEFAULT);
    ODL_OBJEXIT(); //####
} // LeapBlobInputListener::onConnect

void
LeapBlobInputListener::onDeviceChange(const Leap::Controller & theController)
{
    ODL_OBJENTER(); //####
    ODL_P1("theController = ", &theController); //####
    ODL_OBJEXIT(); //####
} // LeapBlobInputListener::onDeviceChange

void
LeapBlobInputListener::onDisconnect(const Leap::Controller & theController)
{
    ODL_OBJENTER(); //####
    ODL_P1("theController = ", &theController); //####
    ODL_OBJEXIT(); //####
} // LeapBlobInputListener::onDisconnect

void
LeapBlobInputListener::onExit(const Leap::Controller & theController)
{
    ODL_OBJENTER(); //####
    ODL_P1("theController = ", &theController); //####
    ODL_OBJEXIT(); //####
} // LeapBlobInputListener::onExit

void
LeapBlobInputListener::onFocusGained(const Leap::Controller & theController)
{
    ODL_OBJENTER(); //####
    ODL_P1("theController = ", &theController); //####
    ODL_OBJEXIT(); //####
} // LeapBlobInputListener::onFocusGained

void
LeapBlobInputListener::onFocusLost(const Leap::Controller & theController)
{
    ODL_OBJENTER(); //####
    ODL_P1("theController = ", &theController); //####
    ODL_OBJEXIT(); //####
} // LeapBlobInputListener::onFocusLost

void
LeapBlobInputListener::onFrame(const Leap::Controller & theController)
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
            int realHandCount = 0;

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
                        ++realHandCount;
                    }
                }
            }
            if (0 < realHandCount)
            {
#if (! defined(MpM_UseCustomStringBuffer))
                std::stringstream outBuffer;
#endif // ! defined(MpM_UseCustomStringBuffer)

                // Write out the number of hands == bodies.
#if defined(MpM_UseCustomStringBuffer)
                _outBuffer.reset().addLong(realHandCount).addString(LINE_END_);
#else // ! defined(MpM_UseCustomStringBuffer)
                outBuffer << realHandCount << LINE_END_;
#endif // ! defined(MpM_UseCustomStringBuffer)
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
#if defined(MpM_UseCustomStringBuffer)
                            _outBuffer.addString(side).addTab().addLong(fingersCount).
                                addString("\t0" LINE_END_);
#else // ! defined(MpM_UseCustomStringBuffer)
                            outBuffer << side << "\t" << fingersCount << "\t0" LINE_END_;
#endif // ! defined(MpM_UseCustomStringBuffer)
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
#if defined(MpM_UseCustomStringBuffer)
                                    _outBuffer.addString(fingerType).addTab();
                                    _outBuffer.addDouble(fingerPosition.x * _scale).addTab();
                                    _outBuffer.addDouble(fingerPosition.y * _scale).addTab();
                                    _outBuffer.addDouble(fingerPosition.z * _scale).addTab();
                                    _outBuffer.addDouble(fingerDirection.x).addTab();
                                    _outBuffer.addDouble(fingerDirection.y).addTab();
                                    _outBuffer.addDouble(fingerDirection.z).
                                        addString("\t1" LINE_END_);
#else // ! defined(MpM_UseCustomStringBuffer)
                                    outBuffer << fingerType << "\t" <<
                                                (fingerPosition.x * _scale) << "\t" <<
                                                (fingerPosition.y * _scale) << "\t" <<
                                                (fingerPosition.z * _scale) << "\t" <<
                                                fingerDirection.x << "\t" << fingerDirection.y <<
                                                "\t" << fingerDirection.z << "\t1" LINE_END_;
#endif // ! defined(MpM_UseCustomStringBuffer)
                                }
                            }
                        }
                    }
                }
#if defined(MpM_UseCustomStringBuffer)
                _outBuffer.addString("END" LINE_END_);
#else // ! defined(MpM_UseCustomStringBuffer)
                outBuffer << "END" LINE_END_;
#endif // ! defined(MpM_UseCustomStringBuffer)
                if (_outChannel)
                {
                    const char * outString;
                    size_t       outLength;
#if (! defined(MpM_UseCustomStringBuffer))
                    std::string  buffAsString(outBuffer.str());
#endif // ! defined(MpM_UseCustomStringBuffer)

#if defined(MpM_UseCustomStringBuffer)
                    outString = _outBuffer.getString(outLength);
#else // ! defined(MpM_UseCustomStringBuffer)
                    outString = buffAsString.c_str();
                    outLength = buffAsString.length();
#endif // ! defined(MpM_UseCustomStringBuffer)
                    if (outString && outLength)
                    {
                        void *          rawString =
                                                static_cast<void *>(const_cast<char *>(outString));
                        yarp::os::Value blobValue(rawString, static_cast<int>(outLength));

                        _messageBottle.clear();
                        _messageBottle.add(blobValue);
                        if (! _outChannel->write(_messageBottle))
                        {
                            ODL_LOG("(! _outChannel->write(message))"); //####
#if defined(MpM_StallOnSendProblem)
                            Stall();
#endif // defined(MpM_StallOnSendProblem)
                        }
                    }
                }
            }
        }
    }
    ODL_OBJEXIT(); //####
} // LeapBlobInputListener::onFrame

void
LeapBlobInputListener::onInit(const Leap::Controller & theController)
{
    ODL_OBJENTER(); //####
    ODL_P1("theController = ", &theController); //####
    theController.setPolicyFlags(Leap::Controller::POLICY_BACKGROUND_FRAMES);
    ODL_OBJEXIT(); //####
} // LeapBlobInputListener::onInit

void
LeapBlobInputListener::onServiceConnect(const Leap::Controller & theController)
{
    ODL_OBJENTER(); //####
    ODL_P1("theController = ", &theController); //####
    ODL_OBJEXIT(); //####
} // LeapBlobInputListener::onServiceConnect

void
LeapBlobInputListener::onServiceDisconnect(const Leap::Controller & theController)
{
    ODL_OBJENTER(); //####
    ODL_P1("theController = ", &theController); //####
    ODL_OBJEXIT(); //####
} // LeapBlobInputListener::onServiceDisconnect

void
LeapBlobInputListener::setScale(const double newScale)
{
    ODL_OBJENTER(); //####
    ODL_D1("newScale = ", newScale); //####
    _scale = newScale;
    ODL_OBJEXIT(); //####
} // LeapBlobInputListener::setScale

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
