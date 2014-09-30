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

#include "M+MLeapMotionInputListener.h"

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The class definition for a %Leap Motion listener. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::LeapMotion;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

/*! @brief Put the three coordinates of a %Vector in a dictionary with the given tag.
 @param dictionary The dictionary to be added to.
 @param tag The name to be associated with the value.
 @param vectorToUse The %Vector containing the data to be added. */
static void putVectorInDictionary(yarp::os::Property &          dictionary,
                                  const yarp::os::ConstString & tag,
                                  const Leap::Vector &          vectorToUse)
{
    yarp::os::Value    stuff;
    yarp::os::Bottle * stuffList = stuff.asList();
    
    if (stuffList)
    {
        stuffList->addDouble(vectorToUse.x);
        stuffList->addDouble(vectorToUse.y);
        stuffList->addDouble(vectorToUse.z);
        dictionary.put(tag, stuff);
    }
} // putVectorInDictionary

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

LeapMotionInputListener::LeapMotionInputListener(GeneralChannel * outChannel) :
    inherited(), _outChannel(outChannel)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P1("outChannel = ", outChannel); //####
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

void LeapMotionInputListener::clearOutputChannel(void)
{
    OD_LOG_OBJENTER(); //####
    _outChannel = NULL;
    OD_LOG_OBJEXIT(); //####
} // LeapMotionInputListener::clearOutputChannel

void LeapMotionInputListener::onConnect(const Leap::Controller & theController)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("theController = ", &theController); //####
    theController.setPolicyFlags(Leap::Controller::POLICY_DEFAULT);
    OD_LOG_OBJEXIT(); //####
} // LeapMotionInputListener::onConnect

void LeapMotionInputListener::onDeviceChange(const Leap::Controller & theController)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("theController = ", &theController); //####
    OD_LOG_OBJEXIT(); //####
} // LeapMotionInputListener::onDeviceChange

void LeapMotionInputListener::onDisconnect(const Leap::Controller & theController)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("theController = ", &theController); //####
    OD_LOG_OBJEXIT(); //####
} // LeapMotionInputListener::onDisconnect

void LeapMotionInputListener::onExit(const Leap::Controller & theController)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("theController = ", &theController); //####
    OD_LOG_OBJEXIT(); //####
} // LeapMotionInputListener::onExit

void LeapMotionInputListener::onFocusGained(const Leap::Controller & theController)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("theController = ", &theController); //####
    OD_LOG_OBJEXIT(); //####
} // LeapMotionInputListener::onFocusGained

void LeapMotionInputListener::onFocusLost(const Leap::Controller & theController)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("theController = ", &theController); //####
    OD_LOG_OBJEXIT(); //####
} // LeapMotionInputListener::onFocusLost

void LeapMotionInputListener::onFrame(const Leap::Controller & theController)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("theController = ", &theController); //####
    Leap::Frame latestFrame(theController.frame());
    
    if (latestFrame.isValid())
    {
        Leap::HandList hands(latestFrame.hands());
        Leap::ToolList tools(latestFrame.tools());
        int            handCount = hands.count();
        int            toolCount = tools.count();
        
        if (0 < (handCount + toolCount))
        {
            yarp::os::Bottle   message;
            yarp::os::Bottle & handStuff = message.addList();

            for (Leap::HandList::const_iterator handWalker(hands.begin());
                 hands.end() != handWalker; ++handWalker)
            {
                Leap::Hand aHand(*handWalker);
                
                if (aHand.isValid())
                {
                    yarp::os::Property & handProps = handStuff.addDict();
                    
                    handProps.put("id", aHand.id());
                    putVectorInDictionary(handProps, "palmposition", aHand.palmPosition());
                    putVectorInDictionary(handProps, "palmnormal", aHand.palmNormal());
                    putVectorInDictionary(handProps, "palmvelocity", aHand.palmVelocity());
                    putVectorInDictionary(handProps, "direction", aHand.direction());
                    Leap::Arm anArm(aHand.arm());
                    
                    if (anArm.isValid())
                    {
                        yarp::os::Value    armStuff;
                        yarp::os::Bottle * armList = armStuff.asList();
                        
                        if (armList)
                        {
                            yarp::os::Property & armDict = armList->addDict();
                            
                            putVectorInDictionary(armDict, "direction", anArm.direction());
                            putVectorInDictionary(armDict, "elbowposition", anArm.elbowPosition());
                            handProps.put("arm", armStuff);
                        }
                    }
                    putVectorInDictionary(handProps, "wristposition", aHand.wristPosition());
                    handProps.put("confidence", aHand.confidence());
                    if (aHand.isLeft())
                    {
                        handProps.put("side", "left");
                    }
                    else if (aHand.isRight())
                    {
                        handProps.put("side", "right");
                    }
                    else
                    {
                        handProps.put("side", "unknown");
                    }
                    yarp::os::Value    fingerSet;
                    yarp::os::Bottle * fingerSetAsList = fingerSet.asList();

                    if (fingerSetAsList)
                    {
                        // fingers
                        Leap::FingerList fingers(aHand.fingers());
                        
                        for (Leap::FingerList::const_iterator fingerWalker(fingers.begin());
                             fingers.end() != fingerWalker; ++fingerWalker)
                        {
                            Leap::Finger aFinger(*fingerWalker);

                            if (aFinger.isValid())
                            {
                                yarp::os::Property & fingProps = fingerSetAsList->addDict();

                                fingProps.put("id", aFinger.id());
                                switch (aFinger.type())
                                {
                                    case Leap::Finger::TYPE_THUMB :
                                        fingProps.put("type", "thumb");
                                        break;
                                        
                                    case Leap::Finger::TYPE_INDEX :
                                        fingProps.put("type", "index");
                                        break;
                                        
                                    case Leap::Finger::TYPE_MIDDLE :
                                        fingProps.put("type", "middle");
                                        break;
                                        
                                    case Leap::Finger::TYPE_RING :
                                        fingProps.put("type", "ring");
                                        break;
                                        
                                    case Leap::Finger::TYPE_PINKY :
                                        fingProps.put("type", "pinky");
                                        break;
                                        
                                    default:
                                        fingProps.put("type", "unknown");
                                        break;
                                        
                                }
                                putVectorInDictionary(fingProps, "tipposition",
                                                      aFinger.tipPosition());
                                putVectorInDictionary(fingProps, "tipvelocity",
                                                      aFinger.tipVelocity());
                                putVectorInDictionary(fingProps, "direction",
                                                      aFinger.direction());
                                fingProps.put("length", aFinger.length());
                                if (aFinger.isExtended())
                                {
                                    fingProps.put("extended", "yes");
                                }
                                else
                                {
                                    fingProps.put("extended", "no");
                                }
                                yarp::os::Value    bonesStuff;
                                yarp::os::Bottle * bonesAsList = bonesStuff.asList();
                                
                                if (bonesAsList)
                                {
                                    static const Leap::Bone::Type boneType[] =
                                    {
                                        Leap::Bone::TYPE_METACARPAL,
                                        Leap::Bone::TYPE_PROXIMAL,
                                        Leap::Bone::TYPE_INTERMEDIATE,
                                        Leap::Bone::TYPE_DISTAL
                                    };
                                    static const size_t numBoneTypes = (sizeof(boneType) /
                                                                        sizeof(*boneType));
                                    
                                    for (size_t ii = 0; numBoneTypes > ii; ++ii)
                                    {
                                        Leap::Bone::Type aType = boneType[ii];
                                        Leap::Bone       aBone = aFinger.bone(aType);
                                        
                                        if (aBone.isValid())
                                        {
                                            yarp::os::Property & boneProps = bonesAsList->addDict();

                                            putVectorInDictionary(boneProps, "proximal",
                                                                  aBone.prevJoint());
                                            putVectorInDictionary(boneProps, "distal",
                                                                  aBone.nextJoint());
                                            putVectorInDictionary(boneProps, "direction",
                                                                  aBone.direction());
                                            boneProps.put("length", aBone.length());
                                        }
                                    }
                                    fingProps.put("bones", bonesStuff);
                                }
                            }
                        }
                        handProps.put("fingers", fingerSet);
                    }
                }
            }
            yarp::os::Bottle & toolStuff = message.addList();
            
            for (Leap::ToolList::const_iterator toolWalker(tools.begin());
                 tools.end() != toolWalker; ++toolWalker)
            {
                Leap::Tool aTool(*toolWalker);
                
                if (aTool.isValid())
                {
                    yarp::os::Property & toolProps = toolStuff.addDict();
                    
                    toolProps.put("id", aTool.id());
                    putVectorInDictionary(toolProps, "tipposition", aTool.tipPosition());
                    putVectorInDictionary(toolProps, "tipvelocity", aTool.tipVelocity());
                    putVectorInDictionary(toolProps, "direction", aTool.direction());
                    toolProps.put("length", aTool.length());
                }
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
    OD_LOG_OBJEXIT(); //####
} // LeapMotionInputListener::onFrame

void LeapMotionInputListener::onInit(const Leap::Controller & theController)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("theController = ", &theController); //####
    OD_LOG_OBJEXIT(); //####
} // LeapMotionInputListener::onInit

void LeapMotionInputListener::onServiceConnect(const Leap::Controller & theController)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("theController = ", &theController); //####
    OD_LOG_OBJEXIT(); //####
} // LeapMotionInputListener::onServiceConnect

void LeapMotionInputListener::onServiceDisconnect(const Leap::Controller & theController)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("theController = ", &theController); //####
    OD_LOG_OBJEXIT(); //####
} // LeapMotionInputListener::onServiceDisconnect

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
