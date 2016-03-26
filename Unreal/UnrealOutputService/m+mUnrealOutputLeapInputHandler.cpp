//--------------------------------------------------------------------------------------------------
//
//  File:       m+mUnrealOutputLeapInputHandler.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the Leap Motion input channel input handler used by the
//              Unreal output service.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2014 by H Plus Technologies Ltd. and Simon Fraser University.
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
//  Created:    2014-11-19
//
//--------------------------------------------------------------------------------------------------

#include "m+mUnrealOutputLeapInputHandler.h"
#include "m+mUnrealOutputService.h"

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the %Leap Motion input channel input handler used by the %Unreal
 output service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Unreal;
using std::cerr;
using std::endl;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief The scale factor to apply to %Leap Motion data. */
static const double kLeapScale = 1.0;

#if defined(__APPLE__)
# pragma mark Global constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

/*! @brief Check the dictionary entry from the finger data.
 @param outBuffer The destination character stream.
 @param fingerProps The dictionary to be checked.
 @param scale The translation scale to use.
 @param okSoFar Set to @c false if an unexpected value appears. */
#if defined(MpM_UseCustomStringBuffer)
static void
dumpFingerProps(Common::StringBuffer & outBuffer,
                yarp::os::Property &   fingerProps,
                const double           scale,
                bool &                 okSoFar)
#else // ! defined(MpM_UseCustomStringBuffer)
static void
dumpFingerProps(std::stringstream &  outBuffer,
                yarp::os::Property & fingerProps,
                const double         scale,
                bool &               okSoFar)
#endif // ! defined(MpM_UseCustomStringBuffer)
{
    ODL_ENTER(); //####
    ODL_P3("outBuffer = ", &outBuffer, "fingerProps = ", fingerProps, "okSoFar = ", //####
              &okSoFar); //####
    ODL_D1("scale = ", scale); //####
    yarp::os::Value & fingerType(fingerProps.find("type"));
    yarp::os::Value & tipPosition(fingerProps.find("tipposition"));
    yarp::os::Value & tipDirection(fingerProps.find("direction"));
    
    if (fingerType.isString() && tipPosition.isList() && tipDirection.isList())
    {
        YarpString         fingerTag = fingerType.asString();
        yarp::os::Bottle * positionData = tipPosition.asList();
        yarp::os::Bottle * directionData = tipDirection.asList();
        
        if (positionData && (3 == positionData->size()) && directionData &&
            (3 == directionData->size()))
        {
            cerr << "Segment = " << fingerTag.c_str() << endl; //!!!!
#if defined(MpM_UseCustomStringBuffer)
            outBuffer.addString(fingerTag);
#else // ! defined(MpM_UseCustomStringBuffer)
            outBuffer << fingerTag.c_str();
#endif // ! defined(MpM_UseCustomStringBuffer)
            for (int jj = 0; okSoFar && (3 > jj); ++jj)
            {
                double            aValue;
                yarp::os::Value & anElement = positionData->get(jj);
                
                if (anElement.isDouble())
                {
                    aValue = anElement.asDouble();
                }
                else if (anElement.isInt())
                {
                    aValue = anElement.asInt();
                }
                else
                {
                    cerr << "Bad position data" << endl; //!!!!
                    okSoFar = false;
                }
                if (okSoFar)
                {
                    aValue *= (scale * kLeapScale);
#if defined(MpM_UseCustomStringBuffer)
                    outBuffer.addTab().addDouble(aValue);
#else // ! defined(MpM_UseCustomStringBuffer)
                    outBuffer << "\t" << aValue;
#endif // ! defined(MpM_UseCustomStringBuffer)
                }
            }
            for (int jj = 0; okSoFar && (3 > jj); ++jj)
            {
                double            aValue;
                yarp::os::Value & anElement = directionData->get(jj);
                
                if (anElement.isDouble())
                {
                    aValue = anElement.asDouble();
                }
                else if (anElement.isInt())
                {
                    aValue = anElement.asInt();
                }
                else
                {
                    cerr << "Bad direction data" << endl; //!!!!
                    okSoFar = false;
                }
                if (okSoFar)
                {
#if defined(MpM_UseCustomStringBuffer)
                    outBuffer.addTab().addDouble(aValue);
#else // ! defined(MpM_UseCustomStringBuffer)
                    outBuffer << "\t" << aValue;
#endif // ! defined(MpM_UseCustomStringBuffer)
                }
            }
            if (okSoFar)
            {
                // Add a dummy 'w' value.
#if defined(MpM_UseCustomStringBuffer)
                outBuffer.addString("\t1" LINE_END_);
#else // ! defined(MpM_UseCustomStringBuffer)
                outBuffer << "\t1" LINE_END_;
#endif // ! defined(MpM_UseCustomStringBuffer)
            }
        }
        else
        {
            cerr << "Position or direction data invalid" << endl; //!!!!
            okSoFar = false;
        }
    }
    else
    {
        cerr << "Missing or invalid finger values" << endl; //!!!!
        okSoFar = false;
    }
    ODL_EXIT(); //####
} // dumpFingerProps

/*! @brief Convert hand data into a character stream.
 @param outBuffer The destination character stream.
 @param handData The hand data to write out.
 @param scale The translation scale to use.
 @returns @c true if the had data was properly structured and @c false otherwise. */
#if defined(MpM_UseCustomStringBuffer)
static bool
dumpHandData(Common::StringBuffer & outBuffer,
             yarp::os::Property &   handData,
             const double           scale)
#else // ! defined(MpM_UseCustomStringBuffer)
static bool
dumpHandData(std::stringstream &  outBuffer,
             yarp::os::Property & handData,
             const double         scale)
#endif // ! defined(MpM_UseCustomStringBuffer)
{
    ODL_ENTER(); //####
    ODL_P2("outBuffer = ", &outBuffer, "handData = ", &handData); //####
    ODL_D1("scale = ", scale); //####
    bool              okSoFar = true;
    yarp::os::Value & idValue(handData.find("id"));
    yarp::os::Value & fingerValue(handData.find("fingers"));
    yarp::os::Value & sideValue(handData.find("side"));
    
    if ((! idValue.isNull()) && sideValue.isString() && fingerValue.isList())
    {
        YarpString         nameTag(sideValue.asString());
        yarp::os::Bottle * fingers = fingerValue.asList();
        
        if (fingers)
        {
            int fingerCount = fingers->size();
            
            cerr << "Subject = " << nameTag.c_str() << endl; //!!!!
#if defined(MpM_UseCustomStringBuffer)
            outBuffer.addString(nameTag).addTab().addLong(fingerCount).
                addString("\t0" LINE_END_);
#else // ! defined(MpM_UseCustomStringBuffer)
            outBuffer << nameTag.c_str() << "\t" << fingerCount << "\t0" LINE_END_;
#endif // ! defined(MpM_UseCustomStringBuffer)
            for (int ii = 0; okSoFar && (fingerCount > ii); ++ii)
            {
                yarp::os::Value & aFinger = fingers->get(ii);
                
                if (aFinger.isDict())
                {
                    yarp::os::Property * fingerProps = aFinger.asDict();
                    
                    if (fingerProps)
                    {
                        dumpFingerProps(outBuffer, *fingerProps, scale, okSoFar);
                    }
                    else
                    {
                        cerr << "Bad finger pointer" << endl; //!!!!
                        okSoFar = false;
                    }
                }
                else if (aFinger.isList())
                {
                    yarp::os::Bottle * fingerList = aFinger.asList();
                    
                    if (fingerList)
                    {
                        yarp::os::Property fingerProps;
                        
                        if (ListIsReallyDictionary(*fingerList, fingerProps))
                        {
                            dumpFingerProps(outBuffer, fingerProps, scale, okSoFar);
                        }
                        else
                        {
                            cerr << "Finger is not a dictionary" << endl; //!!!!
                            okSoFar = false;
                        }
                    }
                    else
                    {
                        cerr << "Bad finger list pointer" << endl; //!!!!
                        okSoFar = false;
                    }
                }
                else
                {
                    cerr << "Finger is not a dictionary" << endl; //!!!!
                    okSoFar = false;
                }
            }
        }
        else
        {
            cerr << "Bad finger list pointer" << endl; //!!!!
            okSoFar = false;
        }
    }
    else
    {
        cerr << "Missing or invalid hand values" << endl; //!!!!
        okSoFar = false;
    }
    ODL_EXIT_B(okSoFar); //####
    return okSoFar;
} // dumpHandData

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

UnrealOutputLeapInputHandler::UnrealOutputLeapInputHandler(UnrealOutputService & owner) :
    inherited(), _owner(owner), _scale(1.0), _outSocket(INVALID_SOCKET)
{
    ODL_ENTER(); //####
    ODL_P1("owner = ", &owner); //####
    ODL_EXIT_P(this); //####
} // UnrealOutputLeapInputHandler::UnrealOutputLeapInputHandler

UnrealOutputLeapInputHandler::~UnrealOutputLeapInputHandler(void)
{
    ODL_OBJENTER(); //####
    ODL_OBJEXIT(); //####
} // UnrealOutputLeapInputHandler::~UnrealOutputLeapInputHandler

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
DEFINE_HANDLE_INPUT_(UnrealOutputLeapInputHandler)
{
#if (! defined(OD_ENABLE_LOGGING_))
# if MAC_OR_LINUX_
#  pragma unused(senderChannel,replyMechanism)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING_)
    ODL_OBJENTER(); //####
    ODL_S2s("senderChannel = ", senderChannel, "got ", input.toString()); //####
    ODL_P1("replyMechanism = ", replyMechanism); //####
    ODL_L1("numBytes = ", numBytes); //####
    bool result = true;
    
    try
    {
        if (_owner.isActive())
        {
            if (INVALID_SOCKET == _outSocket)
            {
                cerr << "invalid socket" << endl; //!!!!
            }
            else
            {
                cerr << "got data" << endl; //!!!!
                if (2 == input.size())
                {
                    yarp::os::Value & firstTopValue = input.get(0);
                    yarp::os::Value & secondTopValue = input.get(1);
                    
                    if (firstTopValue.isList() && secondTopValue.isList())
                    {
                        yarp::os::Bottle * handList = firstTopValue.asList();
                        yarp::os::Bottle * toolList = secondTopValue.asList();
                        
                        if (handList && toolList)
                        {
                            int handCount = handList->size();
                            
                            if (0 < handCount)
                            {
                                bool              okSoFar = true;
#if (! defined(MpM_UseCustomStringBuffer))
                                std::stringstream outBuffer;
#endif // ! defined(MpM_UseCustomStringBuffer)
                                
//                                cerr << "# hands = " << handCount << endl; //!!!!
#if defined(MpM_UseCustomStringBuffer)
                                _outBuffer.reset().addLong(handCount).addString(LINE_END_);
#else // ! defined(MpM_UseCustomStringBuffer)
                                outBuffer << handCount << LINE_END_;
#endif // ! defined(MpM_UseCustomStringBuffer)
                                for (int ii = 0; okSoFar && (handCount > ii); ++ii)
                                {
                                    yarp::os::Value & handValue = handList->get(ii);
                                    
                                    if (handValue.isDict())
                                    {
                                        yarp::os::Property * handData = handValue.asDict();
                                        
                                        if (handData)
                                        {
#if defined(MpM_UseCustomStringBuffer)
                                            okSoFar = dumpHandData(_outBuffer, *handData, _scale);
#else // ! defined(MpM_UseCustomStringBuffer)
                                            okSoFar = dumpHandData(outBuffer, *handData, _scale);
#endif // ! defined(MpM_UseCustomStringBuffer)
                                        }
                                        else
                                        {
                                            cerr << "Bad hand data pointer" << endl; //!!!!
                                            okSoFar = false;
                                        }
                                    }
                                    else if (handValue.isList())
                                    {
                                        yarp::os::Bottle * asList = handValue.asList();
                                        
                                        if (asList)
                                        {
                                            yarp::os::Property handData;
                                            
                                            if (ListIsReallyDictionary(*asList, handData))
                                            {
#if defined(MpM_UseCustomStringBuffer)
                                                okSoFar = dumpHandData(_outBuffer, handData,
                                                                       _scale);
#else // ! defined(MpM_UseCustomStringBuffer)
                                                okSoFar = dumpHandData(outBuffer, handData, _scale);
#endif // ! defined(MpM_UseCustomStringBuffer)
                                            }
                                            else
                                            {
                                                cerr << "Hand value is not a dictionary" <<
                                                        endl; //!!!!
                                                okSoFar = false;
                                            }
                                        }
                                        else
                                        {
                                            cerr << "Bad hand data pointer" << endl; //!!!!
                                            okSoFar = false;
                                        }
                                    }
                                    else
                                    {
                                        cerr << "Hand value is not a dictionary" << endl; //!!!!
                                        okSoFar = false;
                                    }
                                }
                                if (okSoFar)
                                {
#if defined(MpM_UseCustomStringBuffer)
                                    _outBuffer.addString("END" LINE_END_);
#else // ! defined(MpM_UseCustomStringBuffer)
                                    outBuffer << "END" LINE_END_;
#endif // ! defined(MpM_UseCustomStringBuffer)
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
                                        int retVal = send(_outSocket, outString,
                                                          static_cast<int>(outLength), 0);
                                        
                                        cerr << "send--> " << retVal << endl; //!!!!
                                        if (0 > retVal)
                                        {
                                            _owner.deactivateConnection();
                                        }
                                        else
                                        {
                                            SendReceiveCounters toBeAdded(0, 0, retVal, 1);
                                            
                                            _owner.incrementAuxiliaryCounters(toBeAdded);
                                        }
                                    }
                                }
                            }
                        }
                        else
                        {
                            cerr << "Bad hand or tool list pointer" << endl; //!!!!
                        }
                    }
                    else
                    {
                        cerr << "Input not just a list of hands and a list of tools" << endl; //!!!!
                    }
                }
                else
                {
                    cerr << "Input not just a list of hands and a list of tools" << endl; //!!!!
                }
            }
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT_B(result); //####
    return result;
} // UnrealOutputLeapInputHandler::handleInput
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

void
UnrealOutputLeapInputHandler::setScale(const double newScale)
{
    ODL_OBJENTER(); //####
    ODL_D1("newScale = ", newScale); //####
    _scale = newScale;
    ODL_OBJEXIT(); //####
} // UnrealOutputLeapInputHandler::setScale

void
UnrealOutputLeapInputHandler::setSocket(const SOCKET outSocket)
{
    ODL_OBJENTER(); //####
    ODL_L1("outSocket = ", outSocket); //####
    _outSocket = outSocket;
    ODL_OBJEXIT(); //####
} // UnrealOutputLeapInputHandler::setSocket

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
