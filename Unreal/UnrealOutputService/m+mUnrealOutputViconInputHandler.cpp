//--------------------------------------------------------------------------------------------------
//
//  File:       m+mUnrealOutputViconInputHandler.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the Vicon DataStream input channel input handler used by
//              the Unreal output service.
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
//  Created:    2014-11-18
//
//--------------------------------------------------------------------------------------------------

#include "m+mUnrealOutputViconInputHandler.h"
#include "m+mUnrealOutputService.h"

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the Vicon DataStream input channel input handler used by the
 %Unreal output service. */
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

#if defined(__APPLE__)
# pragma mark Global constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

/*! @brief Convert list of segments into a character stream.
 @param outBuffer The destination character stream.
 @param segmentsAsBottle The segments to write out.
 @param scale The translation scale to use.
 @returns @c true if the segments were properly structured and @c false otherwise. */
#if defined(MpM_UseCustomStringBuffer)
static bool dumpSegments(Common::StringBuffer & outBuffer,
                         yarp::os::Bottle &     segmentsAsBottle,
                         const double           scale)
#else // ! defined(MpM_UseCustomStringBuffer)
static bool dumpSegments(std::stringstream & outBuffer,
                         yarp::os::Bottle &  segmentsAsBottle,
                         const double        scale)
#endif // ! defined(MpM_UseCustomStringBuffer)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("outBuffer = ", &outBuffer, "segmentsAsBottle = ", &segmentsAsBottle); //####
    OD_LOG_D1("scale = ", scale); //####
    bool okSoFar = true;
    int  numSegments = segmentsAsBottle.size();
    
    if (0 < numSegments)
    {
        for (int ii = 0; okSoFar && (numSegments > ii); ++ii)
        {
            yarp::os::Value & aValue = segmentsAsBottle.get(ii);
            
            if (aValue.isList())
            {
                yarp::os::Bottle * asList = aValue.asList();
                
                if (asList && (2 == asList->size()))
                {
                    yarp::os::Value & keyValue = asList->get(0);
                    yarp::os::Value & valueValue = asList->get(1);
                    
                    if (keyValue.isString() && valueValue.isList())
                    {
                        YarpString         keyString = keyValue.asString();
                        yarp::os::Bottle * valueList = valueValue.asList();
                        
#if defined(MpM_UseCustomStringBuffer)
                        outBuffer.addString(keyString);
#else // ! defined(MpM_UseCustomStringBuffer)
                        outBuffer << keyString.c_str();
#endif // ! defined(MpM_UseCustomStringBuffer)
                        if (valueList && (7 == valueList->size()))
                        {
                            int numValues = valueList->size();
                            
                            for (int jj = 0; okSoFar && (numValues > jj); ++jj)
                            {
                                double            elementAsDouble;
                                yarp::os::Value & valueElement = valueList->get(jj);
                                
                                if (valueElement.isDouble())
                                {
                                    elementAsDouble = valueElement.asDouble();
                                }
                                else if (valueElement.isInt())
                                {
                                    elementAsDouble = valueElement.asInt();
                                }
                                else
                                {
									cerr << "value not an integer or a float" << endl; //!!!!
                                    okSoFar = false;
                                }
                                if (okSoFar)
                                {
                                    if (3 > jj)
                                    {
                                        elementAsDouble *= scale;
                                    }
#if defined(MpM_UseCustomStringBuffer)
                                    outBuffer.addTab().addDouble(elementAsDouble);
#else // ! defined(MpM_UseCustomStringBuffer)
                                    outBuffer << "\t" << elementAsDouble;
#endif // ! defined(MpM_UseCustomStringBuffer)
                                }
                            }
#if defined(MpM_UseCustomStringBuffer)
                            outBuffer.addString(LINE_END_);
#else // ! defined(MpM_UseCustomStringBuffer)
                            outBuffer << LINE_END_;
#endif // ! defined(MpM_UseCustomStringBuffer)
                        }
                        else
                        {
							cerr << "bad list pointer or incorrect list size" << endl; //!!!!
                            okSoFar = false;
                        }
                    }
                    else
                    {
						cerr << "segment not a string and a list" << endl; //!!!!
                        okSoFar = false;
                    }
                }
                else
                {
					cerr << "segment not a 2-element list" << endl; //!!!!
                    okSoFar = false;
                }
            }
            else
            {
				cerr << "segment not a list" << endl; //!!!!
                okSoFar = false;
            }
        }
    }
    else
    {
		cerr << "no segments" << endl; //!!!!
        okSoFar = false;
    }
    OD_LOG_EXIT_B(okSoFar); //####
    return okSoFar;
} // dumpSegments

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

UnrealOutputViconInputHandler::UnrealOutputViconInputHandler(UnrealOutputService & owner) :
    inherited(), _owner(owner), _scale(1.0), _outSocket(INVALID_SOCKET)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P1("owner = ", &owner); //####
    OD_LOG_EXIT_P(this); //####
} // UnrealOutputViconInputHandler::UnrealOutputViconInputHandler

UnrealOutputViconInputHandler::~UnrealOutputViconInputHandler(void)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_OBJEXIT(); //####
} // UnrealOutputViconInputHandler::~UnrealOutputViconInputHandler

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
DEFINE_HANDLE_INPUT_(UnrealOutputViconInputHandler)
{
#if (! defined(OD_ENABLE_LOGGING))
# if MAC_OR_LINUX_
#  pragma unused(senderChannel,replyMechanism)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING)
    OD_LOG_OBJENTER(); //####
    OD_LOG_S2s("senderChannel = ", senderChannel, "got ", input.toString()); //####
    OD_LOG_P1("replyMechanism = ", replyMechanism); //####
    OD_LOG_L1("numBytes = ", numBytes); //####
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
                int numSubjects = input.size();
                
                if (0 < numSubjects)
                {
                    bool              okSoFar = true;
#if (! defined(MpM_UseCustomStringBuffer))
                    std::stringstream outBuffer;
#endif // ! defined(MpM_UseCustomStringBuffer)
                    
//                    cerr << "# subjects = " << numSubjects << endl; //!!!!
#if defined(MpM_UseCustomStringBuffer)
                    _outBuffer.reset().addLong(numSubjects).addString(LINE_END_);
#else // ! defined(MpM_UseCustomStringBuffer)
                    outBuffer << numSubjects << LINE_END_;
#endif // ! defined(MpM_UseCustomStringBuffer)
                    for (int ii = 0; okSoFar && (numSubjects > ii); ++ii)
                    {
                        yarp::os::Value & aValue = input.get(ii);
                        
                        if (aValue.isList())
                        {
                            yarp::os::Bottle * asBottle = aValue.asList();
                            
                            if (asBottle)
                            {
                                if (2 == asBottle->size())
                                {
                                    yarp::os::Value & firstValue = asBottle->get(0);
                                    
                                    if (firstValue.isString())
                                    {
                                        YarpString        subjName = firstValue.asString();
                                        yarp::os::Value & secondValue = asBottle->get(1);
                                        
                                        if (secondValue.isDict())
                                        {
                                            yarp::os::Property * segments = secondValue.asDict();
                                            
//                                            cerr << subjName.c_str() << endl; //!!!!
                                            if (segments)
                                            {
                                                YarpString       segmentsAsString =
                                                                            segments->toString();
//                                                cerr << ":" << segments->toString() << //!!!!
//                                                        ":" << endl; //!!!!
                                                yarp::os::Bottle segmentsAsBottle =
                                                                                segmentsAsString;
                                                
                                                cerr << ":" << segmentsAsBottle.size() << ":" <<
                                                        segmentsAsBottle.toString() << ":" <<
                                                        endl; //!!!!
#if defined(MpM_UseCustomStringBuffer)
                                                _outBuffer.addString(subjName).addTab();
                                                _outBuffer.addLong(segmentsAsBottle.size()).
                                                    addString("\t0" LINE_END_);
                                                okSoFar = dumpSegments(_outBuffer, segmentsAsBottle,
                                                                       _scale);
#else // ! defined(MpM_UseCustomStringBuffer)
                                                outBuffer << subjName.c_str() << "\t" <<
                                                            segmentsAsBottle.size() <<
                                                            "\t0" LINE_END_;
                                                okSoFar = dumpSegments(outBuffer, segmentsAsBottle,
                                                                       _scale);
#endif // ! defined(MpM_UseCustomStringBuffer)
                                            }
                                            else
                                            {
                                                cerr << "bad segments pointer" << endl; //!!!!
                                                okSoFar = false;
                                            }
                                        }
                                        else if (secondValue.isList())
                                        {
                                            yarp::os::Bottle * asList = secondValue.asList();
                                            
                                            if (asList)
                                            {
                                                yarp::os::Property segments;
                                                
                                                if (ListIsReallyDictionary(*asList, segments))
                                                {
                                                    YarpString       segmentsAsString =
                                                                                segments.toString();
//                                                    cerr << ":" << segments->toString() << ":" <<
//                                                            endl; //!!!!
                                                    yarp::os::Bottle segmentsAsBottle =
                                                                                segmentsAsString;
                                                    
                                                    cerr << ":" << segmentsAsBottle.size() << ":" <<
                                                            segmentsAsBottle.toString() << ":" <<
                                                            endl; //!!!!
#if defined(MpM_UseCustomStringBuffer)
                                                    _outBuffer.addString(subjName).addTab();
                                                    _outBuffer.addLong(segmentsAsBottle.size()).
                                                        addString("\t0" LINE_END_);
                                                    okSoFar = dumpSegments(_outBuffer,
                                                                           segmentsAsBottle,
                                                                           _scale);
#else // ! defined(MpM_UseCustomStringBuffer)
                                                    outBuffer << subjName.c_str() << "\t" <<
                                                                segmentsAsBottle.size() <<
                                                                "\t0" LINE_END_;
                                                    okSoFar = dumpSegments(outBuffer,
                                                                           segmentsAsBottle,
                                                                           _scale);
#endif // ! defined(MpM_UseCustomStringBuffer)
                                                }
                                                else
                                                {
                                                    cerr << "not a dictionary" << endl; //!!!!
                                                    okSoFar = false;
                                                }
                                            }
                                            else
                                            {
                                                cerr << "bad segments pointer" << endl; //!!!!
                                                okSoFar = false;
                                            }
                                        }
                                        else
                                        {
                                            cerr << "not a dictionary" << endl; //!!!!
                                            okSoFar = false;
                                        }
                                    }
                                    else
                                    {
                                        cerr << "not a string" << endl; //!!!!
                                        okSoFar = false;
                                    }
                                }
                                else
                                {
                                    cerr << "not 2 pieces in list" << endl; //!!!!
                                    okSoFar = false;
                                }
                            }
                            else
                            {
                                cerr << "bad subject pointer" << endl; //!!!!
                                okSoFar = false;
                            }
                        }
                        else
                        {
                            cerr << "subject is not a list" << endl; //!!!!
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
                        }
                    }
                }
                else
                {
                    cerr << "no subjects" << endl; //!!!!
                }
            }
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // UnrealOutputViconInputHandler::handleInput
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

void UnrealOutputViconInputHandler::setScale(const double newScale)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_D1("newScale = ", newScale); //####
    _scale = newScale;
    OD_LOG_OBJEXIT(); //####
} // UnrealOutputViconInputHandler::setScale

void UnrealOutputViconInputHandler::setSocket(const SOCKET outSocket)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_L1("outSocket = ", outSocket); //####
    _outSocket = outSocket;
    OD_LOG_OBJEXIT(); //####
} // UnrealOutputViconInputHandler::setSocket

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
