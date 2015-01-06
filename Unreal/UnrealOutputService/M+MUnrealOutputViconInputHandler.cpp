//--------------------------------------------------------------------------------------------------
//
//  File:       M+MUnrealOutputViconInputHandler.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for the Vicon DataStream input channel input handler used by
//              the Unreal output service.
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
//  Created:    2014-11-18
//
//--------------------------------------------------------------------------------------------------

#include "M+MUnrealOutputViconInputHandler.h"
#include "M+MUnrealOutputService.h"

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the Vicon DataStream input channel input handler used by the Unreal
 output service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Unreal;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

#define LINE_END "\r\n"

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

/*! @brief Convert list of segments into a character stream.
 @param outBuffer The destination character stream.
 @param segmentsAsBottle The segments to write out.
 @param scale The translation scale to use.
 @returns @c true if the segments were properly structured and @c false otherwise. */
static bool dumpSegments(std::stringstream & outBuffer,
                         yarp::os::Bottle &  segmentsAsBottle,
                         const double        scale)
{
    OD_LOG_ENTER(); //####
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
                        yarp::os::ConstString keyString = keyValue.asString();
                        yarp::os::Bottle *    valueList = valueValue.asList();
                        
                        outBuffer << keyString.c_str();
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
									std::cerr << "value not an integer or a float" <<
                                                std::endl; //!!!!
                                    okSoFar = false;
                                }
                                if (okSoFar)
                                {
                                    if (3 > jj)
                                    {
                                        elementAsDouble *= scale;
                                    }
                                    outBuffer << "\t" << elementAsDouble;
                                }
                            }
							outBuffer << LINE_END;
                        }
                        else
                        {
							std::cerr << "bad list pointer or incorrect list size" <<
                                        std::endl; //!!!!
                            okSoFar = false;
                        }
                    }
                    else
                    {
						std::cerr << "segment not a string and a list" << std::endl; //!!!!
                        okSoFar = false;
                    }
                }
                else
                {
					std::cerr << "segment not a 2-element list" << std::endl; //!!!!
                    okSoFar = false;
                }
            }
            else
            {
				std::cerr << "segment not a list" << std::endl; //!!!!
                okSoFar = false;
            }
        }
    }
    else
    {
		std::cerr << "no segments" << std::endl; //!!!!
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
bool UnrealOutputViconInputHandler::handleInput(const yarp::os::Bottle &      input,
                                                const yarp::os::ConstString & senderChannel,
                                                yarp::os::ConnectionWriter *  replyMechanism,
                                                const size_t                  numBytes)
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
			std::cerr << "invalid socket" << std::endl; //!!!!
		}
		else
		{
            int numSubjects = input.size();

            if (0 < numSubjects)
            {
                bool              okSoFar = true;
                std::stringstream outBuffer;
                
//				std::cerr << "# subjects = " << numSubjects << std::endl; //!!!!
                outBuffer << numSubjects << LINE_END;
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
                                yarp::os::Value & secondValue = asBottle->get(1);
                                
                                if (firstValue.isString() && secondValue.isDict())
                                {
                                    yarp::os::ConstString subjName = firstValue.asString();
                                    yarp::os::Property *  segments = secondValue.asDict();
                                    
//									std::cerr << subjName.c_str() << std::endl; //!!!!
                                    if (segments)
                                    {
											yarp::os::ConstString segmentsAsString(segments->toString());
											//std::cerr << ":" << segments->toString() << ":" << std::endl; //!!!!
											yarp::os::Bottle      segmentsAsBottle(segmentsAsString);

											std::cerr << ":" << segmentsAsBottle.size() << ":" << segmentsAsBottle.toString() << ":" << std::endl; //!!!!
                                        outBuffer << subjName.c_str() << "\t" <<
                                                    segmentsAsBottle.size() << "\t0" << LINE_END;
                                        okSoFar = dumpSegments(outBuffer, segmentsAsBottle,
                                                               _scale);
                                    }
									else
									{
										std::cerr << "bad segments pointer" << std::endl; //!!!!
										okSoFar = false;
									}
                                }
                                else
                                {
									std::cerr << "not a string or not a dictionary" <<
                                                std::endl; //!!!!
                                    okSoFar = false;
                                }
                            }
                            else
                            {
								std::cerr << "not 2 pieces in list" << std::endl; //!!!!
                                okSoFar = false;
                            }
                        }
                        else
                        {
							std::cerr << "bad subject pointer" << std::endl; //!!!!
                            okSoFar = false;
                        }
                    }
                    else
                    {
						std::cerr << "subject is not a list" << std::endl; //!!!!
                        okSoFar = false;
                    }
                }
                if (okSoFar)
                {
                    outBuffer << "END" << LINE_END;
                    std::string outString(outBuffer.str());    
                    int         retVal = send(_outSocket, outString.c_str(), outString.length(), 0);

//					std::cerr << "send--> " << retVal << std::endl; //!!!!
                    if (0 > retVal)
                    {
                        _owner.deactivateConnection();
                    }
				}
            }
			else
			{
				std::cerr << "no subjects" << std::endl; //!!!!
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
