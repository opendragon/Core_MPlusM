//--------------------------------------------------------------------------------------------------
//
//  File:       M+MRunningSumInputHandler.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for the custom control channel input handler used by the
//              alternative Running Sum adapter.
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
//  Created:    2014-04-15
//
//--------------------------------------------------------------------------------------------------

#include "M+MRunningSumInputHandler.h"
#include "M+MRunningSumAdapterData.h"
#include "M+MRunningSumClient.h"
#include "M+MRunningSumRequests.h"

#include <mpm/M+MAdapterChannel.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the custom control channel input handler used by the alternative
 Running Sum adapter. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Example;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
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

RunningSumInputHandler::RunningSumInputHandler(RunningSumAdapterData & shared) :
    inherited(), _shared(shared)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P1("shared = ", &shared); //####
    OD_LOG_EXIT_P(this); //####
} // RunningSumInputHandler::RunningSumInputHandler

RunningSumInputHandler::~RunningSumInputHandler(void)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_OBJEXIT(); //####
} // RunningSumInputHandler::~RunningSumInputHandler

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
bool RunningSumInputHandler::handleInput(const yarp::os::Bottle &      input,
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
        int howMany = input.size();
        
        if (0 < howMany)
        {
            AdapterChannel *   theOutput = _shared.getOutput();
            DoubleVector       values;
            double             outValue;
            RunningSumClient * theClient = (RunningSumClient *) _shared.getClient();
            
            if (theClient && theOutput)
            {
                // We might have values and commands intermixed; process the whole input, one
                // segment at a time.
                for (int ii = 0; ii < howMany; ++ii)
                {
                    yarp::os::Value argValue(input.get(ii));
                    
                    if (argValue.isString())
                    {
                        yarp::os::ConstString argString(argValue.asString());
                        
                        if (values.size())
                        {
                            DoubleVector::size_type soFar = values.size();
                            
                            if (1 == soFar)
                            {
                                _shared.lock();
                                if (theClient->addToSum(values[0], outValue))
                                {
                                    yarp::os::Bottle message;
                                    
                                    message.addDouble(outValue);
                                    if (! theOutput->write(message))
                                    {
                                        OD_LOG("(! theOutput->write(message))"); //####
#if defined(MpM_StallOnSendProblem)
                                        Stall();
#endif // defined(MpM_StallOnSendProblem)
                                    }
                                }
                                else
                                {
                                    OD_LOG("! (theClient->startSum())"); //####
                                }
                                _shared.unlock();
                            }
                            else
                            {
                                _shared.lock();
                                if (theClient->addToSum(values, outValue))
                                {
                                    yarp::os::Bottle message;
                                    
                                    message.addDouble(outValue);
                                    if (! theOutput->write(message))
                                    {
                                        OD_LOG("(! theOutput->write(message))"); //####
#if defined(MpM_StallOnSendProblem)
                                        Stall();
#endif // defined(MpM_StallOnSendProblem)
                                    }
                                }
                                else
                                {
                                    OD_LOG("! (theClient->startSum())"); //####
                                }
                                _shared.unlock();
                            }
                            values.clear();
                        }
                        if (argString == MpM_RESETSUM_REQUEST)
                        {
                            _shared.lock();
                            if (theClient->resetSum())
                            {
                            
                            }
                            else
                            {
                                OD_LOG("! (theClient->resetSum())"); //####
                            }
                            _shared.unlock();
                        }
                        else if (argString == MpM_QUIT_REQUEST)
                        {
                            _shared.deactivate();
                        }
                        else if (argString == MpM_STARTSUM_REQUEST)
                        {
                            _shared.lock();
                            if (theClient->startSum())
                            {
                            
                            }
                            else
                            {
                                OD_LOG("! (theClient->startSum())"); //####
                            }
                            _shared.unlock();
                        }
                        else if (argString == MpM_STOPSUM_REQUEST)
                        {
                            _shared.lock();
                            if (theClient->stopSum())
                            {
                            
                            }
                            else
                            {
                                OD_LOG("! (theClient->startSum())"); //####
                            }
                            _shared.unlock();
                        }
                    }
                    else if (argValue.isInt())
                    {
                        values.push_back(static_cast<double>(argValue.asInt()));
                    }
                    else if (argValue.isDouble())
                    {
                        values.push_back(argValue.asDouble());
                    }
                }
                if (values.size())
                {
                    DoubleVector::size_type soFar = values.size();
                    
                    if (1 == soFar)
                    {
                        _shared.lock();
                        if (theClient->addToSum(values[0], outValue))
                        {
                            yarp::os::Bottle message;
                            
                            message.addDouble(outValue);
                            if (! theOutput->write(message))
                            {
                                OD_LOG("(! theOutput->write(message))"); //####
#if defined(MpM_StallOnSendProblem)
                                Stall();
#endif // defined(MpM_StallOnSendProblem)
                            }
                        }
                        else
                        {
                            OD_LOG("! (theClient->startSum())"); //####
                        }
                        _shared.unlock();
                    }
                    else
                    {
                        _shared.lock();
                        if (theClient->addToSum(values, outValue))
                        {
                            yarp::os::Bottle message;
                            
                            message.addDouble(outValue);
                            if (! theOutput->write(message))
                            {
                                OD_LOG("(! theOutput->write(message))"); //####
#if defined(MpM_StallOnSendProblem)
                                Stall();
#endif // defined(MpM_StallOnSendProblem)
                            }
                        }
                        else
                        {
                            OD_LOG("! (theClient->startSum())"); //####
                        }
                        _shared.unlock();
                    }
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
} // RunningSumInputHandler::handleInput
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
