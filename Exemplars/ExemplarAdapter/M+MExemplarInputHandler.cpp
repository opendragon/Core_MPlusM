//--------------------------------------------------------------------------------------------------
//
//  File:       M+MExemplarInputHandler.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for the custom data channel input handler used by the exemplar
//              adapter.
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
//  Created:    2014-09-15
//
//--------------------------------------------------------------------------------------------------

#include "M+MExemplarInputHandler.h"
#include "M+MExemplarAdapterData.h"
#include "M+MExemplarClient.h"

#include <mpm/M+MAdapterChannel.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the custom data channel input handler used by the exemplar
 adapter. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Exemplar;

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

ExemplarInputHandler::ExemplarInputHandler(ExemplarAdapterData & shared) :
    inherited(), _shared(shared)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P1("shared = ", &shared); //####
    OD_LOG_EXIT_P(this); //####
} // ExemplarInputHandler::ExemplarInputHandler

ExemplarInputHandler::~ExemplarInputHandler(void)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_OBJEXIT(); //####
} // ExemplarInputHandler::~ExemplarInputHandler

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
bool ExemplarInputHandler::handleInput(const yarp::os::Bottle &      input,
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
        if (0 < input.size())
        {
            AdapterChannel * theOutput = _shared.getOutput();
            ExemplarClient * theClient = (ExemplarClient *) _shared.getClient();
            
            if (theClient && theOutput)
            {
                int             count;
                yarp::os::Value argValue(input.get(0));
                
                if (argValue.isInt())
                {
                    count = argValue.asInt();
                }
                else if (argValue.isDouble())
                {
                    count = static_cast<int>(argValue.asDouble());
                }
                else
                {
                    count = 1;
                }
                if (0 > count)
                {
                    count = 1;
                }
                if (1 < count)
                {
                    DoubleVector randResult;
                    
                    if (theClient->getRandomNumbers(count, randResult))
                    {
                        yarp::os::Bottle message;
                        
                        if (0 < randResult.size())
                        {
                            for (DoubleVector::const_iterator it(randResult.begin());
                                 randResult.end() != it; ++it)
                            {
                                message.addDouble(*it);
                            }
                        }
                        _shared.lock();
                        if (! theOutput->write(message))
                        {
                            OD_LOG("(! theOutput->write(message))"); //####
#if defined(MpM_StallOnSendProblem)
                            Stall();
#endif // defined(MpM_StallOnSendProblem)
                        }
                        _shared.unlock();
                    }
                    else
                    {
                        OD_LOG("! (theClient->getRandomNumbers(count, randResult))"); //####
                    }
                }
                else if (0 < count)
                {
                    double randResult;
                    
                    if (theClient->getOneRandomNumber(randResult))
                    {
                        yarp::os::Bottle message;
                        
                        message.addDouble(randResult);
                        _shared.lock();
                        if (! theOutput->write(message))
                        {
                            OD_LOG("(! theOutput->write(message))"); //####
#if defined(MpM_StallOnSendProblem)
                            Stall();
#endif // defined(MpM_StallOnSendProblem)
                        }
                        _shared.unlock();
                    }
                    else
                    {
                        OD_LOG("! (theClient->getOneRandomNumber(randResult))"); //####
                    }
                }
                else
                {
                    _shared.deactivate();
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
} // ExemplarInputHandler::handleInput
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
