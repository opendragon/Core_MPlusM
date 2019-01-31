//--------------------------------------------------------------------------------------------------
//
//  File:       m+mRandomNumberInputHandler.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the custom data channel input handler used by the Random
//              Number adapter.
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
//  Created:    2015-05-27
//
//--------------------------------------------------------------------------------------------------

#include "m+mRandomNumberInputHandler.hpp"
#include "m+mRandomNumberAdapterData.hpp"
#include "m+mRandomNumberClient.hpp"

#include <m+m/m+mBaseChannel.hpp>

//#include <odlEnable.h>
#include <odlInclude.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the custom data channel input handler used by the Random Number
 adapter. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Example;

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

RandomNumberInputHandler::RandomNumberInputHandler(RandomNumberAdapterData & shared) :
    inherited(), _shared(shared)
{
    ODL_ENTER(); //####
    ODL_P1("shared = ", &shared); //####
    ODL_EXIT_P(this); //####
} // RandomNumberInputHandler::RandomNumberInputHandler

RandomNumberInputHandler::~RandomNumberInputHandler(void)
{
    ODL_OBJENTER(); //####
    ODL_OBJEXIT(); //####
} // RandomNumberInputHandler::~RandomNumberInputHandler

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
bool
RandomNumberInputHandler::handleInput(const yarp::os::Bottle &     input,
                                      const YarpString &           senderChannel,
                                      yarp::os::ConnectionWriter * replyMechanism,
                                      const size_t                 numBytes)
{
#if (! defined(ODL_ENABLE_LOGGING_))
# if MAC_OR_LINUX_
#  pragma unused(senderChannel,replyMechanism,numBytes)
# endif // MAC_OR_LINUX_
#endif // ! defined(ODL_ENABLE_LOGGING_)
    ODL_OBJENTER(); //####
    ODL_S2s("senderChannel = ", senderChannel, "got ", input.toString()); //####
    ODL_P1("replyMechanism = ", replyMechanism); //####
    ODL_I1("numBytes = ", numBytes); //####
    bool result = true;

    try
    {
        if (0 < input.size())
        {
            BaseChannel *        theOutput = _shared.getOutput();
            RandomNumberClient * theClient = (RandomNumberClient *) _shared.getClient();

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
                            ODL_LOG("(! theOutput->write(message))"); //####
#if defined(MpM_StallOnSendProblem)
                            Stall();
#endif // defined(MpM_StallOnSendProblem)
                        }
                        _shared.unlock();
                    }
                    else
                    {
                        ODL_LOG("! (theClient->getRandomNumbers(count, randResult))"); //####
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
                            ODL_LOG("(! theOutput->write(message))"); //####
#if defined(MpM_StallOnSendProblem)
                            Stall();
#endif // defined(MpM_StallOnSendProblem)
                        }
                        _shared.unlock();
                    }
                    else
                    {
                        ODL_LOG("! (theClient->getOneRandomNumber(randResult))"); //####
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
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT_B(result); //####
    return result;
} // RandomNumberInputHandler::handleInput
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
