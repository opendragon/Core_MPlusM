//--------------------------------------------------------------------------------------------------
//
//  File:       m+mSendToMQOutputInputHandler.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the input handler used by the SendToMQ output service.
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
//  Created:    2015-07-26
//
//--------------------------------------------------------------------------------------------------

#include "m+mSendToMQOutputInputHandler.hpp"
#include "m+mSendToMQOutputService.hpp"

//#include <odlEnable.h>
#include <odlInclude.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the input handler used by the %SendToMQ output service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::SendToMQ;
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

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

SendToMQOutputInputHandler::SendToMQOutputInputHandler(SendToMQOutputService & owner) :
    inherited(), _owner(owner)
{
    ODL_ENTER(); //####
    ODL_P1("owner = ", &owner); //####
    ODL_EXIT_P(this); //####
} // SendToMQOutputInputHandler::SendToMQOutputInputHandler

SendToMQOutputInputHandler::~SendToMQOutputInputHandler(void)
{
    ODL_OBJENTER(); //####
    ODL_OBJEXIT(); //####
} // SendToMQOutputInputHandler::~SendToMQOutputInputHandler

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
bool
SendToMQOutputInputHandler::handleInput(const yarp::os::Bottle &     input,
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
        if (_owner.isActive())
        {
            ODL_LOG("(_owner.isActive())"); //####
#if (! defined(MpM_UseCustomStringBuffer))
            std::stringstream outBuffer;
#endif // ! defined(MpM_UseCustomStringBuffer)

#if defined(MpM_UseCustomStringBuffer)
            Utilities::ConvertMessageToJSON(_outBuffer, input);
#else // ! defined(MpM_UseCustomStringBuffer)
            Utilities::ConvertMessageToJSON(outBuffer, input);
#endif // ! defined(MpM_UseCustomStringBuffer)
            size_t      outLength;
            std::string buffAsString;

#if defined(MpM_UseCustomStringBuffer)
            buffAsString = _outBuffer.getString(outLength);
#else // ! defined(MpM_UseCustomStringBuffer)
            buffAsString = outBuffer.str();
            outLength = buffAsString.length();
#endif // ! defined(MpM_UseCustomStringBuffer)
            if (buffAsString.length())
            {
                ODL_LOG("(buffAsString.length())"); //####
                SendReceiveCounters toBeAdded(0, 0, outLength, 1);

                _owner.sendMessage(buffAsString, outLength);
                _owner.incrementAuxiliaryCounters(toBeAdded);
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
} // SendToMQOutputInputHandler::handleInput
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
