//--------------------------------------------------------------------------------------------------
//
//  File:       M+MTruncateFilterInputHandler.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for the input channel input handler used by the truncate filter
//              stream service.
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
//  Created:    2014-07-04
//
//--------------------------------------------------------------------------------------------------

#include "M+MTruncateFilterInputHandler.h"
#include "M+MGeneralChannel.h"

//#include "ODEnableLogging.h"
#include "ODLogging.h"

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The class definition for the input channel input handler used by the truncate filter stream
 service. */
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
# pragma mark Constructors and destructors
#endif // defined(__APPLE__)

TruncateFilterInputHandler::TruncateFilterInputHandler(void) :
    inherited(), _outChannel(NULL)
{
    OD_LOG_ENTER(); //####
    OD_LOG_EXIT_P(this); //####
} // TruncateFilterInputHandler::TruncateFilterInputHandler

TruncateFilterInputHandler::~TruncateFilterInputHandler(void)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_OBJEXIT(); //####
} // TruncateFilterInputHandler::~TruncateFilterInputHandler

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

bool TruncateFilterInputHandler::handleInput(const yarp::os::Bottle &      input,
                                             const yarp::os::ConstString & senderChannel,
                                             yarp::os::ConnectionWriter *  replyMechanism)
{
#if (! defined(OD_ENABLE_LOGGING))
# if MAC_OR_LINUX_
#  pragma unused(senderChannel,replyMechanism)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING)
    OD_LOG_OBJENTER(); //####
    OD_LOG_S2s("senderChannel = ", senderChannel, "got ", input.toString()); //####
    OD_LOG_P1("replyMechanism = ", replyMechanism); //####
    bool result = true;
    
    try
    {
        if (_outChannel)
        {
            OD_LOG("(_output)"); //####
            yarp::os::Bottle outBottle;
            
            for (int ii = 0, mm = input.size(); mm > ii; ++ii)
            {
                yarp::os::Value aValue(input.get(ii));
                
                if (aValue.isInt())
                {
                    outBottle.addInt(aValue.asInt());
                }
                else if (aValue.isDouble())
                {
                    outBottle.addInt(static_cast<int> (aValue.asDouble()));
                }
            }
            if (0 < outBottle.size())
            {
                if (! _outChannel->write(outBottle))
                {
                    OD_LOG("(! _outChannel->write(message))"); //####
#if defined(MpM_StallOnSendProblem)
                    Common::Stall();
#endif // defined(MpM_StallOnSendProblem)
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
} // TruncateFilterInputHandler::handleInput

void TruncateFilterInputHandler::setOutput(Common::GeneralChannel * output)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("output = ", output); //####
    _outChannel = output;
    OD_LOG_OBJEXIT(); //####
} // TruncateFilterInputHandler::setOutput

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
