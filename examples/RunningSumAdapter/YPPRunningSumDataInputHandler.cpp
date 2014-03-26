//--------------------------------------------------------------------------------------
//
//  File:       YPPRunningSumDataInputHandler.cpp
//
//  Project:    YarpPlusPlus
//
//  Contains:   The class definition for the custom data channel input handler used by the example running sum adapter.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2014 by HPlus Technologies Ltd. and Simon Fraser University.
//
//              All rights reserved. Redistribution and use in source and binary forms,
//              with or without modification, are permitted provided that the following
//              conditions are met:
//                * Redistributions of source code must retain the above copyright
//                  notice, this list of conditions and the following disclaimer.
//                * Redistributions in binary form must reproduce the above copyright
//                  notice, this list of conditions and the following disclaimer in the
//                  documentation and/or other materials provided with the
//                  distribution.
//                * Neither the name of the copyright holders nor the names of its
//                  contributors may be used to endorse or promote products derived
//                  from this software without specific prior written permission.
//
//              THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
//              "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
//              LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
//              PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
//              OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
//              SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
//              LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
//              DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
//              THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
//              (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
//              OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
//  Created:    2014-03-24
//
//--------------------------------------------------------------------------------------

#include "YPPRunningSumDataInputHandler.h"
//#define OD_ENABLE_LOGGING /* */
#include "ODLogging.h"
//#include "YPPCommon.h"

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The class definition for the custom data channel input handler used by the example running sum adapter. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace YarpPlusPlusExample;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

//#define REPORT_CONTACT_DETAILS /* Report details of the open() method. */

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and destructors
#endif // defined(__APPLE__)

RunningSumDataInputHandler::RunningSumDataInputHandler(RunningSumClient * client) :
        inherited(), _client(client)
{
    OD_LOG_ENTER();//####
    OD_LOG_EXIT_P(this);//####
} // RunningSumDataInputHandler::RunningSumDataInputHandler

RunningSumDataInputHandler::~RunningSumDataInputHandler(void)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_OBJEXIT();//####
} // RunningSumDataInputHandler::~RunningSumDataInputHandler

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

bool RunningSumDataInputHandler::handleInput(const yarp::os::Bottle &      input,
                                             const yarp::os::ConstString & senderPort,
                                             yarp::os::ConnectionWriter *  replyMechanism)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_S2("senderPort = ", senderPort.c_str(), "got ", input.toString().c_str());//####
    OD_LOG_P1("replyMechanism = ", replyMechanism);//####
    bool result = false;
    
    try
    {
        if (0 < input.size())
        {
#if 0
            result = _service.processRequest(input.get(0).toString(), input.tail(), senderPort, replyMechanism);
#endif//0
        }
        else
        {
            result = true;
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT_B(result);//####
    return result;
} // RunningSumDataInputHandler::handleInput

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
