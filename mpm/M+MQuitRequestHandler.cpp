//--------------------------------------------------------------------------------------------------
//
//  File:       mpm/M+MQuitRequestHandler.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for the request handler for the standard 'quit' request.
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
//  Created:    2014-04-27
//
//--------------------------------------------------------------------------------------------------

#include "M+MQuitRequestHandler.h"

#include <mpm/M+MBaseService.h>
#include <mpm/M+MRequests.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The class definition for the request handler for the standard 'quit' request. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief The protocol version number for the 'quit' request. */
#define QUIT_REQUEST_VERSION_NUMBER "1.0"

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

QuitRequestHandler::QuitRequestHandler(BaseService & service) :
    inherited(MpM_QUIT_REQUEST), _service(service)
{
    OD_LOG_ENTER(); //####
    OD_LOG_EXIT_P(this); //####
} // QuitRequestHandler::QuitRequestHandler

QuitRequestHandler::~QuitRequestHandler(void)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_OBJEXIT(); //####
} // QuitRequestHandler::~QuitRequestHandler

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

void QuitRequestHandler::fillInAliases(StringVector & alternateNames)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("alternateNames = ", &alternateNames); //####
    alternateNames.push_back("q");
    OD_LOG_OBJEXIT(); //####
} // QuitRequestHandler::fillInAliases

void QuitRequestHandler::fillInDescription(const yarp::os::ConstString & request,
                                           yarp::os::Property &          info)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_S1s("request = ", request); //####
    OD_LOG_P1("info = ", &info); //####
    try
    {
        info.put(MpM_REQREP_DICT_REQUEST_KEY, request);
        info.put(MpM_REQREP_DICT_VERSION_KEY, QUIT_REQUEST_VERSION_NUMBER);
        info.put(MpM_REQREP_DICT_DETAILS_KEY, "Stop the service\n"
                 "Inputs: nothing\n"
                 "Outputs: nothing");
        yarp::os::Value    keywords;
        yarp::os::Bottle * asList = keywords.asList();
        
        asList->addString(request);
        info.put(MpM_REQREP_DICT_KEYWORDS_KEY, keywords);
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT(); //####
} // QuitRequestHandler::fillInDescription

bool QuitRequestHandler::processRequest(const yarp::os::ConstString & request,
                                        const yarp::os::Bottle &      restOfInput,
                                        const yarp::os::ConstString & senderChannel,
                                        yarp::os::ConnectionWriter *  replyMechanism)
{
#if (! defined(OD_ENABLE_LOGGING))
# if MAC_OR_LINUX_
#  pragma unused(request,restOfInput,senderChannel)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING)
    OD_LOG_OBJENTER(); //####
    OD_LOG_S3s("request = ", request, "restOfInput = ", restOfInput.toString(), //####
               "senderChannel = ", senderChannel); //####
    OD_LOG_P1("replyMechanism = ", replyMechanism); //####
    bool result = true;
    
    try
    {
        MplusM::StopRunning();
        if (replyMechanism)
        {
            OD_LOG("(replyMechanism)"); //####
            yarp::os::Bottle reply(MpM_OK_RESPONSE);
            
            OD_LOG_S1s("reply <- ", reply.toString()); //####
            if (! reply.write(*replyMechanism))
            {
                OD_LOG("(! reply.write(*replyMechanism))"); //####
#if defined(MpM_StallOnSendProblem)
                Common::Stall();
#endif // defined(MpM_StallOnSendProblem)
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
} // QuitRequestHandler::processRequest

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
