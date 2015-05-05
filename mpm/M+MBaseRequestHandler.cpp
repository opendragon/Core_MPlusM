//--------------------------------------------------------------------------------------------------
//
//  File:       mpm/M+MBaseRequestHandler.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for the minimal functionality required for an M+M request
//              handler.
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
//  Created:    2014-02-26
//
//--------------------------------------------------------------------------------------------------

#include <mpm/M+MBaseRequestHandler.h>
#include <mpm/M+MBaseService.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the minimal functionality required for an M+M request handler. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;

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

BaseRequestHandler::BaseRequestHandler(const yarp::os::ConstString & request,
                                       BaseService &                 service) :
    _owner(NULL), _name(request), _service(service)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("request = ", request); //####
    OD_LOG_P1("service = ", &service); //####
    OD_LOG_EXIT_P(this); //####
} // BaseRequestHandler::BaseRequestHandler

BaseRequestHandler::~BaseRequestHandler(void)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_OBJEXIT(); //####
} // BaseRequestHandler::~BaseRequestHandler

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void BaseRequestHandler::sendResponse(yarp::os::Bottle &           reply,
                                      yarp::os::ConnectionWriter * replyMechanism)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_S1s("reply = ", reply.toString()); //####
    OD_LOG_P1("replyMechanism = ", replyMechanism); //####
    if (replyMechanism)
    {
        OD_LOG("(replyMechanism)"); //####
        size_t messageSize = 0;
        
        if (_service.metricsAreEnabled())
        {
            reply.toBinary(&messageSize);
        }
        if (reply.write(*replyMechanism))
        {
            _service.updateResponseCounters(messageSize);
        }
        else
        {
            OD_LOG("(! reply.write(*replyMechanism))"); //####
#if defined(MpM_StallOnSendProblem)
            Stall();
#endif // defined(MpM_StallOnSendProblem)
        }
    }
    OD_LOG_OBJEXIT(); //####
} // BaseRequestHandler::sendResponse

void BaseRequestHandler::sendResponse(const yarp::os::ConstString & reply,
                                      yarp::os::ConnectionWriter *  replyMechanism)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_S1s("reply = ", reply); //####
    OD_LOG_P1("replyMechanism = ", replyMechanism); //####
    if (replyMechanism)
    {
        OD_LOG("(replyMechanism)"); //####
        yarp::os::Bottle response(reply);
        size_t           messageSize = 0;
        
        if (_service.metricsAreEnabled())
        {
            response.toBinary(&messageSize);
        }
        if (response.write(*replyMechanism))
        {
            _service.updateResponseCounters(messageSize);
        }
        else
        {
            OD_LOG("(! response(*replyMechanism))"); //####
#if defined(MpM_StallOnSendProblem)
            Stall();
#endif // defined(MpM_StallOnSendProblem)
        }
    }
    OD_LOG_OBJEXIT(); //####
} // BaseRequestHandler::sendResponse

void BaseRequestHandler::setOwner(RequestMap & owner)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("owner = ", &owner); //####
    _owner = &owner;
    OD_LOG_OBJEXIT(); //####
} // BaseRequestHandler::setOwner

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
