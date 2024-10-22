//--------------------------------------------------------------------------------------------------
//
//  File:       m+m/m+mBaseRequestHandler.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the minimal functionality required for an m+m request
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

#include "m+mBaseRequestHandler.hpp"

#include <m+m/m+mBaseService.hpp>
#include <m+m/m+mRequests.hpp>

//#include <odlEnable.h>
#include <odlInclude.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the minimal functionality required for an m+m request handler. */
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

BaseRequestHandler::BaseRequestHandler(const YarpString & request,
                                       BaseService &      service) :
    _service(service), _owner(NULL), _name(request)
{
    ODL_ENTER(); //####
    ODL_S1s("request = ", request); //####
    ODL_P1("service = ", &service); //####
    ODL_EXIT_P(this); //####
} // BaseRequestHandler::BaseRequestHandler

BaseRequestHandler::~BaseRequestHandler(void)
{
    ODL_OBJENTER(); //####
    ODL_OBJEXIT(); //####
} // BaseRequestHandler::~BaseRequestHandler

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void
BaseRequestHandler::sendOKResponse(yarp::os::ConnectionWriter * replyMechanism)
{
    ODL_OBJENTER(); //####
    ODL_P1("replyMechanism = ", replyMechanism); //####
    if (replyMechanism)
    {
        ODL_LOG("(replyMechanism)"); //####
        size_t messageSize = 0;

        _response.clear();
        _response.addString(MpM_OK_RESPONSE_);
        if (_service.metricsAreEnabled())
        {
            _response.toBinary(&messageSize);
        }
        if (_response.write(*replyMechanism))
        {
            _service.updateResponseCounters(messageSize);
        }
        else
        {
            ODL_LOG("(! _response.write(*replyMechanism))"); //####
#if defined(MpM_StallOnSendProblem)
            Stall();
#endif // defined(MpM_StallOnSendProblem)
        }
    }
    ODL_OBJEXIT(); //####
} // BaseRequestHandler::sendOKResponse

void
BaseRequestHandler::sendResponse(yarp::os::ConnectionWriter * replyMechanism)
{
    ODL_OBJENTER(); //####
    ODL_P1("replyMechanism = ", replyMechanism); //####
    if (replyMechanism)
    {
        ODL_LOG("(replyMechanism)"); //####
        if (_response.write(*replyMechanism))
        {
            if (_service.metricsAreEnabled())
            {
                size_t messageSize = 0;

                _response.toBinary(&messageSize);
                _service.updateResponseCounters(messageSize);
            }
        }
        else
        {
            ODL_LOG("(! _response.write(*replyMechanism))"); //####
#if defined(MpM_StallOnSendProblem)
            Stall();
#endif // defined(MpM_StallOnSendProblem)
        }
    }
    ODL_OBJEXIT(); //####
} // BaseRequestHandler::sendResponse

void
BaseRequestHandler::setOwner(RequestMap & owner)
{
    ODL_OBJENTER(); //####
    ODL_P1("owner = ", &owner); //####
    _owner = &owner;
    ODL_OBJEXIT(); //####
} // BaseRequestHandler::setOwner

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
