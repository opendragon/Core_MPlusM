//--------------------------------------------------------------------------------------------------
//
//  File:       m+m/m+mServiceRequest.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for an m+m request.
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
//  Created:    2014-02-06
//
//--------------------------------------------------------------------------------------------------

#include "m+mServiceRequest.hpp"

#include <m+m/m+mClientChannel.hpp>
#include <m+m/m+mServiceResponse.hpp>

//#include <odlEnable.h>
#include <odlInclude.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for an m+m request. */
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

ServiceRequest::ServiceRequest(const YarpString & requestName) :
    _name(requestName), _parameters()
{
    ODL_ENTER(); //####
    ODL_S1s("requestName = ", requestName); //####
    ODL_EXIT_P(this); //####
} // ServiceRequest::ServiceRequest

ServiceRequest::ServiceRequest(const YarpString &       requestName,
                               const yarp::os::Bottle & parameters) :
    _name(requestName), _parameters(parameters)
{
    ODL_ENTER(); //####
    ODL_S2s("requestName = ", requestName, "parameters = ", parameters.toString()); //####
    ODL_I1("parameter size = ", parameters.size()); //####
    for (int ii = 0; ii < parameters.size(); ++ii)
    {
        ODL_S1s("parameter = ", parameters.get(ii).asString()); //####
    }
    ODL_EXIT_P(this); //####
} // ServiceRequest::ServiceRequest

ServiceRequest::~ServiceRequest(void)
{
    ODL_OBJENTER(); //####
    ODL_OBJEXIT(); //####
} // ServiceRequest::~ServiceRequest

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

bool
ServiceRequest::send(ClientChannel & usingChannel)
{
    ODL_OBJENTER(); //####
    ODL_P1("usingChannel = ", &usingChannel); //####
    bool result = false;

    try
    {
        yarp::os::Bottle message;

        ODL_I1("parameter size = ", _parameters.size()); //####
        for (int ii = 0; ii < _parameters.size(); ++ii)
        {
            ODL_S1s("parameter = ", _parameters.get(ii).asString()); //####
        }
        message.addString(_name);
        message.append(_parameters);
        ODL_S1s("message <- ", message.toString()); //####
        if (usingChannel.writeBottle(message))
        {
            result = true;
        }
        else
        {
            ODL_LOG("(! usingChannel.writeBottle(message))"); //####
#if defined(MpM_StallOnSendProblem)
            Stall();
#endif // defined(MpM_StallOnSendProblem)
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT_B(result); //####
    return result;
} // ServiceRequest::send

bool
ServiceRequest::send(ClientChannel &   usingChannel,
                     ServiceResponse & response)
{
    ODL_OBJENTER(); //####
    ODL_P2("usingChannel = ", &usingChannel, "response = ", &response); //####
    bool result = false;

    try
    {
        yarp::os::Bottle holder;
        yarp::os::Bottle message;

        ODL_I1("parameter size = ", _parameters.size()); //####
        for (int ii = 0; ii < _parameters.size(); ++ii)
        {
            ODL_S1s("parameter = ", _parameters.get(ii).asString()); //####
        }
        message.addString(_name);
        message.append(_parameters);
        ODL_S1s("message <- ", message.toString()); //####
        if (usingChannel.writeBottle(message, holder))
        {
            ODL_S1s("got ", holder.toString()); //####
            response = holder;
            result = true;
        }
        else
        {
            ODL_LOG("! (usingChannel.writeBottle(message, holder))"); //####
#if defined(MpM_StallOnSendProblem)
            Stall();
#endif // defined(MpM_StallOnSendProblem)
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT_B(result); //####
    return result;
} // ServiceRequest::send

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
