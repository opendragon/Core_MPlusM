//--------------------------------------------------------------------------------------
//
//  File:       YPPServiceRequest.cpp
//
//  Project:    YarpPlusPlus
//
//  Contains:   The class definition for a Yarp++ request.
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
//  Created:    2014-02-06
//
//--------------------------------------------------------------------------------------

#include "YPPServiceRequest.h"
//#include "ODEnableLogging.h"
#include "ODLogging.h"
#include "YPPEndpoint.h"
#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wc++11-extensions"
# pragma clang diagnostic ignored "-Wdocumentation"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# pragma clang diagnostic ignored "-Wpadded"
# pragma clang diagnostic ignored "-Wshadow"
# pragma clang diagnostic ignored "-Wunused-parameter"
# pragma clang diagnostic ignored "-Wweak-vtables"
#endif // defined(__APPLE__)
#include <yarp/os/Time.h>
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The class definition for a Yarp++ request. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace YarpPlusPlus;

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

ServiceRequest::ServiceRequest(const yarp::os::ConstString & requestName,
                               const Package &               parameters) :
        _name(requestName), _holder(), _parameters(parameters)
{
    OD_LOG_ENTER();//####
    OD_LOG_S1("requestName = ", requestName.c_str());//####
    OD_LOG_P1("parameters = ", parameters.toString().c_str());//####
    OD_LOG_LL1("parameter size = ", parameters.size());//####
    for (int ii = 0; ii < parameters.size(); ++ii)
    {
        OD_LOG_S1("parameter = ", parameters.get(ii).asString().c_str());//####
    }
    OD_LOG_EXIT_P(this);//####
} // ServiceRequest::ServiceRequest

ServiceRequest::~ServiceRequest(void)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_OBJEXIT();//####
} // ServiceRequest::~ServiceRequest

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

bool ServiceRequest::send(Channel &         usingChannel,
                          ServiceResponse * response)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_P2("usingChannel = ", &usingChannel, "response = ", response);//####
    bool result = false;
    
    try
    {
        Package message;
        
        OD_LOG_LL1("parameter size = ", _parameters.size());//####
        for (int ii = 0; ii < _parameters.size(); ++ii)
        {
            OD_LOG_S1("parameter = ", _parameters.get(ii).asString().c_str());//####
        }
        message.addString(_name);
        message.append(_parameters);
        OD_LOG_S1("message <- ", message.toString().c_str());//####
        if (response)
        {
            _holder.clear();
            if (usingChannel.write(message, _holder))
            {
                OD_LOG("(usingChannel.write(message, _holder))");//####
                OD_LOG_S1("got ", _holder.toString().c_str());//####
                *response = _holder;
                result = true;
            }
            else
            {
                OD_LOG("! (usingChannel.write(message, _holder))");//####
            }
        }
        else if (usingChannel.write(message))
        {
            result = true;
        }
        else
        {
            OD_LOG("(! usingChannel.write(message))");//####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT_B(result);//####
    return result;
} // ServiceRequest::send

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
