//--------------------------------------------------------------------------------------
//
//  File:       YPPEchoClient.cpp
//
//  Project:    YarpPlusPlus
//
//  Contains:   The class definition for the client of a simple Yarp++ service.
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

#include "YPPEchoClient.h"
//#include "ODEnableLogging.h"
#include "ODLogging.h"
#include "YPPEchoRequests.h"
#include "YPPServiceResponse.h"

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The class definition for the client of a simple Yarp++ service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace YarpPlusPlusExample;

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

EchoClient::EchoClient(void) :
        inherited("example/echo_")
{
    OD_LOG_ENTER();//####
    OD_LOG_EXIT_P(this);//####
} // EchoClient::EchoClient

EchoClient::~EchoClient(void)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_OBJEXIT();//####
} // EchoClient::~EchoClient

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

bool EchoClient::sendAndReceive(const yarp::os::ConstString & outgoing,
                                yarp::os::ConstString &       incoming)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_S1("outgoing = ", outgoing.c_str());//####
    OD_LOG_P1("incoming = ", &incoming);//####
    bool okSoFar = false;

    try
    {
        yarp::os::Bottle              parameters(outgoing);
        YarpPlusPlus::ServiceResponse response;
        
        if (send(YPP_ECHO_REQUEST, parameters, &response))
        {
            incoming = response.asString();
            okSoFar = true;
        }
        else
        {
            OD_LOG("! (send(YPP_ECHO_REQUEST, parameters, &response))");//####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT_B(okSoFar);//####
    return okSoFar;
} // EchoClient::sendAndReceive

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
