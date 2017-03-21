//--------------------------------------------------------------------------------------------------
//
//  File:       m+mTunnelClient.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the client of the Tunnel service.
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
//  Created:    2015-02-11
//
//--------------------------------------------------------------------------------------------------

#include "m+mTunnelClient.hpp"
#include "m+mTunnelRequests.hpp"

#include <m+m/m+mServiceResponse.hpp>

//#include <ODEnableLogging.h>
#include <ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the client of the %Tunnel service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Tunnel;

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

TunnelClient::TunnelClient(void) :
    inherited("tunnel_")
{
    ODL_ENTER(); //####
    ODL_EXIT_P(this); //####
} // TunnelClient::TunnelClient

TunnelClient::~TunnelClient(void)
{
    ODL_OBJENTER(); //####
    ODL_OBJEXIT(); //####
} // TunnelClient::~TunnelClient

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

bool
TunnelClient::getAddress(YarpString & address,
                         int &        port)
{
    ODL_OBJENTER(); //####
    ODL_P2("address = ", &address, "port = ", &port); //####
    bool okSoFar = false;

    try
    {
        yarp::os::Bottle parameters;
        ServiceResponse  response;

        reconnectIfDisconnected();
        if (send(MpM_WHERE_REQUEST_, parameters, response))
        {
            if (MpM_EXPECTED_WHERE_RESPONSE_SIZE_ == response.count())
            {
                yarp::os::Value retrievedAddress(response.element(0));
                yarp::os::Value retrievedPort(response.element(1));

                if (retrievedAddress.isString() && retrievedPort.isInt())
                {
                    address = retrievedAddress.asString();
                    port = retrievedPort.asInt();
                    okSoFar = true;
                }
                else
                {
                    ODL_LOG("! (retrievedCounter.isString() && retrievedElapsed.isInt())"); //####
                }
            }
            else
            {
                ODL_LOG("! (MpM_EXPECTED_WHERE_RESPONSE_SIZE_ == response.count())"); //####
                ODL_S1s("response = ", response.asString()); //####
            }
        }
        else
        {
            ODL_LOG("! (send(MpM_STATS_REQUEST_, parameters, response))"); //####
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT_B(okSoFar); //####
    return okSoFar;
} // TunnelClient::getAddress

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
