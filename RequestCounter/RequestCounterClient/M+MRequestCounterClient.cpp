//--------------------------------------------------------------------------------------------------
//
//  File:       M+MRequestCounterClient.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for the client of the request counter service.
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
//  Created:    2014-03-14
//
//--------------------------------------------------------------------------------------------------

#include "M+MRequestCounterClient.h"
#include "M+MRequestCounterRequests.h"

#include <mpm/M+MServiceResponse.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The class definition for the client of the request counter service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::RequestCounter;

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
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

RequestCounterClient::RequestCounterClient(void) :
    inherited("requestcounter_")
{
    OD_LOG_ENTER(); //####
    OD_LOG_EXIT_P(this); //####
} // RequestCounterClient::RequestCounterClient

RequestCounterClient::~RequestCounterClient(void)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_OBJEXIT(); //####
} // RequestCounterClient::~RequestCounterClient

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

bool RequestCounterClient::getServiceStatistics(long &   counter,
                                                double & elapsedTime)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P2("counter = ", &counter, "elapsedTime = ", &elapsedTime); //####
    bool okSoFar = false;
    
    try
    {
        yarp::os::Bottle parameters;
        ServiceResponse  response;
        
        reconnectIfDisconnected(NULL, NULL);
        if (send(MpM_STATS_REQUEST, parameters, &response))
        {
            if (2 == response.count())
            {
                yarp::os::Value retrievedCounter(response.element(0));
                yarp::os::Value retrievedElapsed(response.element(1));
                
                if (retrievedCounter.isInt() && retrievedElapsed.isDouble())
                {
                    counter = retrievedCounter.asInt();
                    elapsedTime = retrievedElapsed.asDouble();
                    okSoFar = true;
                }
                else
                {
                    OD_LOG("! (retrievedCounter.isInt() && retrievedElapsed.isDouble())"); //####
                }
            }
            else
            {
                OD_LOG("! (2 == response.count())"); //####
                OD_LOG_S1s("response = ", response.asString()); //####
            }
        }
        else
        {
            OD_LOG("! (send(MpM_STATS_REQUEST, parameters, &response))"); //####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(okSoFar); //####
    return okSoFar;
} // RequestCounterClient::getServiceStatistics

bool RequestCounterClient::pokeService(void)
{
    OD_LOG_OBJENTER(); //####
    bool okSoFar = false;
    
    try
    {
        yarp::os::Bottle parameters;
        
        if (send("blarg_blerg_blirg_blorg_blurg", parameters))
        {
            okSoFar = true;
        }
        else
        {
            OD_LOG("! (send(\"blarg_blerg_blirg_blorg_blurg\", parameters))"); //####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(okSoFar); //####
    return okSoFar;
} // RequestCounterClient::pokeService

bool RequestCounterClient::resetServiceCounters(void)
{
    OD_LOG_OBJENTER(); //####
    bool okSoFar = false;
    
    try
    {
        yarp::os::Bottle parameters;
        
        reconnectIfDisconnected(NULL, NULL);
        if (send(MpM_RESETCOUNTER_REQUEST, parameters))
        {
            okSoFar = true;
        }
        else
        {
            OD_LOG("! (send(MpM_RESETCOUNTER_REQUEST, parameters))"); //####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(okSoFar); //####
    return okSoFar;
} // RequestCounterClient::resetServiceCounters

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
