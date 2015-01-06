//--------------------------------------------------------------------------------------------------
//
//  File:       M+MRunningSumClient.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for the running sum client.
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
//  Created:    2014-03-18
//
//--------------------------------------------------------------------------------------------------

#include "M+MRunningSumClient.h"
#include "M+MRunningSumRequests.h"

#include <mpm/M+MServiceResponse.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for running sum client. */
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
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

RunningSumClient::RunningSumClient(void) :
    inherited("examples/runningsum_")
{
    OD_LOG_ENTER(); //####
    OD_LOG_EXIT_P(this); //####
} // RunningSumClient::RunningSumClient

RunningSumClient::~RunningSumClient(void)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_OBJEXIT(); //####
} // RunningSumClient::~RunningSumClient

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

bool RunningSumClient::addToSum(const double value,
                                double &     newSum)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("newSum = ", &newSum); //####
    bool okSoFar = false;
    
    try
    {
        yarp::os::Bottle parameters;
        ServiceResponse  response;
        
        parameters.addDouble(value);
        reconnectIfDisconnected();
        if (send(MpM_ADDTOSUM_REQUEST, parameters, &response))
        {
            if (1 == response.count())
            {
                yarp::os::Value retrieved(response.element(0));
                
                if (retrieved.isDouble())
                {
                    newSum = retrieved.asDouble();
                    okSoFar = true;
                }
                else if (retrieved.isInt())
                {
                    newSum = retrieved.asInt();
                    okSoFar = true;
                }
                else
                {
                    OD_LOG("! (retrieved.isInt())"); //####
                }
            }
            else
            {
                OD_LOG("! (1 == response.count())"); //####
                OD_LOG_S1s("response = ", response.asString()); //####
            }
        }
        else
        {
            OD_LOG("! (send(MpM_ADDTOSUM_REQUEST, parameters, &response))"); //####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(okSoFar);
    return okSoFar;
} // RunningSumClient::addToSum

bool RunningSumClient::addToSum(const DoubleVector & values,
                                double &             newSum)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P2("values = ", &values, "newSum = ", &newSum); //####
    bool okSoFar = false;
    
    try
    {
        yarp::os::Bottle parameters;
        ServiceResponse  response;
        
        if (0 < values.size())
        {
            for (DoubleVector::const_iterator it(values.begin()); values.end() != it; ++it)
            {
                parameters.addDouble(*it);
            }
        }
        if (1 <= parameters.size())
        {
            reconnectIfDisconnected();
            if (send(MpM_ADDTOSUM_REQUEST, parameters, &response))
            {
                if (1 == response.count())
                {
                    yarp::os::Value retrieved(response.element(0));
                    
                    if (retrieved.isDouble())
                    {
                        newSum = retrieved.asDouble();
                        okSoFar = true;
                    }
                    else if (retrieved.isInt())
                    {
                        newSum = retrieved.asInt();
                        okSoFar = true;
                    }
                    else
                    {
                        OD_LOG("! (retrieved.isInt())"); //####
                    }
                }
                else
                {
                    OD_LOG("! (1 == response.count())"); //####
                    OD_LOG_S1s("response = ", response.asString()); //####
                }
            }
            else
            {
                OD_LOG("! (send(MpM_ADDTOSUM_REQUEST, parameters, &response))"); //####
            }
        }
        else
        {
            OD_LOG("! (1 <= parameters.size())"); //####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(okSoFar);
    return okSoFar;
} // RunningSumClient::addToSum

bool RunningSumClient::resetSum(void)
{
    OD_LOG_OBJENTER(); //####
    bool okSoFar = false;
    
    try
    {
        yarp::os::Bottle parameters;
        
        reconnectIfDisconnected();
        if (send(MpM_RESETSUM_REQUEST, parameters))
        {
            okSoFar = true;
        }
        else
        {
            OD_LOG("! (send(MpM_RESETSUM_REQUEST, parameters))"); //####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(okSoFar);
    return okSoFar;
} // RunningSumClient::resetSum

bool RunningSumClient::startSum(void)
{
    OD_LOG_OBJENTER(); //####
    bool okSoFar = false;
    
    try
    {
        yarp::os::Bottle parameters;
        
        reconnectIfDisconnected();
        if (send(MpM_STARTSUM_REQUEST, parameters))
        {
            okSoFar = true;
        }
        else
        {
            OD_LOG("! (send(MpM_STARTSUM_REQUEST, parameters))"); //####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(okSoFar);
    return okSoFar;
} // RunningSumClient::startSum

bool RunningSumClient::stopSum(void)
{
    OD_LOG_OBJENTER(); //####
    bool okSoFar = false;
    
    try
    {
        yarp::os::Bottle parameters;
        
        reconnectIfDisconnected();
        if (send(MpM_STOPSUM_REQUEST, parameters))
        {
            okSoFar = true;
        }
        else
        {
            OD_LOG("! (send(MpM_STOPSUM_REQUEST, parameters))"); //####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(okSoFar);
    return okSoFar;
} // RunningSumClient::stopSum

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
