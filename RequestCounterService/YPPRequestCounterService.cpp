//--------------------------------------------------------------------------------------
//
//  File:       YPPRequestCounterService.cpp
//
//  Project:    YarpPlusPlus
//
//  Contains:   The class definition for a service that collects statistic on requests.
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
//  Created:    2014-03-14
//
//--------------------------------------------------------------------------------------

#include "YPPRequestCounterService.h"
//#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPRequestCounterDefaultRequestHandler.h"
#include "YPPRequestCounterRequests.h"
#include "YPPResetRequestHandler.h"
#include "YPPStatsRequestHandler.h"
#include <yarp/os/Time.h>

using namespace YarpPlusPlus;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

RequestCounterService::RequestCounterService(const yarp::os::ConstString & serviceEndpointName,
                                             const yarp::os::ConstString & serviceHostName,
                                             const yarp::os::ConstString & servicePortNumber) :
        inherited(true, YPP_REQUESTCOUNTER_CANONICAL_NAME, serviceEndpointName, serviceHostName, servicePortNumber),
        _counter(0), _lastReset(yarp::os::Time::now())
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S3("serviceEndpointName = ", serviceEndpointName.c_str(), "serviceHostName = ",//####
                 serviceHostName.c_str(), "servicePortNumber = ", servicePortNumber.c_str());//####
    setUpRequestHandlers();
    OD_SYSLOG_EXIT();//####
} // RequestCounterService::RequestCounterService

RequestCounterService::~RequestCounterService(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // RequestCounterService::~RequestCounterService

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

void RequestCounterService::countRequest(void)
{
    OD_SYSLOG_ENTER();//####
    ++_counter;
    OD_SYSLOG_EXIT();//####
} // RequestCounterService::countRequest

void RequestCounterService::getStatistics(long &   counter,
                                          double & elapsedTime)
{
    OD_SYSLOG_ENTER();//####
    counter = _counter;
    elapsedTime = yarp::os::Time::now() - _lastReset;
    OD_SYSLOG_EXIT();//####
} // RequestCounterService::getStatistics

void RequestCounterService::resetCounters(void)
{
    OD_SYSLOG_ENTER();//####
    _counter = 0;
    _lastReset = yarp::os::Time::now();
    OD_SYSLOG_EXIT();//####
} // RequestCounterService::resetCounters

void RequestCounterService::setUpRequestHandlers(void)
{
    OD_SYSLOG_ENTER();//####
    _requestHandlers.registerRequestHandler(new ResetRequestHandler(*this));
    _requestHandlers.registerRequestHandler(new StatsRequestHandler(*this));
    _requestHandlers.setDefaultRequestHandler(new RequestCounterDefaultRequestHandler(*this));
    OD_SYSLOG_EXIT();//####
} // RequestCounterService::setUpRequestHandlers

bool RequestCounterService::start(void)
{
    OD_SYSLOG_ENTER();//####
    if (! isStarted())
    {
        BaseService::start();
        if (isStarted())
        {
            
        }
    }
    OD_SYSLOG_EXIT_B(isStarted());//####
    return isStarted();
} // RequestCounterService::start

bool RequestCounterService::stop(void)
{
    OD_SYSLOG_ENTER();//####
    bool result = BaseService::stop();
    
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // RequestCounterService::stop
