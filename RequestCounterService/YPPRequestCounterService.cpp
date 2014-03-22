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

using namespace YarpPlusPlus;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief The operation timeout to use with YARP. */
static const float kRequestCounterServiceTimeout = 5.0;

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and destructors
#endif // defined(__APPLE__)

RequestCounterService::RequestCounterService(const yarp::os::ConstString & serviceEndpointName,
                                             const yarp::os::ConstString & serviceHostName,
                                             const yarp::os::ConstString & servicePortNumber) :
        inherited(true, YPP_REQUESTCOUNTER_CANONICAL_NAME, "The request counter service", serviceEndpointName,
                  serviceHostName, servicePortNumber), _counter(0), _lastReset(yarp::os::Time::now())
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S3("serviceEndpointName = ", serviceEndpointName.c_str(), "serviceHostName = ",//####
                 serviceHostName.c_str(), "servicePortNumber = ", servicePortNumber.c_str());//####
    setUpRequestHandlers();
    OD_SYSLOG_EXIT_P(this);//####
} // RequestCounterService::RequestCounterService

RequestCounterService::~RequestCounterService(void)
{
    OD_SYSLOG_OBJENTER();//####
    OD_SYSLOG_OBJEXIT();//####
} // RequestCounterService::~RequestCounterService

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

void RequestCounterService::countRequest(void)
{
    OD_SYSLOG_OBJENTER();//####
    try
    {
        ++_counter;
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_OBJEXIT();//####
} // RequestCounterService::countRequest

void RequestCounterService::getStatistics(long &   counter,
                                          double & elapsedTime)
{
    OD_SYSLOG_OBJENTER();//####
    try
    {
        counter = _counter;
        elapsedTime = yarp::os::Time::now() - _lastReset;
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_OBJEXIT();//####
} // RequestCounterService::getStatistics

void RequestCounterService::resetCounters(void)
{
    OD_SYSLOG_OBJENTER();//####
    try
    {
        _counter = 0;
        _lastReset = yarp::os::Time::now();
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_OBJEXIT();//####
} // RequestCounterService::resetCounters

void RequestCounterService::setUpRequestHandlers(void)
{
    OD_SYSLOG_OBJENTER();//####
    try
    {
        _requestHandlers.registerRequestHandler(new ResetRequestHandler(*this));
        _requestHandlers.registerRequestHandler(new StatsRequestHandler(*this));
        _requestHandlers.setDefaultRequestHandler(new RequestCounterDefaultRequestHandler(*this));
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_OBJEXIT();//####
} // RequestCounterService::setUpRequestHandlers

bool RequestCounterService::start(void)
{
    OD_SYSLOG_OBJENTER();//####
    bool result = false;
    
    try
    {
        if (! isStarted())
        {
            setTimeout(kRequestCounterServiceTimeout);
            inherited::start();
            if (isStarted())
            {
                
            }
            else
            {
                OD_SYSLOG("! (isStarted())");//####
            }
        }
        result = isStarted();
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_OBJEXIT_B(result);//####
    return result;
} // RequestCounterService::start

bool RequestCounterService::stop(void)
{
    OD_SYSLOG_OBJENTER();//####
    bool result = false;
    
    try
    {
        result = inherited::stop();
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_OBJEXIT_B(result);//####
    return result;
} // RequestCounterService::stop
