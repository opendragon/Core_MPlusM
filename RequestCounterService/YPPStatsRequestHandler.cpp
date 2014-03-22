//--------------------------------------------------------------------------------------
//
//  File:       YPPStatsRequestHandler.cpp
//
//  Project:    YarpPlusPlus
//
//  Contains:   The class definition for the request handler for a 'stats' request.
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

#include "YPPStatsRequestHandler.h"
//#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPRequestCounterRequests.h"
#include "YPPRequestCounterService.h"

using namespace YarpPlusPlus;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief The protocol version number for the 'stats' request. */
#define STATS_REQUEST_VERSION_NUMBER "1.0"

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and destructors
#endif // defined(__APPLE__)

StatsRequestHandler::StatsRequestHandler(RequestCounterService & service) :
        inherited(YPP_STATS_REQUEST), _service(service)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT_P(this);//####
} // StatsRequestHandler::StatsRequestHandler

StatsRequestHandler::~StatsRequestHandler(void)
{
    OD_SYSLOG_OBJENTER();//####
    OD_SYSLOG_OBJEXIT();//####
} // StatsRequestHandler::~StatsRequestHandler

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

void StatsRequestHandler::fillInDescription(yarp::os::Property & info)
{
    OD_SYSLOG_OBJENTER();//####
    try
    {
        info.put(YPP_REQREP_DICT_REQUEST_KEY, YPP_STATS_REQUEST);
        info.put(YPP_REQREP_DICT_OUTPUT_KEY, YPP_REQREP_INT YPP_REQREP_DOUBLE);
        info.put(YPP_REQREP_DICT_VERSION_KEY, STATS_REQUEST_VERSION_NUMBER);
        info.put(YPP_REQREP_DICT_DETAILS_KEY, "Return the number of requests and the time since last reset");
        yarp::os::Value    keywords;
        yarp::os::Bottle * asList = keywords.asList();
        
        asList->addString(YPP_STATS_REQUEST);
        info.put(YPP_REQREP_DICT_KEYWORDS_KEY, keywords);
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_OBJEXIT();//####
} // StatsRequestHandler::fillInDescription

bool StatsRequestHandler::operator() (const yarp::os::Bottle &      restOfInput,
                                      const yarp::os::ConstString & senderPort,
                                      yarp::os::ConnectionWriter *  replyMechanism)
{
#if (! defined(ENABLE_OD_SYSLOG))
# pragma unused(restOfInput,senderPort)
#endif // ! defined(ENABLE_OD_SYSLOG)
    OD_SYSLOG_OBJENTER();//####
    OD_SYSLOG_S2("restOfInput = ", restOfInput.toString().c_str(), "senderPort = ", senderPort.c_str());//####
    OD_SYSLOG_P1("replyMechanism = ", replyMechanism);//####
    bool result = true;
    
    try
    {
        if (replyMechanism)
        {
            yarp::os::Bottle response;
            double           elapsedTime;
            long             counter;
            
            _service.getStatistics(counter, elapsedTime);
            response.addInt(static_cast<int>(counter));
            response.addDouble(elapsedTime);
            response.write(*replyMechanism);
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_OBJEXIT_B(result);//####
    return result;
} // StatsRequestHandler::operator()

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
