//--------------------------------------------------------------------------------------
//
//  File:       M+MPingThread.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for a ping thread for M+M.
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
//  Created:    2014-06-18
//
//--------------------------------------------------------------------------------------

#include "M+MPingThread.h"
#include "M+MBaseService.h"

//#include "ODEnableLogging.h"
#include "ODLogging.h"

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The class definition for a ping thread for M+M. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;

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

PingThread::PingThread(const yarp::os::ConstString & channelName) :
        inherited(), _channelName(channelName)
{
    OD_LOG_ENTER();//####
    OD_LOG_S1("channelName = ", channelName.c_str());//####
    OD_LOG_EXIT_P(this);//####
} // PingThread::PingThread

PingThread::~PingThread(void)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_OBJEXIT();//####
} // PingThread::~PingThread

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

void PingThread::run(void)
{
    OD_LOG_OBJENTER();//####
    for ( ; ! isStopping(); )
    {
        double now = yarp::os::Time::now();
        
        if (_pingTime <= now)
        {
            // Send a ping!
            MplusM::Common::BaseService::SendPingForChannel(_channelName);
            _pingTime = now + PING_INTERVAL;
        }
        yarp::os::Time::delay(PING_INTERVAL / 10.0);
    }
    OD_LOG_OBJEXIT();//####
} // PingThread::run

bool PingThread::threadInit(void)
{
    OD_LOG_OBJENTER();//####
    bool result = true;
    
    _pingTime = yarp::os::Time::now() + PING_INTERVAL;
    OD_LOG_OBJEXIT_B(result);//####
    return result;
} // PingThread::threadInit

void PingThread::threadRelease(void)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_OBJEXIT();//####
} // PingThread::threadRelease

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
