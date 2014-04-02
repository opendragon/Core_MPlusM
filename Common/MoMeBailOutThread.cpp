//--------------------------------------------------------------------------------------
//
//  File:       MoMeBailOutThread.cpp
//
//  Project:    MoAndMe
//
//  Contains:   The class definition for a timeout thread for MoAndMe.
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
//  Created:    2014-04-01
//
//--------------------------------------------------------------------------------------

#include "MoMeBailOutThread.h"

//#include "ODEnableLogging.h"
#include "ODLogging.h"

# if (defined(__APPLE__) || defined(__linux__))
#  include <csignal>
# endif // defined(__APPLE__) || defined(__linux__)
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
 
 @brief The class definition for a timeout thread for MoAndMe. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MoAndMe::Common;

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

BailOutThread::BailOutThread(Channel *    channelOfInterest,
                             const double timeToWait) :
        inherited(), _channel(channelOfInterest), _timeToWait(timeToWait)
{
    OD_LOG_ENTER();//####
    OD_LOG_P1("channelOfInterest = ", channelOfInterest);//####
    OD_LOG_D1("timeToWait = ", timeToWait);//####
    OD_LOG_EXIT_P(this);//####
} // BailOutThread::BailOutThread

BailOutThread::~BailOutThread(void)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_OBJEXIT();//####
} // BailOutThread::~BailOutThread

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

void BailOutThread::run(void)
{
    OD_LOG_OBJENTER();//####
    for ( ; ! isStopping(); )
    {
        if (_endTime <= yarp::os::Time::now())
        {
            OD_LOG("(_endTime <= yarp::os::Time::now())");//####
            if (_channel)
            {
                _channel->interrupt();
            }
#if (defined(__APPLE__) || defined(__linux__))
            raise(STANDARD_SIGNAL_TO_USE);
#endif // defined(__APPLE__) || defined(__linux__)
#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunreachable-code"
#endif // defined(__APPLE__)
            break;
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)
        }
        yarp::os::Time::yield();
    }
    OD_LOG_OBJEXIT();//####
} // BailOutThread::run

bool BailOutThread::threadInit(void)
{
    OD_LOG_OBJENTER();//####
    bool result = true;
    
    _endTime = yarp::os::Time::now() + _timeToWait;
    OD_LOG_OBJEXIT_B(result);//####
    return result;
} // BailOutThread::threadInit

void BailOutThread::threadRelease(void)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_OBJEXIT();//####
} // BailOutThread::threadRelease

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
