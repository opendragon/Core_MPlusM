//--------------------------------------------------------------------------------------------------
//
//  File:       M+MExemplarInputThread.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for an output-generating thread for M+M.
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
//  Created:    2014-09-15
//
//--------------------------------------------------------------------------------------------------

#include "M+MExemplarInputThread.h"

#include <mpm/M+MGeneralChannel.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The class definition for an output-generating thread for M+M. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Exemplars;

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

ExemplarInputThread::ExemplarInputThread(Common::GeneralChannel * outChannel,
                                         const double             timeToWait,
                                         const int                numValues) :
    inherited(), _outChannel(outChannel), _timeToWait(timeToWait), _numValues(numValues)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P1("outChannel = ", outChannel); //####
    OD_LOG_D1("timeToWait = ", timeToWait); //####
    OD_LOG_LL1("numValues = ", numValues); //####
    OD_LOG_EXIT_P(this); //####
} // ExemplarInputThread::ExemplarInputThread

ExemplarInputThread::~ExemplarInputThread(void)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_OBJEXIT(); //####
} // ExemplarInputThread::~ExemplarInputThread

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void ExemplarInputThread::run(void)
{
    OD_LOG_OBJENTER(); //####
    for ( ; ! isStopping(); )
    {
        if (_nextTime <= yarp::os::Time::now())
        {
            OD_LOG("(_nextTime <= yarp::os::Time::now())"); //####
            if (_outChannel)
            {
                yarp::os::Bottle message;
                
                for (int ii = 0; ii < _numValues; ++ii)
                {
                    message.addDouble(10000 * yarp::os::Random::uniform());
                }
                if (! _outChannel->write(message))
                {
                    OD_LOG("(! _outChannel->write(message))"); //####
#if defined(MpM_StallOnSendProblem)
                    Common::Stall();
#endif // defined(MpM_StallOnSendProblem)
                }
            }
            _nextTime = yarp::os::Time::now() + _timeToWait;
        }
        yarp::os::Time::yield();
    }
    OD_LOG_OBJEXIT(); //####
} // ExemplarInputThread::run

bool ExemplarInputThread::threadInit(void)
{
    OD_LOG_OBJENTER(); //####
    bool result = true;
    
    _nextTime = yarp::os::Time::now() + _timeToWait;
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // ExemplarInputThread::threadInit

void ExemplarInputThread::threadRelease(void)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_OBJEXIT(); //####
} // ExemplarInputThread::threadRelease

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
