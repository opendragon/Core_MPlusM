//--------------------------------------------------------------------------------------------------
//
//  File:       m+mCommonLispFilterThread.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for an output-generating thread for m+m.
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
//  Created:    2015-08-05
//
//--------------------------------------------------------------------------------------------------

#include "m+mCommonLispFilterThread.h"

#include "m+mCommonLispFilterService.h"

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for an output-generating thread for m+m. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::CommonLisp;
using std::cerr;
using std::endl;

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

CommonLispFilterThread::CommonLispFilterThread(CommonLispFilterService & owner,
                                               const double              timeToWait) :
    inherited(), _timeToWait(timeToWait), _owner(owner)
{
    ODL_ENTER(); //####
    ODL_P2("owner = ", &owner, "threadFunc = ", threadFunc); //####
    ODL_D1("timeToWait = ", timeToWait); //####
    ODL_EXIT_P(this); //####
} // CommonLispFilterThread::CommonLispFilterThread

CommonLispFilterThread::~CommonLispFilterThread(void)
{
    ODL_OBJENTER(); //####
    ODL_OBJEXIT(); //####
} // CommonLispFilterThread::~CommonLispFilterThread

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void
CommonLispFilterThread::clearOutputChannel(void)
{
    ODL_OBJENTER(); //####
    ODL_OBJEXIT(); //####
} // CommonLispFilterThread::clearOutputChannel

DEFINE_RUN_(CommonLispFilterThread)
{
    ODL_OBJENTER(); //####
    for ( ; ! isStopping(); )
    {
        if (_nextTime <= yarp::os::Time::now())
        {
            ODL_LOG("(_nextTime <= yarp::os::Time::now())"); //####
            _owner.signalRunFunction();
            _nextTime = yarp::os::Time::now() + _timeToWait;
        }
        ConsumeSomeTime();
    }
    ODL_OBJEXIT(); //####
} // CommonLispFilterThread::run

DEFINE_THREADINIT_(CommonLispFilterThread)
{
    ODL_OBJENTER(); //####
    bool result = true;
    
    _nextTime = yarp::os::Time::now() + _timeToWait;
    ODL_OBJEXIT_B(result); //####
    return result;
} // CommonLispFilterThread::threadInit

DEFINE_THREADRELEASE_(CommonLispFilterThread)
{
    ODL_OBJENTER(); //####
    ODL_OBJEXIT(); //####
} // CommonLispFilterThread::threadRelease

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
