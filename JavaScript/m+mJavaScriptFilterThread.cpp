//--------------------------------------------------------------------------------------------------
//
//  File:       m+mJavaScriptFilterThread.cpp
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
//  Created:    2015-01-15
//
//--------------------------------------------------------------------------------------------------

#include "m+mJavaScriptFilterThread.hpp"
#include "m+mJavaScriptFilterService.hpp"

//#include <ODEnableLogging.h>
#include <ODLogging.h>

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
using namespace MplusM::JavaScript;
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

JavaScriptFilterThread::JavaScriptFilterThread(JavaScriptFilterService & owner,
                                               const double              timeToWait) :
    inherited(), _timeToWait(timeToWait), _owner(owner)
{
    ODL_ENTER(); //####
    ODL_P1("owner = ", &owner); //####
    ODL_D1("timeToWait = ", timeToWait); //####
    ODL_EXIT_P(this); //####
} // JavaScriptFilterThread::JavaScriptFilterThread

JavaScriptFilterThread::~JavaScriptFilterThread(void)
{
    ODL_OBJENTER(); //####
    ODL_OBJEXIT(); //####
} // JavaScriptFilterThread::~JavaScriptFilterThread

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void
JavaScriptFilterThread::clearOutputChannel(void)
{
    ODL_OBJENTER(); //####
//    _outChannel = NULL;
    ODL_OBJEXIT(); //####
} // JavaScriptFilterThread::clearOutputChannel

void
JavaScriptFilterThread::run(void)
{
    ODL_OBJENTER(); //####
    try
    {
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
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
    }
    ODL_OBJEXIT(); //####
} // JavaScriptFilterThread::run

bool
JavaScriptFilterThread::threadInit(void)
{
    ODL_OBJENTER(); //####
    bool result = true;

    _nextTime = yarp::os::Time::now() + _timeToWait;
    ODL_OBJEXIT_B(result); //####
    return result;
} // JavaScriptFilterThread::threadInit

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
