//--------------------------------------------------------------------------------------------------
//
//  File:       m+mJavaScriptThread.cpp
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

#include "m+mJavaScriptThread.h"

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

JavaScriptThread::JavaScriptThread(const double            timeToWait,
                                   JSContext *             context,
                                   JS::RootedObject &      global,
                                   const JS::RootedValue & threadFunc) :
    inherited(), _timeToWait(timeToWait), _threadFunc(context), _global(global), _context(context)
{
    OD_LOG_ENTER(); //####
    OD_LOG_D1("timeToWait = ", timeToWait); //####
    OD_LOG_P3("context = ", context, "global = ", &global, "threadFunc = ", &threadFunc); //####
    _threadFunc = threadFunc;
    OD_LOG_EXIT_P(this); //####
} // JavaScriptThread::JavaScriptThread

JavaScriptThread::~JavaScriptThread(void)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_OBJEXIT(); //####
} // JavaScriptThread::~JavaScriptThread

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void JavaScriptThread::clearOutputChannel(void)
{
    OD_LOG_OBJENTER(); //####
//    _outChannel = nullptr;
    OD_LOG_OBJEXIT(); //####
} // JavaScriptThread::clearOutputChannel

void JavaScriptThread::run(void)
{
    OD_LOG_OBJENTER(); //####
    for ( ; ! isStopping(); )
    {
        if (_nextTime <= yarp::os::Time::now())
        {
            OD_LOG("(_nextTime <= yarp::os::Time::now())"); //####
            if (_context && (! _threadFunc.isNullOrUndefined()))
            {
                JS::AutoValueVector funcArgs(_context);
                JS::RootedValue     funcResult(_context);
                
                JS_BeginRequest(_context);
                if (JS_CallFunctionValue(_context, _global, _threadFunc, funcArgs, &funcResult))
                {
                    // We don't care about the function result, as it's supposed to just perform an
                    // iteration of the thread.
                }
                else
                {
                    OD_LOG("! (JS_CallFunctionValue(_context, _global, _threadFunc, " //####
                           "funcArgs, &funcResult))"); //####
                    JS::RootedValue exc(_context);
                    
                    if (JS_GetPendingException(_context, &exc))
                    {
                        JS_ClearPendingException(_context);
#if MAC_OR_LINUX_
                        GetLogger().fail("Exception occurred while executing scriptThread "
                                         "function.");
#else // ! MAC_OR_LINUX_
                        cerr << "Exception occurred while executing scriptThread function." << endl;
#endif // ! MAC_OR_LINUX_
                    }
                }
                JS_EndRequest(_context);
            }
            _nextTime = yarp::os::Time::now() + _timeToWait;
        }
        yarp::os::Time::yield();
    }
    OD_LOG_OBJEXIT(); //####
} // JavaScriptThread::run

bool JavaScriptThread::threadInit(void)
{
    OD_LOG_OBJENTER(); //####
    bool result = true;
    
    _nextTime = yarp::os::Time::now() + _timeToWait;
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // JavaScriptThread::threadInit

void JavaScriptThread::threadRelease(void)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_OBJEXIT(); //####
} // JavaScriptThread::threadRelease

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
