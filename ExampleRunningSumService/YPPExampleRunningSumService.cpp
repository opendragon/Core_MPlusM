//--------------------------------------------------------------------------------------
//
//  File:       YPPExampleRunningSumService.cpp
//
//  Project:    YarpPlusPlus
//
//  Contains:   The class definition for a simple Yarp++ service with context.
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
//  Created:    2014-03-18
//
//--------------------------------------------------------------------------------------

#include "YPPExampleRunningSumService.h"
#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPAddRequestHandler.h"
#include "YPPExampleRunningSumRequests.h"
#include "YPPResetRequestHandler.h"
#include "YPPRunningSumContext.h"
#include "YPPStartRequestHandler.h"
#include "YPPStopRequestHandler.h"

using namespace YarpPlusPlusExample;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

ExampleRunningSumService::ExampleRunningSumService(const yarp::os::ConstString & serviceEndpointName,
                                                   const yarp::os::ConstString & serviceHostName,
                                                   const yarp::os::ConstString & servicePortNumber) :
        inherited(true, YPP_RUNNINGSUM_CANONICAL_NAME, "An example running sum service", serviceEndpointName,
                  serviceHostName, servicePortNumber)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S3("serviceEndpointName = ", serviceEndpointName.c_str(), "serviceHostName = ",//####
                 serviceHostName.c_str(), "servicePortNumber = ", servicePortNumber.c_str());//####
    setUpRequestHandlers();
    OD_SYSLOG_EXIT_P(this);//####
} // ExampleRunningSumService::ExampleRunningSumService

ExampleRunningSumService::~ExampleRunningSumService(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // ExampleRunningSumService::~ExampleRunningSumService

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

#if 0
void addContext(const yarp::os::ConstString & key,
                BaseContext *                 context);

/*! @brief Remove all contexts. */
void clearContexts(void);

/*! @brief Locate the context corresponding to a name.
 @param key The name of the context.
 @returns @c NULL if the named context could not be found or a pointer to the context if found. */
BaseContext * findContext(const yarp::os::ConstString & key);

/*! @brief Remove a context.
 @param key The name of the context. */
void removeContext(const yarp::os::ConstString & key);
#endif//0

double ExampleRunningSumService::addToSum(const yarp::os::ConstString & key,
                                          const double                  value)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("key = ", key.c_str());//####
    OD_SYSLOG_D1("value = ", value);//####
    RunningSumContext * context = (RunningSumContext *) findContext(key);
    
    if (! context)
    {
        context = new RunningSumContext;
        addContext(key, context);
    }
    context->sum() += value;
    OD_SYSLOG_EXIT_D(context->sum());//####
    return context->sum();
} // ExampleRunningSumService::addToSum

void ExampleRunningSumService::resetSum(const yarp::os::ConstString & key)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("key = ", key.c_str());//####
    RunningSumContext * context = (RunningSumContext *) findContext(key);
    
    if (! context)
    {
        context = new RunningSumContext;
        addContext(key, context);
    }
    context->sum() = 0;
    OD_SYSLOG_EXIT();//####
} // ExampleRunningSumService::resetSum

void ExampleRunningSumService::setUpRequestHandlers(void)
{
    OD_SYSLOG_ENTER();//####
    _requestHandlers.registerRequestHandler(new AddRequestHandler(*this));
    _requestHandlers.registerRequestHandler(new ResetRequestHandler(*this));
    _requestHandlers.registerRequestHandler(new StartRequestHandler(*this));
    _requestHandlers.registerRequestHandler(new StopRequestHandler(*this));
    OD_SYSLOG_EXIT();//####
} // ExampleRunningSumService::setUpRequestHandlers

bool ExampleRunningSumService::start(void)
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
} // ExampleRunningSumService::start

void ExampleRunningSumService::startSum(const yarp::os::ConstString & key)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("key = ", key.c_str());//####
    RunningSumContext * context = (RunningSumContext *) findContext(key);
    
    if (! context)
    {
        context = new RunningSumContext;
        addContext(key, context);
    }
    context->sum() = 0;
    OD_SYSLOG_EXIT();//####
} // ExampleRunningSumService::startSum

bool ExampleRunningSumService::stop(void)
{
    OD_SYSLOG_ENTER();//####
    bool result = BaseService::stop();
    
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // ExampleRunningSumService::stop

void ExampleRunningSumService::stopSum(const yarp::os::ConstString & key)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("key = ", key.c_str());//####
    removeContext(key);
    OD_SYSLOG_EXIT();//####
} // ExampleRunningSumService::stopSum
