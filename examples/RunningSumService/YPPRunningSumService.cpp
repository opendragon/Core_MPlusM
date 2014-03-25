//--------------------------------------------------------------------------------------
//
//  File:       YPPRunningSumService.cpp
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

#include "YPPRunningSumService.h"
//#define OD_ENABLE_LOGGING /* */
#include "ODLogging.h"
#include "YPPAddRequestHandler.h"
#include "YPPResetRequestHandler.h"
#include "YPPRunningSumContext.h"
#include "YPPRunningSumRequests.h"
#include "YPPStartRequestHandler.h"
#include "YPPStopRequestHandler.h"

using namespace YarpPlusPlusExample;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief The operation timeout to use with YARP. */
static const float kRunningSumServiceTimeout = 5.0;

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and destructors
#endif // defined(__APPLE__)

RunningSumService::RunningSumService(const yarp::os::ConstString & serviceEndpointName,
                                     const yarp::os::ConstString & serviceHostName,
                                     const yarp::os::ConstString & servicePortNumber) :
        inherited(true, YPP_RUNNINGSUM_CANONICAL_NAME, "An example running sum service", serviceEndpointName,
                  serviceHostName, servicePortNumber)
#if (! defined(SERVICES_HAVE_CONTEXTS))
        , _runningSum(0.0)
#endif // ! defined(SERVICES_HAVE_CONTEXTS)
{
    OD_LOG_ENTER();//####
    OD_LOG_S3("serviceEndpointName = ", serviceEndpointName.c_str(), "serviceHostName = ",//####
              serviceHostName.c_str(), "servicePortNumber = ", servicePortNumber.c_str());//####
    setUpRequestHandlers();
    OD_LOG_EXIT_P(this);//####
} // RunningSumService::RunningSumService

RunningSumService::~RunningSumService(void)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_OBJEXIT();//####
} // RunningSumService::~RunningSumService

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

double RunningSumService::addToSum(const yarp::os::ConstString & key,
                                   const double                  value)
{
#if (! defined(SERVICES_HAVE_CONTEXTS))
# pragma unused(key)
#endif // ! defined(SERVICES_HAVE_CONTEXTS)
    OD_LOG_OBJENTER();//####
    OD_LOG_S1("key = ", key.c_str());//####
    OD_LOG_D1("value = ", value);//####
    double result = 0.0;
    
    try
    {
#if defined(SERVICES_HAVE_CONTEXTS)
        RunningSumContext * context = (RunningSumContext *) findContext(key);
#endif // ! defined(SERVICES_HAVE_CONTEXTS)
        
#if defined(SERVICES_HAVE_CONTEXTS)
        if (! context)
        {
            context = new RunningSumContext;
            addContext(key, context);
        }
        context->sum() += value;
        result = context->sum();
#else // ! defined(SERVICES_HAVE_CONTEXTS)
        _runningSum += value;
        result = _runningSum;
#endif // ! defined(SERVICES_HAVE_CONTEXTS)
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT_D(result);//####
    return result;
} // RunningSumService::addToSum

void RunningSumService::resetSum(const yarp::os::ConstString & key)
{
#if (! defined(SERVICES_HAVE_CONTEXTS))
# pragma unused(key)
#endif // ! defined(SERVICES_HAVE_CONTEXTS)
    OD_LOG_OBJENTER();//####
    OD_LOG_S1("key = ", key.c_str());//####
    try
    {
#if defined(SERVICES_HAVE_CONTEXTS)
        RunningSumContext * context = (RunningSumContext *) findContext(key);
#endif // ! defined(SERVICES_HAVE_CONTEXTS)

#if defined(SERVICES_HAVE_CONTEXTS)
        if (! context)
        {
            context = new RunningSumContext;
            addContext(key, context);
        }
        context->sum() = 0.0;
#else // ! defined(SERVICES_HAVE_CONTEXTS)
        _runningSum = 0.0;
#endif // ! defined(SERVICES_HAVE_CONTEXTS)
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT();//####
} // RunningSumService::resetSum

void RunningSumService::setUpRequestHandlers(void)
{
    OD_LOG_OBJENTER();//####
    try
    {
        _requestHandlers.registerRequestHandler(new AddRequestHandler(*this));
        _requestHandlers.registerRequestHandler(new ResetRequestHandler(*this));
        _requestHandlers.registerRequestHandler(new StartRequestHandler(*this));
        _requestHandlers.registerRequestHandler(new StopRequestHandler(*this));
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT();//####
} // RunningSumService::setUpRequestHandlers

bool RunningSumService::start(void)
{
    OD_LOG_OBJENTER();//####
    bool result = false;
    
    try
    {
        if (! isStarted())
        {
            setTimeout(kRunningSumServiceTimeout);
            inherited::start();
            if (isStarted())
            {
                
            }
            else
            {
                OD_LOG("! (isStarted())");//####
            }
        }
        result = isStarted();
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT_B(result);//####
    return result;
} // RunningSumService::start

void RunningSumService::startSum(const yarp::os::ConstString & key)
{
#if (! defined(SERVICES_HAVE_CONTEXTS))
# pragma unused(key)
#endif // ! defined(SERVICES_HAVE_CONTEXTS)
    OD_LOG_OBJENTER();//####
    OD_LOG_S1("key = ", key.c_str());//####
    try
    {
#if defined(SERVICES_HAVE_CONTEXTS)
        RunningSumContext * context = (RunningSumContext *) findContext(key);
#endif // ! defined(SERVICES_HAVE_CONTEXTS)
        
#if defined(SERVICES_HAVE_CONTEXTS)
        if (! context)
        {
            context = new RunningSumContext;
            addContext(key, context);
        }
        context->sum() = 0.0;
#else // ! defined(SERVICES_HAVE_CONTEXTS)
        _runningSum = 0.0;
#endif // ! defined(SERVICES_HAVE_CONTEXTS)
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT();//####
} // RunningSumService::startSum

bool RunningSumService::stop(void)
{
    OD_LOG_OBJENTER();//####
    bool result = false;
    
    try
    {
        result = inherited::stop();
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT_B(result);//####
    return result;
} // RunningSumService::stop

void RunningSumService::stopSum(const yarp::os::ConstString & key)
{
#if (! defined(SERVICES_HAVE_CONTEXTS))
# pragma unused(key)
#endif // ! defined(SERVICES_HAVE_CONTEXTS)
    OD_LOG_OBJENTER();//####
    OD_LOG_S1("key = ", key.c_str());//####
    try
    {
#if defined(SERVICES_HAVE_CONTEXTS)
        removeContext(key);
#endif // defined(SERVICES_HAVE_CONTEXTS)
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT();//####
} // RunningSumService::stopSum
