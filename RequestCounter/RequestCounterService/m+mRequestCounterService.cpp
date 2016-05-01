//--------------------------------------------------------------------------------------------------
//
//  File:       m+mRequestCounterService.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for a service that collects statistic on requests.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2014 by H Plus Technologies Ltd. and Simon Fraser University.
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
//  Created:    2014-03-14
//
//--------------------------------------------------------------------------------------------------

#include "m+mRequestCounterService.h"
#include "m+mRequestCounterContext.h"
#include "m+mRequestCounterDefaultRequestHandler.h"
#include "m+mRequestCounterRequests.h"
#include "m+mResetCounterRequestHandler.h"
#include "m+mStatsRequestHandler.h"

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for a service that collects statistic on requests. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::RequestCounter;

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

RequestCounterService::RequestCounterService(const YarpString & launchPath,
                                             const int          argc,
                                             char * *           argv,
                                             const YarpString & serviceEndpointName,
                                             const YarpString & servicePortNumber) :
    inherited(kServiceKindNormal, launchPath, argc, argv, "", true,
              MpM_REQUESTCOUNTER_CANONICAL_NAME_, REQUESTCOUNTER_SERVICE_DESCRIPTION_,
              "reset - clear the Request Counter and the elapsed time\n"
              "stats - report the Request Counter and the elapsed time\n"
              "<anything else> - simply increment the Request Counter", serviceEndpointName,
              servicePortNumber), _defaultHandler(NULL), _resetSumHandler(NULL),
    _statsHandler(NULL)
{
    ODL_ENTER(); //####
    ODL_S3s("launchPath = ", launchPath, "serviceEndpointName = ", serviceEndpointName, //####
            "servicePortNumber = ", servicePortNumber); //####
    ODL_LL1("argc = ", argc); //####
    ODL_P1("argv = ", argv); //####
    attachRequestHandlers();
    ODL_EXIT_P(this); //####
} // RequestCounterService::RequestCounterService

RequestCounterService::~RequestCounterService(void)
{
    ODL_OBJENTER(); //####
    detachRequestHandlers();
    ODL_OBJEXIT(); //####
} // RequestCounterService::~RequestCounterService

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void
RequestCounterService::attachRequestHandlers(void)
{
    ODL_OBJENTER(); //####
    try
    {
        _defaultHandler = new RequestCounterDefaultRequestHandler(*this);
        _resetSumHandler = new ResetCounterRequestHandler(*this);
        _statsHandler = new StatsRequestHandler(*this);
        if (_defaultHandler && _resetSumHandler && _statsHandler)
        {
            registerRequestHandler(_resetSumHandler);
            registerRequestHandler(_statsHandler);
            setDefaultRequestHandler(_defaultHandler);
        }
        else
        {
            ODL_LOG("! (_defaultHandler && _resetSumHandler && _statsHandler)"); //####
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT(); //####
} // RequestCounterService::attachRequestHandlers

void
RequestCounterService::countRequest(const YarpString & key)
{
    ODL_OBJENTER(); //####
    try
    {
        RequestCounterContext * context = (RequestCounterContext *) findContext(key);

        if (! context)
        {
            context = new RequestCounterContext;
            addContext(key, context);
        }
        context->counter() += 1;
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT(); //####
} // RequestCounterService::countRequest

void
RequestCounterService::detachRequestHandlers(void)
{
    ODL_OBJENTER(); //####
    try
    {
        if (_defaultHandler)
        {
            setDefaultRequestHandler(NULL);
            delete _defaultHandler;
            _defaultHandler = NULL;
        }
        if (_resetSumHandler)
        {
            unregisterRequestHandler(_resetSumHandler);
            delete _resetSumHandler;
            _resetSumHandler = NULL;
        }
        if (_statsHandler)
        {
            unregisterRequestHandler(_statsHandler);
            delete _statsHandler;
            _statsHandler = NULL;
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT(); //####
} // RequestCounterService::detachRequestHandlers

void
RequestCounterService::getStatistics(const YarpString & key,
                                     long &             counter,
                                     double &           elapsedTime)
{
    ODL_OBJENTER(); //####
    try
    {
        RequestCounterContext * context = (RequestCounterContext *) findContext(key);

        if (! context)
        {
            context = new RequestCounterContext;
            addContext(key, context);
        }
        counter = context->counter();
        elapsedTime = yarp::os::Time::now() - context->lastReset();
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT(); //####
} // RequestCounterService::getStatistics

void
RequestCounterService::resetCounters(const YarpString & key)
{
    ODL_OBJENTER(); //####
    try
    {
        RequestCounterContext * context = (RequestCounterContext *) findContext(key);

        if (! context)
        {
            context = new RequestCounterContext;
            addContext(key, context);
        }
        context->counter() = 0;
        context->lastReset() = yarp::os::Time::now();
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT(); //####
} // RequestCounterService::resetCounters

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
