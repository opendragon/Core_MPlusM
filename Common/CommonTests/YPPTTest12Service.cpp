//--------------------------------------------------------------------------------------
//
//  File:       YPPTTest12Service.cpp
//
//  Project:    YarpPlusPlus
//
//  Contains:   The class definition for a simple service used by the unit tests.
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
//  Created:    2014-02-28
//
//--------------------------------------------------------------------------------------

#include "YPPTTest12Service.h"
//#include "ODEnableLogging.h"
#include "ODLogging.h"
#include "YPPRequests.h"
#include "YPPTTest12EchoRequestHandler.h"

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The class definition for a simple service used by the unit tests. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace YarpPlusPlusTest;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief The operation timeout to use with YARP. */
static const float kTest12ServiceTimeout = 5.0;

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and destructors
#endif // defined(__APPLE__)

Test12Service::Test12Service(const int argc,
                             char **   argv) :
        inherited(true, "Test12", "Simple service for unit tests", argc, argv), _echoHandler(NULL)
{
    OD_LOG_ENTER();//####
    attachRequestHandlers();
    OD_LOG_EXIT_P(this);//####
} // Test12Service::Test12Service

Test12Service::~Test12Service(void)
{
    OD_LOG_OBJENTER();//####
    detachRequestHandlers();
    OD_LOG_OBJEXIT();//####
} // Test12Service::~Test12Service

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

void Test12Service::attachRequestHandlers(void)
{
    OD_LOG_OBJENTER();//####
    try
    {
        _echoHandler = new Test12EchoRequestHandler;
        if (_echoHandler)
        {
            registerRequestHandler(_echoHandler);
        }
        else
        {
            OD_LOG("! (_echoHandler)");//####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT();//####
} // Test12Service::attachRequestHandlers

void Test12Service::detachRequestHandlers(void)
{
    OD_LOG_OBJENTER();//####
    try
    {
        if (_echoHandler)
        {
            unregisterRequestHandler(_echoHandler);
            delete _echoHandler;
            _echoHandler = NULL;
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT();//####
} // Test12Service::detachRequestHandlers

bool Test12Service::start(void)
{
    OD_LOG_OBJENTER();//####
    bool result = false;
    
    try
    {
        if (! isStarted())
        {
            setTimeout(kTest12ServiceTimeout);
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
} // Test12Service::start

bool Test12Service::stop(void)
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
} // Test12Service::stop

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
