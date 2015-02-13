//--------------------------------------------------------------------------------------
//
//  File:       M+MRGBLEDService.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for a simple M+M service.
//
//  Written by: Johnty Wang
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
//                  documentation and / or other materials provided with the
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
//  Created:    2014-02-06
//
//--------------------------------------------------------------------------------------

#include "M+MRGBLEDService.h"
#include "M+MRGBLEDRequestHandler.h"
#include "M+MRGBLEDRequests.h"

//#include "ODEnableLogging.h"
#include "ODLogging.h"

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for a simple M+M service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Example;

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

RGBLEDService::RGBLEDService(const yarp::os::ConstString & launchPath,
                             const yarp::os::ConstString & tag,
                             const yarp::os::ConstString & serviceEndpointName,
                             const yarp::os::ConstString & servicePortNumber) :
        inherited(kServiceKindNormal, launchPath, tag, true, MpM_ECHO_CANONICAL_NAME,
                  "An example RGB LED service",
                  "echo - send back any values given with the request", serviceEndpointName,
                  servicePortNumber), _echoHandler(NULL)
{
    OD_LOG_ENTER();//####
    OD_LOG_S4s("launchPath = ", launchPath, "tag = ", tag, "serviceEndpointName = ", //####
               serviceEndpointName, "servicePortNumber = ", servicePortNumber);//####
    attachRequestHandlers();
    OD_LOG_EXIT_P(this);//####
} // RGBLEDService::RGBLEDService

RGBLEDService::~RGBLEDService(void)
{
    OD_LOG_OBJENTER();//####
    detachRequestHandlers();
    OD_LOG_OBJEXIT();//####
} // RGBLEDService::~RGBLEDService

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

void RGBLEDService::attachRequestHandlers(void)
{
    OD_LOG_OBJENTER();//####
    try
    {
        _echoHandler = new RGBLEDRequestHandler(*this);
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
} // EchoService::attachRequestHandlers

void RGBLEDService::detachRequestHandlers(void)
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
} // RGBLEDService::detachRequestHandlers

bool RGBLEDService::start(void)
{
    OD_LOG_OBJENTER();//####
    try
    {
        if (! isStarted())
        {
            inherited::start();
            if (isStarted())
            {
                
            }
            else
            {
                OD_LOG("! (isStarted())");//####
            }
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT_B(isStarted());//####
    return isStarted();
} // RGBLEDService::start

bool RGBLEDService::stop(void)
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
} // RGBLEDService::stop
