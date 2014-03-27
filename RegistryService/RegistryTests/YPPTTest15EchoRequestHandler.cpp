//--------------------------------------------------------------------------------------
//
//  File:       YPPTTest15EchoRequestHandler.cpp
//
//  Project:    YarpPlusPlus
//
//  Contains:   The class definition for a simple request handler used by the unit tests.
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
//  Created:    2014-03-05
//
//--------------------------------------------------------------------------------------

#include "YPPTTest15EchoRequestHandler.h"
//#define OD_ENABLE_LOGGING /* */
#include "ODLogging.h"
#include "YPPRequests.h"

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The class definition for a simple request handler used by the unit tests. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace YarpPlusPlusTest;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief The protocol version number for the 'echo' request. */
#define ECHO_REQUEST_VERSION_NUMBER "1.0"

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and destructors
#endif // defined(__APPLE__)

Test15EchoRequestHandler::Test15EchoRequestHandler(void) :
        inherited(YPP_ECHO_REQUEST)
{
    OD_LOG_ENTER();//####
    OD_LOG_EXIT_P(this);//####
} // Test15EchoRequestHandler::Test15EchoRequestHandler

Test15EchoRequestHandler::~Test15EchoRequestHandler(void)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_OBJEXIT();//####
} // Test15EchoRequestHandler::~Test15EchoRequestHandler

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

void Test15EchoRequestHandler::fillInAliases(YarpPlusPlus::StringVector & alternateNames)
{
#if (! defined(OD_ENABLE_LOGGING))
# pragma unused(alternateNames)
#endif // ! defined(OD_ENABLE_LOGGING)
    OD_LOG_OBJENTER();//####
    OD_LOG_P1("alternateNames = ", &alternateNames);//####
    OD_LOG_OBJEXIT();//####
} // Test15EchoRequestHandler::fillInAliases

void Test15EchoRequestHandler::fillInDescription(const yarp::os::ConstString & request,
                                                 yarp::os::Property &          info)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_S1("request = ", request.toString().c_str());//####
    OD_LOG_P1("info = ", &info);//####
    try
    {
        info.put(YPP_REQREP_DICT_REQUEST_KEY, request);
        info.put(YPP_REQREP_DICT_INPUT_KEY, YPP_REQREP_ANYTHING YPP_REQREP_0_OR_MORE);
        info.put(YPP_REQREP_DICT_OUTPUT_KEY, YPP_REQREP_ANYTHING YPP_REQREP_0_OR_MORE);
        info.put(YPP_REQREP_DICT_VERSION_KEY, ECHO_REQUEST_VERSION_NUMBER);
        info.put(YPP_REQREP_DICT_DETAILS_KEY, "Echo back any input");
        yarp::os::Value    keywords;
        yarp::os::Bottle * asList = keywords.asList();
        
        asList->addString(request);
        info.put(YPP_REQREP_DICT_KEYWORDS_KEY, keywords);
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT();//####
} // Test15EchoRequestHandler::fillInDescription

bool Test15EchoRequestHandler::processRequest(const yarp::os::ConstString & request,
                                              const yarp::os::Bottle &      restOfInput,
                                              const yarp::os::ConstString & senderPort,
                                              yarp::os::ConnectionWriter *  replyMechanism)
{
#if (! defined(OD_ENABLE_LOGGING))
# pragma unused(request,senderPort)
#endif // ! defined(OD_ENABLE_LOGGING)
    OD_LOG_OBJENTER();//####
    OD_LOG_S3("request = ", request.toString().c_str(), "restOfInput = ", restOfInput.toString().c_str(),//####
              "senderPort = ", senderPort.c_str());//####
    OD_LOG_P1("replyMechanism = ", replyMechanism);//####
    bool result = true;
    
    try
    {
        if (replyMechanism)
        {
            yarp::os::Bottle argsCopy(restOfInput);
            
            argsCopy.write(*replyMechanism);
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT_B(result);//####
    return result;
} // Test15EchoRequestHandler::processRequest

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
