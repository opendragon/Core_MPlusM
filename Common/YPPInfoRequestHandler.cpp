//--------------------------------------------------------------------------------------
//
//  File:       YPPInfoRequestHandler.cpp
//
//  Project:    YarpPlusPlus
//
//  Contains:   The class definition for the request handler for the standard 'info'
//              request.
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
//  Created:    2014-02-27
//
//--------------------------------------------------------------------------------------

#include "YPPInfoRequestHandler.h"
//#define OD_ENABLE_LOGGING /* */
#include "ODLogging.h"
#include "YPPBaseService.h"
#include "YPPRequests.h"

using namespace YarpPlusPlus;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief The protocol version number for the 'info' request. */
#define INFO_REQUEST_VERSION_NUMBER "1.0"

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and destructors
#endif // defined(__APPLE__)

InfoRequestHandler::InfoRequestHandler(void) :
        inherited(YPP_INFO_REQUEST)
{
    OD_LOG_ENTER();//####
    OD_LOG_EXIT_P(this);//####
} // InfoRequestHandler::InfoRequestHandler

InfoRequestHandler::~InfoRequestHandler(void)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_OBJEXIT();//####
} // InfoRequestHandler::~InfoRequestHandler

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

void InfoRequestHandler::fillInDescription(yarp::os::Property & info)
{
    OD_LOG_OBJENTER();//####
    try
    {
        info.put(YPP_REQREP_DICT_REQUEST_KEY, YPP_INFO_REQUEST);
        info.put(YPP_REQREP_DICT_INPUT_KEY, YPP_REQREP_ANYTHING YPP_REQREP_1_OR_MORE);
        info.put(YPP_REQREP_DICT_OUTPUT_KEY, YPP_REQREP_LIST_START YPP_REQREP_DICT_START YPP_REQREP_DICT_END
                 YPP_REQREP_0_OR_1 YPP_REQREP_LIST_END);
        info.put(YPP_REQREP_DICT_VERSION_KEY, INFO_REQUEST_VERSION_NUMBER);
        info.put(YPP_REQREP_DICT_DETAILS_KEY, "Return information on a request");
        yarp::os::Value    keywords;
        yarp::os::Bottle * asList = keywords.asList();
        
        asList->addString(YPP_INFO_REQUEST);
        info.put(YPP_REQREP_DICT_KEYWORDS_KEY, keywords);
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT();//####
} // InfoRequestHandler::fillInDescription

bool InfoRequestHandler::operator() (const yarp::os::Bottle &      restOfInput,
                                     const yarp::os::ConstString & senderPort,
                                     yarp::os::ConnectionWriter *  replyMechanism)
{
#if (! defined(OD_ENABLE_LOGGING))
# pragma unused(senderPort)
#endif // ! defined(OD_ENABLE_LOGGING)
    OD_LOG_OBJENTER();//####
    OD_LOG_S2("restOfInput = ", restOfInput.toString().c_str(), "senderPort = ", senderPort.c_str());//####
    OD_LOG_P1("replyMechanism = ", replyMechanism);//####
    bool result = true;
    
    try
    {
        if (replyMechanism)
        {
            yarp::os::Bottle reply;
            
            if (_owner && (1 == restOfInput.size()))
            {
                _owner->fillInRequestInfo(reply, restOfInput.get(0).toString());
            }
            else
            {
                OD_LOG("! (_owner && (1 == restOfInput.size()))");//####
            }
            OD_LOG_S1("reply <- ", reply.toString().c_str());
            reply.write(*replyMechanism);
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT_B(result);//####
    return result;
} // InfoRequestHandler::operator()

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
