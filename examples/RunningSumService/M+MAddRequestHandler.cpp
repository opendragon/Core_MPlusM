//--------------------------------------------------------------------------------------
//
//  File:       M+MAddRequestHandler.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for the request handler for an 'add' request.
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

#include "M+MAddRequestHandler.h"
#include "M+MRunningSumRequests.h"
#include "M+MRunningSumService.h"

//#include "ODEnableLogging.h"
#include "ODLogging.h"

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The class definition for the request handler for an 'add' request. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Example;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief The protocol version number for the 'reset' request. */
#define ADD_REQUEST_VERSION_NUMBER "1.1"

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and destructors
#endif // defined(__APPLE__)

AddRequestHandler::AddRequestHandler(RunningSumService & service) :
        inherited(MpM_ADD_REQUEST), _service(service)
{
    OD_LOG_ENTER();//####
    OD_LOG_P1("service = ", &service);//####
    OD_LOG_EXIT_P(this);//####
} // AddRequestHandler::AddRequestHandler

AddRequestHandler::~AddRequestHandler(void)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_OBJEXIT();//####
} // AddRequestHandler::~AddRequestHandler

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

void AddRequestHandler::fillInAliases(Common::StringVector & alternateNames)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_P1("alternateNames = ", &alternateNames);//####
    alternateNames.push_back("+");
    OD_LOG_OBJEXIT();//####
} // AddRequestHandler::fillInAliases

void AddRequestHandler::fillInDescription(const yarp::os::ConstString & request,
                                          yarp::os::Property &          info)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_S1("request = ", request.c_str());//####
    OD_LOG_P1("info = ", &info);//####
    try
    {
        info.put(MpM_REQREP_DICT_REQUEST_KEY, request);
        info.put(MpM_REQREP_DICT_INPUT_KEY, MpM_REQREP_NUMBER MpM_REQREP_1_OR_MORE);
        info.put(MpM_REQREP_DICT_OUTPUT_KEY, MpM_REQREP_DOUBLE);
        info.put(MpM_REQREP_DICT_VERSION_KEY, ADD_REQUEST_VERSION_NUMBER);
        info.put(MpM_REQREP_DICT_DETAILS_KEY, "Add to the running sum\n"
                 "Input: one or more numeric values\n"
                 "Output: the current running sum, including the new values");
        yarp::os::Value   keywords;
        Common::Package * asList = keywords.asList();
        
        asList->addString(request);
        info.put(MpM_REQREP_DICT_KEYWORDS_KEY, keywords);
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT();//####
} // AddRequestHandler::fillInDescription

bool AddRequestHandler::processRequest(const yarp::os::ConstString & request,
                                       const Common::Package &       restOfInput,
                                       const yarp::os::ConstString & senderChannel,
                                       yarp::os::ConnectionWriter *  replyMechanism)
{
#if (! defined(OD_ENABLE_LOGGING))
# if MAC_OR_LINUX_
#  pragma unused(request)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING)
    OD_LOG_OBJENTER();//####
    OD_LOG_S3("request = ", request.c_str(), "restOfInput = ", restOfInput.toString().c_str(), "senderChannel = ",//####
              senderChannel.c_str());//####
    OD_LOG_P1("replyMechanism = ", replyMechanism);//####
    bool result = true;

    try
    {
        double          total = 0.0;
        int             count = restOfInput.size();
        Common::Package response;
        
        if (1 < count)
        {
            int tally = 0;
            
            for (int ii = 0; ii < count; ++ii)
            {
                yarp::os::Value incoming(restOfInput.get(ii));
                
                if (incoming.isInt())
                {
                    ++tally;
                    total = _service.addToSum(senderChannel, incoming.asInt());
                }
                else if (incoming.isDouble())
                {
                    ++tally;
                    total = _service.addToSum(senderChannel, incoming.asDouble());
                }
            }
            if (tally)
            {
                response.addDouble(total);
            }
            else
            {
                OD_LOG("! (tally)");//####
                response.addString(MpM_FAILED_RESPONSE);
            }
        }
        else
        {
            OD_LOG("! (1 == restOfInput.size())");//####
            response.addString(MpM_FAILED_RESPONSE);
        }
        if (replyMechanism)
        {
            if (! response.write(*replyMechanism))
            {
                OD_LOG("(! response.write(*replyMechanism))");//####
#if defined(MpM_STALL_ON_SEND_PROBLEM)
                Common::Stall();
#endif // defined(MpM_STALL_ON_SEND_PROBLEM)
            }
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT_B(result);//####
    return result;
} // AddRequestHandler::processRequest

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
