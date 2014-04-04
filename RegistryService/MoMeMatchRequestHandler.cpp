//--------------------------------------------------------------------------------------
//
//  File:       MoMeMatchRequestHandler.cpp
//
//  Project:    MoAndMe
//
//  Contains:   The class definition for the request handler for the standard 'match'
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
//  Created:    2014-03-10
//
//--------------------------------------------------------------------------------------

#include "MoMeMatchRequestHandler.h"
#include "MoMeBaseNameValidator.h"
#include "MoMeEndpoint.h"
#include "MoMeMatchExpression.h"
#include "MoMeRegistryService.h"
#include "MoMeRequests.h"

//#include "ODEnableLogging.h"
#include "ODLogging.h"

#include <cstdlib>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The class definition for the request handler for the standard 'match' request. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MoAndMe::Registry;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief The protocol version number for the 'list' request. */
#define MATCH_REQUEST_VERSION_NUMBER "1.0"

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and destructors
#endif // defined(__APPLE__)

MatchRequestHandler::MatchRequestHandler(RegistryService &           service,
                                         Parser::BaseNameValidator * validator) :
        inherited(MAM_MATCH_REQUEST), _service(service), _validator(validator)
{
    OD_LOG_ENTER();//####
    OD_LOG_P2("service = ", &service, "validator = ", validator);//####
    OD_LOG_EXIT_P(this);//####
} // MatchRequestHandler::MatchRequestHandler

MatchRequestHandler::~MatchRequestHandler(void)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_OBJEXIT();//####
} // MatchRequestHandler::~MatchRequestHandler

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

void MatchRequestHandler::fillInAliases(StringVector & alternateNames)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_P1("alternateNames = ", &alternateNames);//####
    alternateNames.push_back("find");
    OD_LOG_OBJEXIT();//####
} // MatchRequestHandler::fillInAliases

void MatchRequestHandler::fillInDescription(const yarp::os::ConstString & request,
                                            yarp::os::Property &          info)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_S1("request = ", request.c_str());//####
    OD_LOG_P1("info = ", &info);//####
    try
    {
        info.put(MAM_REQREP_DICT_REQUEST_KEY, request);
        info.put(MAM_REQREP_DICT_INPUT_KEY, MAM_REQREP_STRING MAM_REQREP_1_OR_MORE);
        info.put(MAM_REQREP_DICT_OUTPUT_KEY, MAM_REQREP_LIST_START MAM_REQREP_STRING MAM_REQREP_0_OR_MORE
                 MAM_REQREP_LIST_END);
        info.put(MAM_REQREP_DICT_VERSION_KEY, MATCH_REQUEST_VERSION_NUMBER);
        info.put(MAM_REQREP_DICT_DETAILS_KEY, "Find a matching service");
        yarp::os::Value keywords;
        Package *       asList = keywords.asList();
        
        asList->addString(request);
        asList->addString("find");
        info.put(MAM_REQREP_DICT_KEYWORDS_KEY, keywords);
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT();//####
} // MatchRequestHandler::fillInDescription

bool MatchRequestHandler::processRequest(const yarp::os::ConstString & request,
                                         const Package &               restOfInput,
                                         const yarp::os::ConstString & senderChannel,
                                         yarp::os::ConnectionWriter *  replyMechanism)
{
#if (! defined(OD_ENABLE_LOGGING))
# pragma unused(request,senderChannel)
#endif // ! defined(OD_ENABLE_LOGGING)
    OD_LOG_OBJENTER();//####
    OD_LOG_S3("request = ", request.c_str(), "restOfInput = ", restOfInput.toString().c_str(), "senderChannel = ",//####
              senderChannel.c_str());//####
    OD_LOG_P1("replyMechanism = ", replyMechanism);//####
    bool result = true;
    
    try
    {
        if (replyMechanism)
        {
            Package reply;
            
            // We are expecting just one string as the parameter
            if (1 == restOfInput.size())
            {
                yarp::os::Value argument(restOfInput.get(0));
                
                if (argument.isString())
                {
                    yarp::os::ConstString argAsString(argument.toString());
                    
                    OD_LOG_S1("argAsString <- ", argAsString.c_str());//####
                    int                       endPos;
                    Parser::MatchExpression * matcher = Parser::MatchExpression::CreateMatcher(argAsString,
                                                                                               argAsString.length(), 0,
                                                                                               endPos, _validator);
                    
                    if (matcher)
                    {
                        OD_LOG("(matcher)");//####
                        // Hand off the processing to the registry service. First, put the 'OK' response in the output
                        // buffer, as we have successfully parsed the request.
                        reply.addString(MAM_OK_RESPONSE);
                        if (! _service.processMatchRequest(matcher, reply))
                        {
                            OD_LOG("(! _service.processMatchRequest(matcher, reply))");//####
                            reply.clear();
                            reply.addString(MAM_FAILED_RESPONSE);
                            reply.addString("Invalid criteria");
                        }
                        delete matcher;
                    }
                    else
                    {
                        OD_LOG("! (matcher)");//####
                        reply.addString(MAM_FAILED_RESPONSE);
                        reply.addString("Invalid criteria");
                    }
                }
                else
                {
                    OD_LOG("! (argument.isString())");//####
                    reply.addString(MAM_FAILED_RESPONSE);
                    reply.addString("Invalid criteria");
                }
            }
            else
            {
                OD_LOG("! (1 == restOfInput.size())");//####
                reply.addString(MAM_FAILED_RESPONSE);
                reply.addString("Missing criteria or extra arguments to request");
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
} // MatchRequestHandler::processRequest

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)