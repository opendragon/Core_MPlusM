//--------------------------------------------------------------------------------------------------
//
//  File:       m+mMatchRequestHandler.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the request handler for the 'match' request.
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
//  Created:    2014-03-10
//
//--------------------------------------------------------------------------------------------------

#include "m+mMatchRequestHandler.hpp"
#include "m+mRegistryService.hpp"

#include <m+m/m+mEndpoint.hpp>
#include <m+m/m+mRequests.hpp>

//#include <odlEnable.h>
#include <odlInclude.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the request handler for the 'match' request. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Parser;
using namespace MplusM::Registry;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief The protocol version number for the 'match' request. */
#define MATCH_REQUEST_VERSION_NUMBER_ "1.0"

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

MatchRequestHandler::MatchRequestHandler(RegistryService &           service,
                                         Parser::BaseNameValidator * validator) :
    inherited(MpM_MATCH_REQUEST_, service), _validator(validator)
{
    ODL_ENTER(); //####
    ODL_P2("service = ", &service, "validator = ", validator); //####
    ODL_EXIT_P(this); //####
} // MatchRequestHandler::MatchRequestHandler

MatchRequestHandler::~MatchRequestHandler(void)
{
    ODL_OBJENTER(); //####
    ODL_OBJEXIT(); //####
} // MatchRequestHandler::~MatchRequestHandler

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void
MatchRequestHandler::fillInAliases(YarpStringVector & alternateNames)
{
    ODL_OBJENTER(); //####
    ODL_P1("alternateNames = ", &alternateNames); //####
    alternateNames.push_back("find");
    ODL_OBJEXIT(); //####
} // MatchRequestHandler::fillInAliases

void
MatchRequestHandler::fillInDescription(const YarpString &   request,
                                       yarp::os::Property & info)
{
    ODL_OBJENTER(); //####
    ODL_S1s("request = ", request); //####
    ODL_P1("info = ", &info); //####
    try
    {
        info.put(MpM_REQREP_DICT_REQUEST_KEY_, request);
        info.put(MpM_REQREP_DICT_INPUT_KEY_, MpM_REQREP_INT_ MpM_REQREP_STRING_);
        info.put(MpM_REQREP_DICT_OUTPUT_KEY_, MpM_REQREP_LIST_START_ MpM_REQREP_STRING_
                 MpM_REQREP_0_OR_MORE_ MpM_REQREP_LIST_END_);
        info.put(MpM_REQREP_DICT_VERSION_KEY_, MATCH_REQUEST_VERSION_NUMBER_);
        info.put(MpM_REQREP_DICT_DETAILS_KEY_, T_("Find a matching service\n"
                                                  "Input: an integer (1=return names, 0=return "
                                                  "ports) and an expression describing the service "
                                                  "to be found\n"
                                                  "Output: OK and a list of matching service "
                                                  "names/ports or FAILED, with a description of "
                                                  "the problem encountered"));
        yarp::os::Value    keywords;
        yarp::os::Bottle * asList = keywords.asList();

        asList->addString(request);
        asList->addString("find");
        info.put(MpM_REQREP_DICT_KEYWORDS_KEY_, keywords);
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT(); //####
} // MatchRequestHandler::fillInDescription

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
bool
MatchRequestHandler::processRequest(const YarpString &           request,
                                    const yarp::os::Bottle &     restOfInput,
                                    const YarpString &           senderChannel,
                                    yarp::os::ConnectionWriter * replyMechanism)
{
#if (! defined(OD_ENABLE_LOGGING_))
# if MAC_OR_LINUX_
#  pragma unused(request,senderChannel)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING_)
    ODL_OBJENTER(); //####
    ODL_S3s("request = ", request, "restOfInput = ", restOfInput.toString(), //####
            "senderChannel = ", senderChannel); //####
    ODL_P1("replyMechanism = ", replyMechanism); //####
    bool result = true;

    try
    {
        // We are expecting an integer and a string as the parameter
        _response.clear();
        if (2 == restOfInput.size())
        {
            yarp::os::Value condition(restOfInput.get(0));
            yarp::os::Value argument(restOfInput.get(1));

            if (condition.isInt() && argument.isString())
            {
                int        conditionAsInt = condition.asInt();
                YarpString argAsString(argument.toString());

                ODL_S1s("argAsString <- ", argAsString); //####
                size_t                    endPos;
                Parser::MatchExpression * matcher =
                                        Parser::MatchExpression::CreateMatcher(argAsString,
                                                                               argAsString.length(),
                                                                               0, endPos,
                                                                               _validator);

                if (matcher)
                {
                    ODL_LOG("(matcher)"); //####
                    // Hand off the processing to the Registry Service. First, put the 'OK' response
                    // in the output buffer, as we have successfully parsed the request.
                    _response.addString(MpM_OK_RESPONSE_);
                    if (! static_cast<RegistryService &>(_service).processMatchRequest(matcher,
                                                                               0 != conditionAsInt,
                                                                                       _response))
                    {
                        ODL_LOG("(! static_cast<RegistryService &>(_service)." //####
                                "processMatchRequest(matcher, 0 != conditionAsInt, " //####
                                "_response))"); //####
                        _response.clear();
                        _response.addString(MpM_FAILED_RESPONSE_);
                        _response.addString("Invalid criteria");
                    }
                    delete matcher;
                }
                else
                {
                    ODL_LOG("! (matcher)"); //####
                    _response.addString(MpM_FAILED_RESPONSE_);
                    _response.addString("Invalid criteria");
                }
            }
            else
            {
                ODL_LOG("! (argument.isString())"); //####
                _response.addString(MpM_FAILED_RESPONSE_);
                _response.addString("Invalid criteria");
            }
        }
        else
        {
            ODL_LOG("! (1 == restOfInput.size())"); //####
            _response.addString(MpM_FAILED_RESPONSE_);
            _response.addString("Missing criteria or extra arguments to request");
        }
        sendResponse(replyMechanism);
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT_B(result); //####
    return result;
} // MatchRequestHandler::processRequest
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
