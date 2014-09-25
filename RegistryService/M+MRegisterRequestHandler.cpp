//--------------------------------------------------------------------------------------------------
//
//  File:       M+MRegisterRequestHandler.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for the request handler for the 'register' request.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2014 by HPlus Technologies Ltd. and Simon Fraser University.
//
//              All rights reserved. Redistribution and use in source and binary forms, with or
//              without modification, are permitted provided that the following conditions are met:
//                * Redistributions of source code must retain the above copyright notice, this list
//                  of conditions and the following disclaimer.
//                * Redistributions in binary form must reproduce the above copyright notice, this
//                  list of conditions and the following disclaimer in the documentation and/or
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
//  Created:    2014-03-03
//
//--------------------------------------------------------------------------------------------------

#include "M+MRegisterRequestHandler.h"
#include "M+MRegistryService.h"

#include <mpm/M+MClientChannel.h>
#include <mpm/M+MEndpoint.h>
#include <mpm/M+MRequests.h>
#include <mpm/M+MServiceResponse.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The class definition for the request handler for the 'register' request. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Registry;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief The protocol version number for the 'register' request. */
#define REGISTER_REQUEST_VERSION_NUMBER "1.0"

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

RegisterRequestHandler::RegisterRequestHandler(RegistryService & service) :
    inherited(MpM_REGISTER_REQUEST), _service(service)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P1("service = ", &service); //####
    OD_LOG_EXIT_P(this); //####
} // RegisterRequestHandler::RegisterRequestHandler

RegisterRequestHandler::~RegisterRequestHandler(void)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_OBJEXIT(); //####
} // RegisterRequestHandler::~RegisterRequestHandler

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void RegisterRequestHandler::fillInAliases(StringVector & alternateNames)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("alternateNames = ", &alternateNames); //####
    alternateNames.push_back("remember");
    OD_LOG_OBJEXIT(); //####
} // RegisterRequestHandler::fillInAliases

void RegisterRequestHandler::fillInDescription(const yarp::os::ConstString & request,
                                               yarp::os::Property &          info)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_S1s("request = ", request); //####
    OD_LOG_P1("info = ", &info); //####
    try
    {
        info.put(MpM_REQREP_DICT_REQUEST_KEY, request);
        info.put(MpM_REQREP_DICT_INPUT_KEY, MpM_REQREP_STRING);
        info.put(MpM_REQREP_DICT_OUTPUT_KEY, MpM_REQREP_STRING);
        info.put(MpM_REQREP_DICT_VERSION_KEY, REGISTER_REQUEST_VERSION_NUMBER);
        info.put(MpM_REQREP_DICT_DETAILS_KEY, "Register the service and its requests\n"
                 "Input: the channel used by the service\n"
                 "Output: OK or FAILED, with a description of the problem encountered");
        yarp::os::Value    keywords;
        yarp::os::Bottle * asList = keywords.asList();
        
        asList->addString(request);
        asList->addString("add");
        info.put(MpM_REQREP_DICT_KEYWORDS_KEY, keywords);
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT(); //####
} // RegisterRequestHandler::fillInDescription

bool RegisterRequestHandler::processRequest(const yarp::os::ConstString & request,
                                            const yarp::os::Bottle &      restOfInput,
                                            const yarp::os::ConstString & senderChannel,
                                            yarp::os::ConnectionWriter *  replyMechanism)
{
#if (! defined(OD_ENABLE_LOGGING))
# if MAC_OR_LINUX_
#  pragma unused(request,senderChannel)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING)
    OD_LOG_OBJENTER(); //####
    OD_LOG_S3s("request = ", request, "restOfInput = ", restOfInput.toString(), //####
               "senderChannel = ", senderChannel); //####
    OD_LOG_P1("replyMechanism = ", replyMechanism); //####
    bool result = true;
    
    try
    {
        if (replyMechanism)
        {
            OD_LOG("(replyMechanism)"); //####
            yarp::os::Bottle reply;
            
            // Validate the name as a channel name
            if (1 == restOfInput.size())
            {
                yarp::os::Value argument(restOfInput.get(0));
                
                if (argument.isString())
                {
                    yarp::os::ConstString argAsString(argument.toString());
                    
                    if (Endpoint::CheckEndpointName(argAsString))
                    {
                        _service.reportStatusChange(argAsString,
                                        RegistryService::ServiceStatus::kRegistryRegisterService);
                        // Send a 'name' request to the channel
                        yarp::os::ConstString aName = GetRandomChannelName(HIDDEN_CHANNEL_PREFIX
                                                                           "register_/"
                                                                           DEFAULT_CHANNEL_ROOT);
                        ClientChannel *       outChannel = new ClientChannel;
                        
                        if (outChannel)
                        {
                            if (outChannel->openWithRetries(aName, STANDARD_WAIT_TIME))
                            {
                                if (outChannel->addOutputWithRetries(argAsString,
                                                                     STANDARD_WAIT_TIME))
                                {
                                    yarp::os::Bottle message1(MpM_NAME_REQUEST);
                                    yarp::os::Bottle response;
                                    
                                    if (outChannel->write(message1, response))
                                    {
                                        if (_service.processNameResponse(argAsString, response))
                                        {
                                            yarp::os::Bottle message2(MpM_LIST_REQUEST);
                                            
                                            if (outChannel->write(message2, response))
                                            {
                                                if (_service.processListResponse(argAsString,
                                                                                 response))
                                                {
                                                    // Remember the response
                                                    reply.addString(MpM_OK_RESPONSE);
                                                    // If we're registering the Service Registry, we
                                                    // don't care about timeouts!
                                                    if (argAsString != MpM_REGISTRY_CHANNEL_NAME)
                                                    {
                                                _service.updateCheckedTimeForChannel(argAsString);
                                                    }
                                                }
                                                else
                                                {
                                                    OD_LOG("! (_service.processListResponse(" //####
                                                           "argAsString, response))"); //####
                                                    reply.addString(MpM_FAILED_RESPONSE);
                                                    reply.addString("Invalid response to '"
                                                                    MpM_LIST_REQUEST "' request");
                                                }
                                            }
                                            else
                                            {
                                                OD_LOG("! (outChannel->write(message2, " //####
                                                       "response))"); //####
                                                reply.addString(MpM_FAILED_RESPONSE);
                                                reply.addString("Could not write to channel");
#if defined(MpM_StallOnSendProblem)
                                                Stall();
#endif // defined(MpM_StallOnSendProblem)
                                            }
                                        }
                                        else
                                        {
                                            OD_LOG("! (_service.processNameResponse(" //####
                                                   "argAsString, response))"); //####
                                            reply.addString(MpM_FAILED_RESPONSE);
                                            reply.addString("Invalid response to '"
                                                            MpM_NAME_REQUEST "' request");
                                        }
                                    }
                                    else
                                    {
                                        OD_LOG("! (outChannel->write(message1, response))"); //####
                                        reply.addString(MpM_FAILED_RESPONSE);
                                        reply.addString("Could not write to channel");
#if defined(MpM_StallOnSendProblem)
                                        Stall();
#endif // defined(MpM_StallOnSendProblem)
                                    }
#if defined(MpM_DoExplicitDisconnect)
                                if (! Utilities::NetworkDisconnectWithRetries(outChannel->name(),
                                                                              argAsString,
                                                                              STANDARD_WAIT_TIME,
                                                                              nullptr, nullptr))
                                    {
                                        OD_LOG("(! Utilities::NetworkDisconnectWithRetries(" //####
                                               "outChannel->name(), argAsString, " //####
                                               "STANDARD_WAIT_TIME, nullptr, nullptr))"); //####
                                    }
#endif // defined(MpM_DoExplicitDisconnect)
                                }
                                else
                                {
                                    OD_LOG("! (outChannel->addOutputWithRetries(" //####
                                           "argAsString, STANDARD_WAIT_TIME))"); //####
                                    reply.addString(MpM_FAILED_RESPONSE);
                                    reply.addString("Could not connect to channel");
                                    reply.addString(argAsString);
                                }
#if defined(MpM_DoExplicitClose)
                                outChannel->close();
#endif // defined(MpM_DoExplicitClose)
                            }
                            else
                            {
                                OD_LOG("! (outChannel->openWithRetries(aName, " //####
                                       "STANDARD_WAIT_TIME))"); //####
                                reply.addString(MpM_FAILED_RESPONSE);
                                reply.addString("Channel could not be opened");
                            }
                            ClientChannel::RelinquishChannel(outChannel);
                        }
                        else
                        {
                            OD_LOG("! (outChannel)");
                        }
                    }
                    else
                    {
                        OD_LOG("! (Endpoint::CheckEndpointName(argAsString))"); //####
                        reply.addString(MpM_FAILED_RESPONSE);
                        reply.addString("Invalid channel name");
                    }
                }
                else
                {
                    OD_LOG("! (argument.isString())"); //####
                    reply.addString(MpM_FAILED_RESPONSE);
                    reply.addString("Invalid channel name");
                }
            }
            else
            {
                OD_LOG("! (1 == restOfInput.size())"); //####
                reply.addString(MpM_FAILED_RESPONSE);
                reply.addString("Missing channel name or extra arguments to request");
            }
            OD_LOG_S1s("reply <- ", reply.toString()); //####
            if (! reply.write(*replyMechanism))
            {
                OD_LOG("(! reply.write(*replyMechanism))"); //####
#if defined(MpM_StallOnSendProblem)
                Stall();
#endif // defined(MpM_StallOnSendProblem)
            }
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // RegisterRequestHandler::processRequest

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
