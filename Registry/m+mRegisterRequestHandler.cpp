//--------------------------------------------------------------------------------------------------
//
//  File:       m+mRegisterRequestHandler.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the request handler for the 'register' request.
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
//  Created:    2014-03-03
//
//--------------------------------------------------------------------------------------------------

#include "m+mRegisterRequestHandler.h"

#include "m+mRegistryService.h"

#include <m+m/m+mClientChannel.h>
#include <m+m/m+mEndpoint.h>
#include <m+m/m+mRequests.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the request handler for the 'register' request. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Registry;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief The protocol version number for the 'register' request. */
#define REGISTER_REQUEST_VERSION_NUMBER_ "1.0"

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

RegisterRequestHandler::RegisterRequestHandler(RegistryService & service) :
    inherited(MpM_REGISTER_REQUEST_, service)
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

DEFINE_FILLINALIASES_(RegisterRequestHandler)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("alternateNames = ", &alternateNames); //####
    alternateNames.push_back("remember");
    OD_LOG_OBJEXIT(); //####
} // RegisterRequestHandler::fillInAliases

DEFINE_FILLINDESCRIPTION_(RegisterRequestHandler)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_S1s("request = ", request); //####
    OD_LOG_P1("info = ", &info); //####
    try
    {
        info.put(MpM_REQREP_DICT_REQUEST_KEY_, request);
        info.put(MpM_REQREP_DICT_INPUT_KEY_, MpM_REQREP_STRING_);
        info.put(MpM_REQREP_DICT_OUTPUT_KEY_, MpM_REQREP_STRING_);
        info.put(MpM_REQREP_DICT_VERSION_KEY_, REGISTER_REQUEST_VERSION_NUMBER_);
        info.put(MpM_REQREP_DICT_DETAILS_KEY_, "Register the service and its requests\n"
                 "Input: the channel used by the service\n"
                 "Output: OK or FAILED, with a description of the problem encountered");
        yarp::os::Value    keywords;
        yarp::os::Bottle * asList = keywords.asList();
        
        asList->addString(request);
        asList->addString("add");
        info.put(MpM_REQREP_DICT_KEYWORDS_KEY_, keywords);
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT(); //####
} // RegisterRequestHandler::fillInDescription

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
DEFINE_PROCESSREQUEST_(RegisterRequestHandler)
{
#if (! defined(OD_ENABLE_LOGGING_))
# if MAC_OR_LINUX_
#  pragma unused(request,senderChannel)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING_)
    OD_LOG_OBJENTER(); //####
    OD_LOG_S3s("request = ", request, "restOfInput = ", restOfInput.toString(), //####
               "senderChannel = ", senderChannel); //####
    OD_LOG_P1("replyMechanism = ", replyMechanism); //####
    bool result = true;
    
    try
    {
        // Validate the name as a channel name
        _response.clear();
        if (1 == restOfInput.size())
        {
            yarp::os::Value argument(restOfInput.get(0));
            
            if (argument.isString())
            {
                YarpString argAsString(argument.toString());
                
                if (Endpoint::CheckEndpointName(argAsString))
                {
                    RegistryService & theService = static_cast<RegistryService &>(_service);
                    
                    theService.reportStatusChange(argAsString,
                                                  RegistryService::kRegistryRegisterService);
                    // Send a 'name' request to the channel
                    YarpString      aName = GetRandomChannelName(HIDDEN_CHANNEL_PREFIX_
                                                                 "register_/"
                                                                 DEFAULT_CHANNEL_ROOT_);
                    ClientChannel * outChannel = new ClientChannel;
                    
                    if (outChannel)
                    {
                        if (outChannel->openWithRetries(aName, STANDARD_WAIT_TIME_))
                        {
                            if (outChannel->addOutputWithRetries(argAsString,
                                                                 STANDARD_WAIT_TIME_))
                            {
                                yarp::os::Bottle message1(MpM_NAME_REQUEST_);
                                yarp::os::Bottle reply;
                                
                                if (outChannel->write(message1, reply))
                                {
                                    if (theService.processNameResponse(argAsString, reply))
                                    {
                                        yarp::os::Bottle message2(MpM_LIST_REQUEST_);
                                        
                                        if (outChannel->write(message2, reply))
                                        {
                                            if (theService.processListResponse(argAsString, reply))
                                            {
                                                // Remember the response
                                                _response.addString(MpM_OK_RESPONSE_);
                                                // If we're registering the Registry Service, we
                                                // don't care about timeouts!
                                                if (argAsString != MpM_REGISTRY_ENDPOINT_NAME_)
                                                {
                                                theService.updateCheckedTimeForChannel(argAsString);
                                                }
                                            }
                                            else
                                            {
                                                OD_LOG("! (theService.processList" //####
                                                       "Response(argAsString, reply))"); //####
                                                _response.addString(MpM_FAILED_RESPONSE_);
                                                _response.addString("Invalid response to '"
                                                                    MpM_LIST_REQUEST_ "' request");
                                            }
                                        }
                                        else
                                        {
                                            OD_LOG("! (outChannel->write(message2, reply))"); //####
                                            _response.addString(MpM_FAILED_RESPONSE_);
                                            _response.addString("Could not write to channel");
#if defined(MpM_StallOnSendProblem)
                                            Stall();
#endif // defined(MpM_StallOnSendProblem)
                                        }
                                    }
                                    else
                                    {
                                        OD_LOG("! (theService.processNameResponse(" //####
                                               "argAsString, reply))"); //####
                                        _response.addString(MpM_FAILED_RESPONSE_);
                                        _response.addString("Invalid response to '"
                                                            MpM_NAME_REQUEST_ "' request");
                                    }
                                }
                                else
                                {
                                    OD_LOG("! (outChannel->write(message1, reply))"); //####
                                    _response.addString(MpM_FAILED_RESPONSE_);
                                    _response.addString("Could not write to channel");
#if defined(MpM_StallOnSendProblem)
                                    Stall();
#endif // defined(MpM_StallOnSendProblem)
                                }
#if defined(MpM_DoExplicitDisconnect)
                                if (! Utilities::NetworkDisconnectWithRetries(outChannel->name(),
                                                                              argAsString,
                                                                              STANDARD_WAIT_TIME_))
                                {
                                    OD_LOG("(! Utilities::NetworkDisconnectWithRetries(" //####
                                           "outChannel->name(), argAsString, " //####
                                           "STANDARD_WAIT_TIME_))"); //####
                                }
#endif // defined(MpM_DoExplicitDisconnect)
                            }
                            else
                            {
                                OD_LOG("! (outChannel->addOutputWithRetries(" //####
                                       "argAsString, STANDARD_WAIT_TIME_))"); //####
                                _response.addString(MpM_FAILED_RESPONSE_);
                                _response.addString("Could not connect to channel");
                                _response.addString(argAsString);
                            }
#if defined(MpM_DoExplicitClose)
                            outChannel->close();
#endif // defined(MpM_DoExplicitClose)
                        }
                        else
                        {
                            OD_LOG("! (outChannel->openWithRetries(aName, " //####
                                   "STANDARD_WAIT_TIME_))"); //####
                            _response.addString(MpM_FAILED_RESPONSE_);
                            _response.addString("Channel could not be opened");
                        }
                        BaseChannel::RelinquishChannel(outChannel);
                    }
                    else
                    {
                        OD_LOG("! (outChannel)");
                    }
                }
                else
                {
                    OD_LOG("! (Endpoint::CheckEndpointName(argAsString))"); //####
                    _response.addString(MpM_FAILED_RESPONSE_);
                    _response.addString("Invalid channel name");
                }
            }
            else
            {
                OD_LOG("! (argument.isString())"); //####
                _response.addString(MpM_FAILED_RESPONSE_);
                _response.addString("Invalid channel name");
            }
        }
        else
        {
            OD_LOG("! (1 == restOfInput.size())"); //####
            _response.addString(MpM_FAILED_RESPONSE_);
            _response.addString("Missing channel name or extra arguments to request");
        }
        sendResponse(replyMechanism);
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // RegisterRequestHandler::processRequest
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
