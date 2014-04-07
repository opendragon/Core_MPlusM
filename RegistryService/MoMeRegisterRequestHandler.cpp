//--------------------------------------------------------------------------------------
//
//  File:       MoMeRegisterRequestHandler.cpp
//
//  Project:    MoAndMe
//
//  Contains:   The class definition for the request handler for the standard 'register'
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
//  Created:    2014-03-03
//
//--------------------------------------------------------------------------------------

#include "MoMeRegisterRequestHandler.h"
#include "MoMeClientChannel.h"
#include "MoMeEndpoint.h"
#include "MoMeRegistryService.h"
#include "MoMeRequests.h"
#include "MoMeServiceResponse.h"

//#include "ODEnableLogging.h"
#include "ODLogging.h"

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wc++11-extensions"
# pragma clang diagnostic ignored "-Wdocumentation"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# pragma clang diagnostic ignored "-Wpadded"
# pragma clang diagnostic ignored "-Wshadow"
# pragma clang diagnostic ignored "-Wunused-parameter"
# pragma clang diagnostic ignored "-Wweak-vtables"
#endif // defined(__APPLE__)
#include <yarp/os/Time.h>
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The class definition for the request handler for the standard 'register' request. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MoAndMe::Registry;

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
# pragma mark Constructors and destructors
#endif // defined(__APPLE__)

RegisterRequestHandler::RegisterRequestHandler(RegistryService & service) :
        inherited(MAM_REGISTER_REQUEST), _service(service)
{
    OD_LOG_ENTER();//####
    OD_LOG_P1("service = ", &service);//####
    OD_LOG_EXIT_P(this);//####
} // RegisterRequestHandler::RegisterRequestHandler

RegisterRequestHandler::~RegisterRequestHandler(void)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_OBJEXIT();//####
} // RegisterRequestHandler::~RegisterRequestHandler

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

void RegisterRequestHandler::fillInAliases(Common::StringVector & alternateNames)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_P1("alternateNames = ", &alternateNames);//####
    alternateNames.push_back("remember");
    OD_LOG_OBJEXIT();//####
} // RegisterRequestHandler::fillInAliases

void RegisterRequestHandler::fillInDescription(const yarp::os::ConstString & request,
                                               yarp::os::Property &          info)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_S1("request = ", request.c_str());//####
    OD_LOG_P1("info = ", &info);//####
    try
    {
        info.put(MAM_REQREP_DICT_REQUEST_KEY, request);
        info.put(MAM_REQREP_DICT_INPUT_KEY, MAM_REQREP_STRING);
        info.put(MAM_REQREP_DICT_OUTPUT_KEY, MAM_REQREP_STRING);
        info.put(MAM_REQREP_DICT_VERSION_KEY, REGISTER_REQUEST_VERSION_NUMBER);
        info.put(MAM_REQREP_DICT_DETAILS_KEY, "Register the service and its requests");
        yarp::os::Value   keywords;
        Common::Package * asList = keywords.asList();
        
        asList->addString(request);
        asList->addString("add");
        info.put(MAM_REQREP_DICT_KEYWORDS_KEY, keywords);
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT();//####
} // RegisterRequestHandler::fillInDescription

bool RegisterRequestHandler::processRequest(const yarp::os::ConstString & request,
                                            const Common::Package &       restOfInput,
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
            Common::Package reply;
            
            // Validate the name as a channel name
            if (1 == restOfInput.size())
            {
                yarp::os::Value argument(restOfInput.get(0));
                
                if (argument.isString())
                {
                    yarp::os::ConstString argAsString(argument.toString());
                    
                    if (Common::Endpoint::CheckEndpointName(argAsString))
                    {
                        // Send a 'list' request to the channel
                        yarp::os::ConstString   aName(Common::GetRandomChannelName("register/channel_"));
                        Common::ClientChannel * outChannel = new Common::ClientChannel;
                        
                        if (outChannel)
                        {
                            if (outChannel->open(aName))
                            {
                                if (outChannel->addOutputWithRetries(argAsString))
                                {
                                    Common::Package message1(MAM_NAME_REQUEST);
                                    Common::Package response;
                                    
                                    if (outChannel->write(message1, response))
                                    {
                                        if (processNameResponse(argAsString, response))
                                        {
                                            Common::Package message2(MAM_LIST_REQUEST);
                                            
                                            if (outChannel->write(message2, response))
                                            {
                                                if (processListResponse(argAsString, response))
                                                {
                                                    // Remember the response
                                                    reply.addString(MAM_OK_RESPONSE);
                                                }
                                                else
                                                {
                                                    OD_LOG("! (processListResponse(argAsString, response))");//####
                                                    reply.addString(MAM_FAILED_RESPONSE);
                                                    reply.addString("Invalid response to 'list' request");
                                                }
                                            }
                                            else
                                            {
                                                OD_LOG("! (outChannel->write(message2, response))");//####
                                                reply.addString(MAM_FAILED_RESPONSE);
                                                reply.addString("Could not write to channel");
                                            }
                                        }
                                        else
                                        {
                                            OD_LOG("! (processNameResponse(argAsString, response))");//####
                                            reply.addString(MAM_FAILED_RESPONSE);
                                            reply.addString("Invalid response to 'name' request");
                                        }
                                    }
                                    else
                                    {
                                        OD_LOG("! (outChannel->write(message1, response))");//####
                                        reply.addString(MAM_FAILED_RESPONSE);
                                        reply.addString("Could not write to channel");
                                    }
#if defined(MAM_DO_EXPLICIT_DISCONNECT)
                                    if (! Common::NetworkDisconnectWithRetries(outChannel->getName(), argAsString))
                                    {
                                        OD_LOG("(! Common::NetworkDisconnectWithRetries(outChannel->getName(), "//####
                                               "argAsString))");//####
                                    }
#endif // defined(MAM_DO_EXPLICIT_DISCONNECT)
                                }
                                else
                                {
                                    OD_LOG("! (outChannel->addOutputWithRetries(argAsString))");//####
                                    reply.addString(MAM_FAILED_RESPONSE);
                                    reply.addString("Could not connect to channel");
                                    reply.addString(argAsString);
                                }
#if defined(MAM_DO_EXPLICIT_CLOSE)
                                outChannel->close();
#endif // defined(MAM_DO_EXPLICIT_CLOSE)
                            }
                            else
                            {
                                OD_LOG("! (outChannel->open(aName))");//####
                                reply.addString(MAM_FAILED_RESPONSE);
                                reply.addString("Channel could not be opened");
                            }
                            Common::ClientChannel::RelinquishChannel(outChannel);
                        }
                        else
                        {
                            OD_LOG("! (outChannel)");
                        }
                    }
                    else
                    {
                        OD_LOG("! (Common::Endpoint::CheckEndpointName(argAsString))");//####
                        reply.addString(MAM_FAILED_RESPONSE);
                        reply.addString("Invalid channel name");
                    }
                }
                else
                {
                    OD_LOG("! (argument.isString())");//####
                    reply.addString(MAM_FAILED_RESPONSE);
                    reply.addString("Invalid channel name");
                }
            }
            else
            {
                OD_LOG("! (1 == restOfInput.size())");//####
                reply.addString(MAM_FAILED_RESPONSE);
                reply.addString("Missing channel name or extra arguments to request");
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
} // RegisterRequestHandler::processRequest

bool RegisterRequestHandler::processListResponse(const yarp::os::ConstString &   channelName,
                                                 const Common::ServiceResponse & response)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_S2("channelName = ", channelName.c_str(), "response = ", response.asString().c_str());//####
    bool result = false;

    try
    {
        int  count = response.count();
        
        if (0 < count)
        {
            result = true;
            for (int ii = 0; result && (ii < count); ++ii)
            {
                yarp::os::Value anElement(response.element(ii));
                
                if (anElement.isDict())
                {
                    yarp::os::Property * asDict = anElement.asDict();
                    
                    if (asDict->check(MAM_REQREP_DICT_REQUEST_KEY))
                    {
                        yarp::os::ConstString theRequest(asDict->find(MAM_REQREP_DICT_REQUEST_KEY).asString());
                        Common::Package       keywordList;
                        RequestDescription    requestDescriptor;
                        
                        OD_LOG_S1("theRequest <- ", theRequest.c_str());//####
                        if (asDict->check(MAM_REQREP_DICT_DETAILS_KEY))
                        {
                            yarp::os::Value theDetails = asDict->find(MAM_REQREP_DICT_DETAILS_KEY);
                            
                            OD_LOG_S1("theDetails <- ", theDetails.toString().c_str());//####
                            if (theDetails.isString())
                            {
                                requestDescriptor._details = theDetails.toString();
                            }
                            else
                            {
                                OD_LOG("! (theDetails.isString())");//####
                                // The details field is present, but it's not a string.
                                result = false;
                            }
                        }
                        if (asDict->check(MAM_REQREP_DICT_INPUT_KEY))
                        {
                            yarp::os::Value theInputs = asDict->find(MAM_REQREP_DICT_INPUT_KEY);
                            
                            OD_LOG_S1("theInputs <- ", theInputs.toString().c_str());//####
                            if (theInputs.isString())
                            {
                                requestDescriptor._inputs = theInputs.toString();
                            }
                            else
                            {
                                OD_LOG("! (theInputs.isString())");//####
                                // The inputs descriptor is present, but it's not a string
                                result = false;
                            }
                        }
                        if (asDict->check(MAM_REQREP_DICT_KEYWORDS_KEY))
                        {
                            yarp::os::Value theKeywords = asDict->find(MAM_REQREP_DICT_KEYWORDS_KEY);
                            
                            OD_LOG_S1("theKeywords <- ", theKeywords.toString().c_str());//####
                            if (theKeywords.isList())
                            {
                                keywordList = *theKeywords.asList();
                            }
                            else
                            {
                                OD_LOG("! (theKeywords.isList())");//####
                                // The keywords entry is present, but it's not a list
                                result = false;
                            }
                        }
                        if (asDict->check(MAM_REQREP_DICT_OUTPUT_KEY))
                        {
                            yarp::os::Value theOutputs = asDict->find(MAM_REQREP_DICT_OUTPUT_KEY);
                            
                            OD_LOG_S1("theOutputs <- ", theOutputs.toString().c_str());//####
                            if (theOutputs.isString())
                            {
                                requestDescriptor._outputs = theOutputs.toString();
                            }
                            else
                            {
                                OD_LOG("! (theOutputs.isString())");//####
                                // The outputs descriptor is present, but it's not a string
                                result = false;
                            }
                        }
                        if (asDict->check(MAM_REQREP_DICT_VERSION_KEY))
                        {
                            yarp::os::Value theVersion = asDict->find(MAM_REQREP_DICT_VERSION_KEY);
                            
                            OD_LOG_S1("theVersion <- ", theVersion.toString().c_str());//####
                            if (theVersion.isString() || theVersion.isInt() || theVersion.isDouble())
                            {
                                requestDescriptor._version = theVersion.toString();
                            }
                            else
                            {
                                OD_LOG("! (theVersion.isString() || theVersion.isInt() || "//####
                                          "theVersion.isDouble())");//####
                                // The version entry is present, but it's not a simple value
                                result = false;
                            }
                        }
                        if (result)
                        {
                            requestDescriptor._channel = channelName;
                            requestDescriptor._request = theRequest;
                            result = _service.addRequestRecord(keywordList, requestDescriptor);
                            OD_LOG_B1("result <- ", result);//####
                        }
                    }
                    else
                    {
                        OD_LOG("! (asDict->check(MAM_REQREP_DICT_REQUEST_KEY))");//####
                        // There is no 'name' entry in this dictionary
                        result = false;
                    }
                }
                else
                {
                    OD_LOG("! (anElement.isDict())");//####
                    // One of the values is not a dictionary
                    result = false;
                }
            }
        }
        else
        {
            OD_LOG("! (0 < count)");//####
            // Wrong number of values in the response.
            result = false;
        }
        if (! result)
        {
            // We need to remove any values that we've recorded for this channel!
            _service.removeServiceRecord(channelName);
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT_B(result);//####
    return result;
} // RegisterRequestHandler::processListResponse

bool RegisterRequestHandler::processNameResponse(const yarp::os::ConstString &   channelName,
                                                 const Common::ServiceResponse & response)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_S2("channelName = ", channelName.c_str(), "response = ", response.asString().c_str());//####
    bool result = false;
    
    try
    {
        if (2 == response.count())
        {
            yarp::os::Value theCanonicalName(response.element(0));
            yarp::os::Value theDescription(response.element(1));
            
            if (theCanonicalName.isString() && theDescription.isString())
            {
                result = _service.addServiceRecord(channelName, theCanonicalName.toString(), theDescription.toString());
            }
            else
            {
                OD_LOG("! (theCanonicalName.isString() && theDescription.isString())");//####
                // The canonical name and description are present, but at least one of them is not a string
                result = false;
            }
        }
        else
        {
            OD_LOG("! (2 == response.count())");//####
            OD_LOG_S1("response = ", response.asString().c_str());//####
            // Wrong number of values in the response.
            result = false;
        }
        if (! result)
        {
            // We need to remove any values that we've recorded for this channel!
            _service.removeServiceRecord(channelName);
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT_B(result);//####
    return result;
} // RegisterRequestHandler::processNameResponse

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
