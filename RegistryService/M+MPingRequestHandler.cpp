//--------------------------------------------------------------------------------------------------
//
//  File:       M+MPingRequestHandler.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for the request handler for the 'ping' request.
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
//  Created:    2014-06-17
//
//--------------------------------------------------------------------------------------------------

#include "M+MPingRequestHandler.h"
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
 
 @brief The class definition for the request handler for the 'ping' request. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Registry;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief The protocol version number for the 'ping' request. */
#define PING_REQUEST_VERSION_NUMBER "1.0"

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

PingRequestHandler::PingRequestHandler(RegistryService & service) :
    inherited(MpM_PING_REQUEST), _service(service)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P1("service = ", &service); //####
    OD_LOG_EXIT_P(this); //####
} // PingRequestHandler::PingRequestHandler

PingRequestHandler::~PingRequestHandler(void)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_OBJEXIT(); //####
} // PingRequestHandler::~PingRequestHandler

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void PingRequestHandler::fillInAliases(Common::StringVector & alternateNames)
{
#if (! defined(OD_ENABLE_LOGGING))
# if MAC_OR_LINUX_
#  pragma unused(alternateNames)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING)
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("alternateNames = ", &alternateNames); //####
    OD_LOG_OBJEXIT(); //####
} // PingRequestHandler::fillInAliases

void PingRequestHandler::fillInDescription(const yarp::os::ConstString & request,
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
        info.put(MpM_REQREP_DICT_VERSION_KEY, PING_REQUEST_VERSION_NUMBER);
        info.put(MpM_REQREP_DICT_DETAILS_KEY,
                 "Update the last-pinged time for a service or re-register it\n"
                 "Input: the channel used by the service\n"
                 "Output: OK or FAILED, with a description of the problem encountered");
        yarp::os::Value    keywords;
        yarp::os::Bottle * asList = keywords.asList();
        
        asList->addString(request);
        info.put(MpM_REQREP_DICT_KEYWORDS_KEY, keywords);
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT(); //####
} // PingRequestHandler::fillInDescription

bool PingRequestHandler::processListResponse(const yarp::os::ConstString &   channelName,
                                             const Common::ServiceResponse & response)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_S2s("channelName = ", channelName, "response = ", response.asString()); //####
    bool result = false;
    
    try
    {
        int count = response.count();
        
        if (0 < count)
        {
            result = true;
            for (int ii = 0; result && (ii < count); ++ii)
            {
                yarp::os::Value anElement(response.element(ii));
                
                if (anElement.isDict())
                {
                    yarp::os::Property * asDict = anElement.asDict();
                    
                    if (asDict->check(MpM_REQREP_DICT_REQUEST_KEY))
                    {
                        yarp::os::ConstString theRequest =
                        asDict->find(MpM_REQREP_DICT_REQUEST_KEY).asString();
                        yarp::os::Bottle      keywordList;
                        RequestDescription    requestDescriptor;
                        
                        OD_LOG_S1s("theRequest <- ", theRequest); //####
                        if (asDict->check(MpM_REQREP_DICT_DETAILS_KEY))
                        {
                            yarp::os::Value theDetails = asDict->find(MpM_REQREP_DICT_DETAILS_KEY);
                            
                            OD_LOG_S1s("theDetails <- ", theDetails.toString()); //####
                            if (theDetails.isString())
                            {
                                requestDescriptor._details = theDetails.toString();
                            }
                            else
                            {
                                OD_LOG("! (theDetails.isString())"); //####
                                // The details field is present, but it's not a string.
                                result = false;
                            }
                        }
                        if (asDict->check(MpM_REQREP_DICT_INPUT_KEY))
                        {
                            yarp::os::Value theInputs = asDict->find(MpM_REQREP_DICT_INPUT_KEY);
                            
                            OD_LOG_S1s("theInputs <- ", theInputs.toString()); //####
                            if (theInputs.isString())
                            {
                                requestDescriptor._inputs = theInputs.toString();
                            }
                            else
                            {
                                OD_LOG("! (theInputs.isString())"); //####
                                // The inputs descriptor is present, but it's not a string
                                result = false;
                            }
                        }
                        if (asDict->check(MpM_REQREP_DICT_KEYWORDS_KEY))
                        {
                            yarp::os::Value theKeywords =
                            asDict->find(MpM_REQREP_DICT_KEYWORDS_KEY);
                            
                            OD_LOG_S1s("theKeywords <- ", theKeywords.toString()); //####
                            if (theKeywords.isList())
                            {
                                keywordList = *theKeywords.asList();
                            }
                            else
                            {
                                OD_LOG("! (theKeywords.isList())"); //####
                                // The keywords entry is present, but it's not a list
                                result = false;
                            }
                        }
                        if (asDict->check(MpM_REQREP_DICT_OUTPUT_KEY))
                        {
                            yarp::os::Value theOutputs = asDict->find(MpM_REQREP_DICT_OUTPUT_KEY);
                            
                            OD_LOG_S1s("theOutputs <- ", theOutputs.toString()); //####
                            if (theOutputs.isString())
                            {
                                requestDescriptor._outputs = theOutputs.toString();
                            }
                            else
                            {
                                OD_LOG("! (theOutputs.isString())"); //####
                                // The outputs descriptor is present, but it's not a string
                                result = false;
                            }
                        }
                        if (asDict->check(MpM_REQREP_DICT_VERSION_KEY))
                        {
                            yarp::os::Value theVersion = asDict->find(MpM_REQREP_DICT_VERSION_KEY);
                            
                            OD_LOG_S1s("theVersion <- ", theVersion.toString()); //####
                            if (theVersion.isString() || theVersion.isInt() ||
                                theVersion.isDouble())
                            {
                                requestDescriptor._version = theVersion.toString();
                            }
                            else
                            {
                                OD_LOG("! (theVersion.isString() || theVersion.isInt() || " //####
                                       "theVersion.isDouble())"); //####
                                // The version entry is present, but it's not a simple value
                                result = false;
                            }
                        }
                        if (result)
                        {
                            requestDescriptor._channel = channelName;
                            requestDescriptor._request = theRequest;
                            result = _service.addRequestRecord(keywordList, requestDescriptor);
                            OD_LOG_B1("result <- ", result); //####
                            if (! result)
                            {
                                // We need to remove any values that we've recorded for this
                                // channel!
                                _service.removeServiceRecord(channelName);
                            }
                        }
                    }
                    else
                    {
                        OD_LOG("! (asDict->check(MpM_REQREP_DICT_REQUEST_KEY))"); //####
                        // There is no 'name' entry in this dictionary
                        result = false;
                    }
                }
                else
                {
                    OD_LOG("! (anElement.isDict())"); //####
                    // One of the values is not a dictionary
                    result = false;
                }
            }
        }
        else
        {
            OD_LOG("! (0 < count)"); //####
            // Wrong number of values in the response.
            result = false;
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // PingRequestHandler::processListResponse

bool PingRequestHandler::processNameResponse(const yarp::os::ConstString &   channelName,
                                             const Common::ServiceResponse & response)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_S2s("channelName = ", channelName, "response = ", response.asString()); //####
    bool result = false;
    
    try
    {
        if (MpM_EXPECTED_NAME_RESPONSE_SIZE == response.count())
        {
            yarp::os::Value theCanonicalName(response.element(0));
            yarp::os::Value theDescription(response.element(1));
            yarp::os::Value theKind(response.element(2));
            yarp::os::Value thePath(response.element(3));
            yarp::os::Value theRequestsDescription(response.element(4));
            
            if (theCanonicalName.isString() && theDescription.isString() && theKind.isString() &&
                thePath.isString() && theRequestsDescription.isString())
            {
                result = _service.addServiceRecord(channelName, theCanonicalName.toString(),
                                                   theDescription.toString(), thePath.toString(),
                                                   theRequestsDescription.toString());
                if (! result)
                {
                    // We need to remove any values that we've recorded for this channel!
                    _service.removeServiceRecord(channelName);
                }
            }
            else
            {
                OD_LOG("! (theCanonicalName.isString() && theDescription.isString() && " //####
                       "theKind.isString() && thePath.isString() && " //####
                       "theRequestsDescription.isString()"); //####
                result = false;
            }
        }
        else
        {
            OD_LOG("! (MpM_EXPECTED_NAME_RESPONSE_SIZE == response.count())"); //####
            OD_LOG_S1s("response = ", response.asString()); //####
            // Wrong number of values in the response.
            result = false;
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // PingRequestHandler::processNameResponse

bool PingRequestHandler::processRequest(const yarp::os::ConstString & request,
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
                    
                    if (Common::Endpoint::CheckEndpointName(argAsString))
                    {
                        _service.reportStatusChange(argAsString,
                                                    RegistryService::kRegistryPingFromService);
                        if (_service.checkForExistingService(argAsString))
                        {
                            // This service is already known, so just update the last-checked time.
                            _service.updateCheckedTimeForChannel(argAsString);
                        }
                        else if (_service.checkForExistingService(argAsString))
                        {
                            // Second try - something happened with the first call.
                            // This service is already known, so just update the last-checked time.
                            _service.updateCheckedTimeForChannel(argAsString);
                        }
                        else
                        {
                            // Send a 'list' request to the channel
                            yarp::os::ConstString   aName =
                                                Common::GetRandomChannelName(HIDDEN_CHANNEL_PREFIX
                                                                             "ping_/"
                                                                             DEFAULT_CHANNEL_ROOT);
                            Common::ClientChannel * outChannel = new Common::ClientChannel;
                            
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
                                            if (processNameResponse(argAsString, response))
                                            {
                                                yarp::os::Bottle message2(MpM_LIST_REQUEST);
                                                
                                                if (outChannel->write(message2, response))
                                                {
                                                    if (processListResponse(argAsString, response))
                                                    {
                                                        // Remember the response
                                                        reply.addString(MpM_OK_RESPONSE);
                                                _service.updateCheckedTimeForChannel(argAsString);
                                                    }
                                                    else
                                                    {
                                                        OD_LOG("! (processListResponse(" //####
                                                               "argAsString, response))"); //####
                                                        reply.addString(MpM_FAILED_RESPONSE);
                                                        reply.addString("Invalid response to "
                                                                        "'list' request");
                                                    }
                                                }
                                                else
                                                {
                                                    OD_LOG("! (outChannel->write(message2, " //####
                                                           "response))"); //####
                                                    reply.addString(MpM_FAILED_RESPONSE);
                                                    reply.addString("Could not write to channel");
#if defined(MpM_StallOnSendProblem)
                                                    Common::Stall();
#endif // defined(MpM_StallOnSendProblem)
                                                }
                                            }
                                            else
                                            {
                                                OD_LOG("! (processNameResponse(argAsString, " //####
                                                       "response))"); //####
                                                reply.addString(MpM_FAILED_RESPONSE);
                                                reply.addString("Invalid response to 'name' "
                                                                "request");
                                            }
                                        }
                                        else
                                        {
                                            OD_LOG("! (outChannel->write(message1, " //####
                                                   "response))"); //####
                                            reply.addString(MpM_FAILED_RESPONSE);
                                            reply.addString("Could not write to channel");
#if defined(MpM_StallOnSendProblem)
                                            Common::Stall();
#endif // defined(MpM_StallOnSendProblem)
                                        }
#if defined(MpM_DoExplicitDisconnect)
                                    if (! Common::NetworkDisconnectWithRetries(outChannel->name(),
                                                                               argAsString,
                                                                               STANDARD_WAIT_TIME,
                                                                               NULL, NULL))
                                        {
                                            OD_LOG("(! Common::NetworkDisconnectWithRetries(" //####
                                                   "outChannel->name(), argAsString, " //####
                                                   "STANDARD_WAIT_TIME, NULL, NULL))"); //####
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
                                Common::ClientChannel::RelinquishChannel(outChannel);
                            }
                            else
                            {
                                OD_LOG("! (outChannel)");
                            }
                        }
                    }
                    else
                    {
                        OD_LOG("! (Common::Endpoint::CheckEndpointName(argAsString))"); //####
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
                Common::Stall();
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
} // PingRequestHandler::processRequest

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
