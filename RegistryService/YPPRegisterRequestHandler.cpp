//--------------------------------------------------------------------------------------
//
//  File:       YPPRegisterRequestHandler.cpp
//
//  Project:    YarpPlusPlus
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

#include "YPPRegisterRequestHandler.h"
//#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPEndpoint.h"
#include "YPPRegistryService.h"
#include "YPPRequests.h"
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

using namespace YarpPlusPlus;

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
        inherited(YPP_REGISTER_REQUEST), _service(service)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P1("service = ", &service);//####
    OD_SYSLOG_EXIT_P(this);//####
} // RegisterRequestHandler::RegisterRequestHandler

RegisterRequestHandler::~RegisterRequestHandler(void)
{
    OD_SYSLOG_OBJENTER();//####
    OD_SYSLOG_OBJEXIT();//####
} // RegisterRequestHandler::~RegisterRequestHandler

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

void RegisterRequestHandler::fillInDescription(yarp::os::Property & info)
{
    OD_SYSLOG_OBJENTER();//####
    try
    {
        info.put(YPP_REQREP_DICT_REQUEST_KEY, YPP_REGISTER_REQUEST);
        info.put(YPP_REQREP_DICT_INPUT_KEY, YPP_REQREP_STRING);
        info.put(YPP_REQREP_DICT_OUTPUT_KEY, YPP_REQREP_STRING);
        info.put(YPP_REQREP_DICT_VERSION_KEY, REGISTER_REQUEST_VERSION_NUMBER);
        info.put(YPP_REQREP_DICT_DETAILS_KEY, "Register the service and its requests");
        yarp::os::Value    keywords;
        yarp::os::Bottle * asList = keywords.asList();
        
        asList->addString(YPP_REGISTER_REQUEST);
        asList->addString("add");
        info.put(YPP_REQREP_DICT_KEYWORDS_KEY, keywords);
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_OBJEXIT();//####
} // RegisterRequestHandler::fillInDescription

bool RegisterRequestHandler::operator() (const yarp::os::Bottle &      restOfInput,
                                         const yarp::os::ConstString & senderPort,
                                         yarp::os::ConnectionWriter *  replyMechanism)
{
#if (! defined(ENABLE_OD_SYSLOG))
# pragma unused(senderPort)
#endif // ! defined(ENABLE_OD_SYSLOG)
    OD_SYSLOG_OBJENTER();//####
    OD_SYSLOG_S2("restOfInput = ", restOfInput.toString().c_str(), "senderPort = ", senderPort.c_str());//####
    OD_SYSLOG_P1("replyMechanism = ", replyMechanism);//####
    bool result = true;
    
    try
    {
        if (replyMechanism)
        {
            yarp::os::Bottle reply;
            
            // Validate the name as a port name
            if (1 == restOfInput.size())
            {
                yarp::os::Value argument(restOfInput.get(0));
                
                if (argument.isString())
                {
                    yarp::os::ConstString argAsString(argument.toString());
                    
                    if (Endpoint::CheckEndpointName(argAsString))
                    {
                        // Send a 'list' request to the port
                        yarp::os::ConstString aName(GetRandomPortName("register/port_"));
                        yarp::os::Port *      outPort = new yarp::os::Port();
                        
                        if (outPort)
                        {
                            if (outPort->open(aName))
                            {
                                if (outPort->addOutput(argAsString))
                                {
                                    yarp::os::Bottle message1(YPP_NAME_REQUEST);
                                    yarp::os::Bottle response;
                                    
                                    if (outPort->write(message1, response))
                                    {
                                        if (processNameResponse(argAsString, response))
                                        {
                                            yarp::os::Bottle message2(YPP_LIST_REQUEST);
                                            
                                            if (outPort->write(message2, response))
                                            {
                                                if (processListResponse(argAsString, response))
                                                {
                                                    // Remember the response
                                                    reply.addString(YPP_OK_RESPONSE);
                                                }
                                                else
                                                {
                                                    OD_SYSLOG("! (processListResponse(argAsString, response))");//####
                                                    reply.addString(YPP_FAILED_RESPONSE);
                                                    reply.addString("Invalid response to 'list' request");
                                                }
                                            }
                                            else
                                            {
                                                OD_SYSLOG("! (outPort->write(message2, response))");//####
                                                reply.addString(YPP_FAILED_RESPONSE);
                                                reply.addString("Could not write to port");
                                            }
                                        }
                                        else
                                        {
                                            OD_SYSLOG("! (processNameResponse(argAsString, response))");//####
                                            reply.addString(YPP_FAILED_RESPONSE);
                                            reply.addString("Invalid response to 'name' request");
                                        }
                                    }
                                    else
                                    {
                                        OD_SYSLOG("! (outPort->write(message1, response))");//####
                                        reply.addString(YPP_FAILED_RESPONSE);
                                        reply.addString("Could not write to port");
                                    }
                                }
                                else
                                {
                                    OD_SYSLOG("! (outPort->addOutput(argAsString))");//####
                                    reply.addString(YPP_FAILED_RESPONSE);
                                    reply.addString("Could not connect to port");
                                }
                                OD_SYSLOG_S1("about to close, port = ", aName.c_str());//####
                                outPort->close();
                                OD_SYSLOG("close completed.");//####
                            }
                            else
                            {
                                OD_SYSLOG("! (outPort->open(aName))");//####
                                reply.addString(YPP_FAILED_RESPONSE);
                                reply.addString("Port could not be opened");
                            }
                            delete outPort;
                        }
                        else
                        {
                            OD_SYSLOG("! (outPort)");
                        }
                    }
                    else
                    {
                        OD_SYSLOG("! (Endpoint::CheckEndpointName(argAsString))");//####
                        reply.addString(YPP_FAILED_RESPONSE);
                        reply.addString("Invalid port name");
                    }
                }
                else
                {
                    OD_SYSLOG("! (argument.isString())");//####
                    reply.addString(YPP_FAILED_RESPONSE);
                    reply.addString("Invalid port name");
                }
            }
            else
            {
                OD_SYSLOG("! (1 == restOfInput.size())");//####
                reply.addString(YPP_FAILED_RESPONSE);
                reply.addString("Missing port name or extra arguments to request");
            }
            OD_SYSLOG_S1("reply <- ", reply.toString().c_str());
            reply.write(*replyMechanism);
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_OBJEXIT_B(result);//####
    return result;
} // RegisterRequestHandler::operator()

bool RegisterRequestHandler::processListResponse(const yarp::os::ConstString & portName,
                                                 const ServiceResponse &       response)
{
    OD_SYSLOG_OBJENTER();//####
    OD_SYSLOG_S2("portName = ", portName.c_str(), "response = ", response.asString().c_str());//####
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
                    
                    if (asDict->check(YPP_REQREP_DICT_REQUEST_KEY))
                    {
                        yarp::os::ConstString theRequest(asDict->find(YPP_REQREP_DICT_REQUEST_KEY).asString());
                        yarp::os::Bottle      keywordList;
                        RequestDescription    requestDescriptor;
                        
                        OD_SYSLOG_S1("theRequest <- ", theRequest.c_str());//####
                        if (asDict->check(YPP_REQREP_DICT_DETAILS_KEY))
                        {
                            yarp::os::Value theDetails = asDict->find(YPP_REQREP_DICT_DETAILS_KEY);
                            
                            OD_SYSLOG_S1("theDetails <- ", theDetails.toString().c_str());//####
                            if (theDetails.isString())
                            {
                                requestDescriptor._details = theDetails.toString();
                            }
                            else
                            {
                                OD_SYSLOG("! (theDetails.isString())");//####
                                // The details field is present, but it's not a string.
                                result = false;
                            }
                        }
                        if (asDict->check(YPP_REQREP_DICT_INPUT_KEY))
                        {
                            yarp::os::Value theInputs = asDict->find(YPP_REQREP_DICT_INPUT_KEY);
                            
                            OD_SYSLOG_S1("theInputs <- ", theInputs.toString().c_str());//####
                            if (theInputs.isString())
                            {
                                requestDescriptor._inputs = theInputs.toString();
                            }
                            else
                            {
                                OD_SYSLOG("! (theInputs.isString())");//####
                                // The inputs descriptor is present, but it's not a string
                                result = false;
                            }
                        }
                        if (asDict->check(YPP_REQREP_DICT_KEYWORDS_KEY))
                        {
                            yarp::os::Value theKeywords = asDict->find(YPP_REQREP_DICT_KEYWORDS_KEY);
                            
                            OD_SYSLOG_S1("theKeywords <- ", theKeywords.toString().c_str());//####
                            if (theKeywords.isList())
                            {
                                keywordList = *theKeywords.asList();
                            }
                            else
                            {
                                OD_SYSLOG("! (theKeywords.isList())");//####
                                // The keywords entry is present, but it's not a list
                                result = false;
                            }
                        }
                        if (asDict->check(YPP_REQREP_DICT_OUTPUT_KEY))
                        {
                            yarp::os::Value theOutputs = asDict->find(YPP_REQREP_DICT_OUTPUT_KEY);
                            
                            OD_SYSLOG_S1("theOutputs <- ", theOutputs.toString().c_str());//####
                            if (theOutputs.isString())
                            {
                                requestDescriptor._outputs = theOutputs.toString();
                            }
                            else
                            {
                                OD_SYSLOG("! (theOutputs.isString())");//####
                                // The outputs descriptor is present, but it's not a string
                                result = false;
                            }
                        }
                        if (asDict->check(YPP_REQREP_DICT_VERSION_KEY))
                        {
                            yarp::os::Value theVersion = asDict->find(YPP_REQREP_DICT_VERSION_KEY);
                            
                            OD_SYSLOG_S1("theVersion <- ", theVersion.toString().c_str());//####
                            if (theVersion.isString() || theVersion.isInt() || theVersion.isDouble())
                            {
                                requestDescriptor._version = theVersion.toString();
                            }
                            else
                            {
                                OD_SYSLOG("! (theVersion.isString() || theVersion.isInt() || "//####
                                          "theVersion.isDouble())");//####
                                // The version entry is present, but it's not a simple value
                                result = false;
                            }
                        }
                        if (result)
                        {
                            requestDescriptor._port = portName;
                            requestDescriptor._request = theRequest;
                            result = _service.addRequestRecord(keywordList, requestDescriptor);
                            OD_SYSLOG_B1("result <- ", result);//####
                        }
                    }
                    else
                    {
                        OD_SYSLOG("! (asDict->check(YPP_REQREP_DICT_REQUEST_KEY))");//####
                        // There is no 'name' entry in this dictionary
                        result = false;
                    }
                }
                else
                {
                    OD_SYSLOG("! (anElement.isDict())");//####
                    // One of the values is not a dictionary
                    result = false;
                }
            }
        }
        else
        {
            OD_SYSLOG("! (0 < count)");//####
            // Wrong number of values in the response.
            result = false;
        }
        if (! result)
        {
            // We need to remove any values that we've recorded for this port!
            _service.removeServiceRecord(portName);
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_OBJEXIT_B(result);//####
    return result;
} // RegisterRequestHandler::processListResponse

bool RegisterRequestHandler::processNameResponse(const yarp::os::ConstString & portName,
                                                 const ServiceResponse &       response)
{
    OD_SYSLOG_OBJENTER();//####
    OD_SYSLOG_S2("portName = ", portName.c_str(), "response = ", response.asString().c_str());//####
    bool result = false;
    
    try
    {
        if (2 == response.count())
        {
            yarp::os::Value theCanonicalName(response.element(0));
            yarp::os::Value theDescription(response.element(1));
            
            if (theCanonicalName.isString() && theDescription.isString())
            {
                result = _service.addServiceRecord(portName, theCanonicalName.toString(), theDescription.toString());
            }
            else
            {
                OD_SYSLOG("! (theCanonicalName.isString() && theDescription.isString())");//####
                // The canonical name and description are present, but at least one of them is not a string
                result = false;
            }
        }
        else
        {
            OD_SYSLOG("! (2 == response.count())");//####
            // Wrong number of values in the response.
            result = false;
        }
        if (! result)
        {
            // We need to remove any values that we've recorded for this port!
            _service.removeServiceRecord(portName);
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_OBJEXIT_B(result);//####
    return result;
} // RegisterRequestHandler::processNameResponse

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
