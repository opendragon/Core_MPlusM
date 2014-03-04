//
//  YPPRegisterRequestHandler.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-03-03.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPRegisterRequestHandler.h"
#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPEndpoint.h"
#include "YPPRegistryService.h"
#include "YPPRequests.h"

using namespace YarpPlusPlus;

#if defined(__APPLE__)
# pragma mark Private structures and constants
#endif // defined(__APPLE__)

/*! @brief The protocol version number for the 'list' request. */
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
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // RegisterRequestHandler::~RegisterRequestHandler

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

void RegisterRequestHandler::fillInDescription(yarp::os::Property & info)
{
    OD_SYSLOG_ENTER();//####
    info.put(YPP_REQREP_DICT_NAME_KEY, YPP_REGISTER_REQUEST);
    info.put(YPP_REQREP_DICT_INPUT_KEY, YPP_REQREP_STRING);
    info.put(YPP_REQREP_DICT_OUTPUT_KEY, YPP_REQREP_STRING);
    info.put(YPP_REQREP_DICT_VERSION_KEY, REGISTER_REQUEST_VERSION_NUMBER);
    info.put(YPP_REQREP_DICT_DESCRIPTION_KEY, "Register the service and its requests");
    yarp::os::Value    keywords;
    yarp::os::Bottle * asList = keywords.asList();
    
    asList->addString(YPP_REGISTER_REQUEST);
    asList->addString("add");
    info.put(YPP_REQREP_DICT_KEYWORDS_KEY, keywords);
    OD_SYSLOG_EXIT();//####
} // RegisterRequestHandler::fillInDescription

bool RegisterRequestHandler::operator() (const yarp::os::Bottle &     restOfInput,
                                         yarp::os::ConnectionWriter * replyMechanism)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("restOfInput = ", restOfInput.toString().c_str());//####
    bool result = true;
    
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
                
                if (Endpoint::checkEndpointName(argAsString))
                {
                    // Send a 'list' request to the port
                    yarp::os::Port        outPort;
                    yarp::os::ConstString aName = Endpoint::getRandomPortName();

                    if (outPort.open(aName))
                    {
                        if (outPort.addOutput(argAsString))
                        {
                            yarp::os::Bottle message(YPP_LIST_REQUEST);
                            yarp::os::Bottle response;
                            
                            if (outPort.write(message, response))
                            {
                                if (processListResponse(argAsString, response))
                                {
                                    // Remember the response
                                    reply.addString(YPP_OK_RESPONSE);
                                }
                                else
                                {
                                    reply.addString(YPP_FAILED_RESPONSE);
                                    reply.addString("Invalid response to 'list' request");
                                }
                            }
                            else
                            {
                                reply.addString(YPP_FAILED_RESPONSE);
                                reply.addString("Could not write to port");
                            }
                        }
                        else
                        {
                            reply.addString(YPP_FAILED_RESPONSE);
                            reply.addString("Could not connect to port");
                        }
                        outPort.close();
                    }
                    else
                    {
                        reply.addString(YPP_FAILED_RESPONSE);
                        reply.addString("Port could not be opened");
                    }
                }
                else
                {
                    reply.addString(YPP_FAILED_RESPONSE);
                    reply.addString("Invalid port name");
                }
            }
            else
            {
                reply.addString(YPP_FAILED_RESPONSE);
                reply.addString("Invalid port name");
            }
        }
        else
        {
            reply.addString(YPP_FAILED_RESPONSE);
            reply.addString("Missing port name or extra arguments to request");
        }
        OD_SYSLOG_S1("reply <- ", reply.toString().c_str());
        reply.write(*replyMechanism);
    }
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // RegisterRequestHandler::operator()

bool RegisterRequestHandler::processListResponse(const yarp::os::ConstString & portName,
                                                 const ServiceResponse &       response)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S2("portName = ", portName.c_str(), "response = ", response.asString().c_str());//####
    bool result;
    
    if (0 < response.count())
    {
        result = true;
        for (int ii = 0; result && (ii < response.count()); ++ii)
        {
            yarp::os::Value anElement(response.element(ii));
            
            if (anElement.isDict())
            {
                yarp::os::Property * asDict = anElement.asDict();
                
                if (asDict->check(YPP_REQREP_DICT_NAME_KEY))
                {
                    yarp::os::ConstString theName = asDict->find(YPP_REQREP_DICT_NAME_KEY).asString();

                    OD_SYSLOG_S1("theName <- ", theName.c_str());//####
                    if (asDict->check(YPP_REQREP_DICT_DESCRIPTION_KEY))
                    {
                        yarp::os::Value theDescription = asDict->find(YPP_REQREP_DICT_DESCRIPTION_KEY);
                        
                        OD_SYSLOG_S1("theDescription <- ", theDescription.toString().c_str());//####
                        if (theDescription.isString())
                        {
                            //TBD!!
                        }
                        else
                        {
                            // The description is present, but it's not a string.
                            result = false;
                        }
                    }
                    if (asDict->check(YPP_REQREP_DICT_INPUT_KEY))
                    {
                        yarp::os::Value theInputs = asDict->find(YPP_REQREP_DICT_INPUT_KEY);
                        
                        OD_SYSLOG_S1("theInputs <- ", theInputs.toString().c_str());//####
                        if (theInputs.isString())
                        {
                            //TBD!!
                        }
                        else
                        {
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
                            //TBD!!
                        }
                        else
                        {
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
                            //TBD!!
                        }
                        else
                        {
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
                            //TBD!!
                        }
                        else
                        {
                            // The version entry is present, but it's not a simple value
                            result = false;
                        }
                    }
                    if (result)
                    {
                        
                    }
                }
                else
                {
                    // There is no 'name' entry in this dictionary
                    result = false;
                }
            }
            else
            {
                // One of the values is not a dictionary
                result = false;
            }
        }
    }
    else
    {
        // Wrong number of values in the response.
        result = false;
    }
    if (! result)
    {
        // We need to remove any values that we've recorded for this port!
        //TBD!!
    }
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // RegisterRequestHandler::processListResponse

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
