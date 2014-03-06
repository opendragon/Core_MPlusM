//
//  YPPUnregisterRequestHandler.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-03-03.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPUnregisterRequestHandler.h"
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
#define UNREGISTER_REQUEST_VERSION_NUMBER "1.0"

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and destructors
#endif // defined(__APPLE__)

UnregisterRequestHandler::UnregisterRequestHandler(RegistryService & service) :
        inherited(YPP_UNREGISTER_REQUEST), _service(service)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P1("service = ", &service);//####
    OD_SYSLOG_EXIT_P(this);//####
} // UnregisterRequestHandler::UnregisterRequestHandler

UnregisterRequestHandler::~UnregisterRequestHandler(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // UnregisterRequestHandler::~UnregisterRequestHandler

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

void UnregisterRequestHandler::fillInDescription(yarp::os::Property & info)
{
    OD_SYSLOG_ENTER();//####
    info.put(YPP_REQREP_DICT_REQUEST_KEY, YPP_UNREGISTER_REQUEST);
    info.put(YPP_REQREP_DICT_INPUT_KEY, YPP_REQREP_STRING);
    info.put(YPP_REQREP_DICT_OUTPUT_KEY, YPP_REQREP_STRING);
    info.put(YPP_REQREP_DICT_VERSION_KEY, UNREGISTER_REQUEST_VERSION_NUMBER);
    info.put(YPP_REQREP_DICT_DESCRIPTION_KEY, "Unregister the service and its requests");
    yarp::os::Value    keywords;
    yarp::os::Bottle * asList = keywords.asList();
    
    asList->addString(YPP_UNREGISTER_REQUEST);
    asList->addString("remove");
    info.put(YPP_REQREP_DICT_KEYWORDS_KEY, keywords);
    OD_SYSLOG_EXIT();//####
} // UnregisterRequestHandler::fillInDescription

bool UnregisterRequestHandler::operator() (const yarp::os::Bottle &     restOfInput,
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
                    // Forget the information associated with the port name
                    if (_service.removeServiceRecord(argAsString))
                    {
                        reply.addString(YPP_OK_RESPONSE);
                    }
                    else
                    {
                        reply.addString(YPP_FAILED_RESPONSE);
                        reply.addString("Could not remove service");
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
} // UnregisterRequestHandler::operator()

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
