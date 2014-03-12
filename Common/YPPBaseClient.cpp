//
//  YPPBaseClient.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-06.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPBaseClient.h"
#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPRequests.h"
#include "YPPServiceRequest.h"
#include <cstring>

using namespace YarpPlusPlus;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

/*! @brief Check the response to the 'match' request for validity.
 @param response The response to be checked.
 @returns The original response, if it is valid, or an empty response if it is not. */
static yarp::os::Bottle ValidateMatchRequest(const yarp::os::Bottle & response)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("response = ", response.toString().c_str());//####
    yarp::os::Bottle result;
    
    if (BaseClient::kExpectedResponseSize == response.size())
    {
        // The first element of the response should be 'OK' or 'FAILED'; if 'OK', the second element should be a list of
        // service names.
        yarp::os::Value responseFirst(response.get(0));
        
        if (responseFirst.isString())
        {
            yarp::os::ConstString responseFirstAsString(responseFirst.toString());
            
            if (! strcmp(YPP_OK_RESPONSE, responseFirstAsString.c_str()))
            {
                // Now, check the second element.
                yarp::os::Value responseSecond(response.get(1));
                
                if (responseSecond.isList())
                {
                    result = response;
                }
            }
            else if (! strcmp(YPP_FAILED_RESPONSE, responseFirstAsString.c_str()))
            {
                result = response;
            }
        }
    }
    OD_SYSLOG_EXIT();//####
    return result;
} // ValidateMatchRequest

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

const int BaseClient::kExpectedResponseSize = 2;

#if defined(__APPLE__)
# pragma mark Constructors and destructors
#endif // defined(__APPLE__)

BaseClient::BaseClient(void) :
    _servicePort()
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT_P(this);//####
} // BaseClient::BaseClient

BaseClient::~BaseClient(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // BaseClient::~BaseClient

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

bool BaseClient::connect(const char * criteria,
                         const bool   allowOnlyOneMatch)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("criteria = ", criteria);//####
    OD_SYSLOG_B1("allowOnlyOneMatch = ", allowOnlyOneMatch);//####
    bool             result = false;
    yarp::os::Bottle candidates(FindMatchingServices(criteria));
    
    OD_SYSLOG_S1("candidates <- ", candidates.toString().c_str());//####
    if (BaseClient::kExpectedResponseSize == candidates.size())
    {
        // First, check if the search succeeded.
        yarp::os::ConstString candidatesFirstString(candidates.get(0).toString());
        
        if (! strcmp(YPP_OK_RESPONSE, candidatesFirstString.c_str()))
        {
            // Now, process the second element.
            yarp::os::Bottle * candidateList = candidates.get(1).asList();
            
            if (candidateList)
            {
                // Now, set up the service port.
                int candidateCount = candidateList->size();
                
                if ((! allowOnlyOneMatch) || (1 == candidateCount))
                {
                    _servicePort = candidateList->get(0).toString();
                    result = true;
                }
                
            }
        }
    }
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // BaseClient::connect

bool BaseClient::send(const yarp::os::Bottle & request,
                      ServiceResponse *        response)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("request = ", request.toString().c_str());//####
    OD_SYSLOG_P1("response = ", response);//####
    bool result = false;

    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // BaseClient::send












#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

/*! @brief Find one or more matching local services that are registered with a running Service Registry service.
 @param criteria The matching conditions.
 @returns A (possibly empty) list of matching services, preceded by the request status. */
yarp::os::Bottle YarpPlusPlus::FindMatchingServices(const char * criteria)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("criteria = ", criteria);//####
    yarp::os::Bottle parameters;
    
    parameters.addString(criteria); // Note that we can't simply initialize the Bottle with the criteria, as it will be
                                    // parsed by YARP.
    yarp::os::Bottle result;
    ServiceRequest   request(YPP_MATCH_REQUEST, parameters);
    ServiceResponse  response;
    
    if (request.send(YPP_SERVICE_REGISTRY_PORT_NAME, NULL, &response))
    {
        result = ValidateMatchRequest(response.values());
    }
    OD_SYSLOG_EXIT();//####
    return result;
} // YarpPlusPlus::FindMatchingServices
