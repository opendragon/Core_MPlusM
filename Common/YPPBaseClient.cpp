//--------------------------------------------------------------------------------------
//
//  File:       YPPBaseClient.cpp
//
//  Project:    YarpPlusPlus
//
//  Contains:   The class definition for the minimal functionality required for a Yarp++
//              client.
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
//  Created:    2014-02-06
//
//--------------------------------------------------------------------------------------

#include "YPPBaseClient.h"
//#define ENABLE_OD_SYSLOG /* */
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
static yarp::os::Bottle ValidateMatchResponse(const yarp::os::Bottle & response)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("response = ", response.toString().c_str());//####
    yarp::os::Bottle result;
    
    if (YPP_EXPECTED_MATCH_RESPONSE_SIZE == response.size())
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
} // ValidateMatchResponse

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

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
    if (YPP_EXPECTED_MATCH_RESPONSE_SIZE == candidates.size())
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
                    OD_SYSLOG_S1("_servicePort <- ", _servicePort.c_str());
                    result = true;
                }
            }
        }
    }
    if (! result)
    {
        _servicePort = "";
    }
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // BaseClient::connect

bool BaseClient::send(const char *             request,
                      const yarp::os::Bottle & parameters,
                      yarp::os::Port *         usingPort,
                      ServiceResponse *        response)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S2("request = ", request, "parameters = ", parameters.toString().c_str());//####
    OD_SYSLOG_P1("response = ", response);//####
    bool result;

    if (0 < _servicePort.length())
    {
        ServiceRequest  actualRequest(request, parameters);
        
        result = actualRequest.send(_servicePort, usingPort, response);
    }
    else
    {
        result = false;
    }
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
        result = ValidateMatchResponse(response.values());
    }
    OD_SYSLOG_EXIT();//####
    return result;
} // YarpPlusPlus::FindMatchingServices
