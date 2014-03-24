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
//#define OD_ENABLE_LOGGING /* */
#include "ODLogging.h"
#include "YPPRequests.h"
#include "YPPServiceRequest.h"
#include <cstring>
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
#include <yarp/os/Network.h>
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

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
static yarp::os::Bottle validateMatchResponse(const yarp::os::Bottle & response)
{
    OD_LOG_ENTER();//####
    OD_LOG_S1("response = ", response.toString().c_str());//####
    yarp::os::Bottle result;
    
    try
    {
        if (YPP_EXPECTED_MATCH_RESPONSE_SIZE == response.size())
        {
            // The first element of the response should be 'OK' or 'FAILED'; if 'OK', the second element should be a
            // list of service names.
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
                    else
                    {
                        OD_LOG("! (responseSecond.isList())");//####
                    }
                }
                else if (! strcmp(YPP_FAILED_RESPONSE, responseFirstAsString.c_str()))
                {
                    result = response;
                }
                else
                {
                    OD_LOG("! (! strcmp(YPP_FAILED_RESPONSE, responseFirstAsString.c_str()))");//####
                }
            }
            else
            {
                OD_LOG("! (responseFirst.isString())");//####
            }
        }
        else
        {
            OD_LOG("! (YPP_EXPECTED_MATCH_RESPONSE_SIZE == response.size())");//####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_EXIT();//####
    return result;
} // validateMatchResponse

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and destructors
#endif // defined(__APPLE__)

BaseClient::BaseClient(const char * basePortName) :
        _clientPort(NULL), _clientPortName(), _servicePortName(), _basePortName(NULL), _connected(false)
{
    OD_LOG_ENTER();//####
    size_t len = strlen(basePortName);
    
    _basePortName = new char[len + 1];
    memcpy(_basePortName, basePortName, len + 1);
    OD_LOG_EXIT_P(this);//####
} // BaseClient::BaseClient

BaseClient::~BaseClient(void)
{
    OD_LOG_OBJENTER();//####
    disconnectFromService();
    if (_clientPort)
    {
        delete _clientPort;
    }
    delete _basePortName;
    OD_LOG_OBJEXIT();//####
} // BaseClient::~BaseClient

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

bool BaseClient::connectToService(void)
{
    OD_LOG_OBJENTER();//####
    if (! _connected)
    {
        if (! _clientPort)
        {
            _clientPortName = GetRandomPortName(_basePortName);
            _clientPort = new yarp::os::Port;
        }
        if (_clientPort)
        {
            if (_clientPort->open(_clientPortName))
            {
                if (yarp::os::Network::connect(_clientPortName, _servicePortName))
                {
                    _connected = true;
                }
                else
                {
                    OD_LOG("! (yarp::os::Network::connect(_clientPortName, _servicePortName))");//####
                }
            }
            else
            {
                OD_LOG("! (_clientPort->open(aName))");//####
            }
        }
        else
        {
            OD_LOG("! (_clientPort)");//####
        }
    }
    OD_LOG_OBJEXIT_B(_connected);//####
    return _connected;
} // BaseClient::connectToService

bool BaseClient::disconnectFromService(void)
{
    OD_LOG_OBJENTER();//####
    if (_connected)
    {
        if (yarp::os::Network::disconnect(_clientPortName, _servicePortName))
        {
            _connected = false;
        }
        else
        {
            OD_LOG("! (yarp::os::Network::disconnect(_clientPortName, _servicePortName))");//####
        }
    }
    OD_LOG_OBJEXIT_B(! _connected);//####
    return (! _connected);
} // BaseClient::disconnectFromService

bool BaseClient::findService(const char * criteria,
                             const bool   allowOnlyOneMatch)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_S1("criteria = ", criteria);//####
    OD_LOG_B1("allowOnlyOneMatch = ", allowOnlyOneMatch);//####
    bool result = false;
    
    try
    {
        yarp::os::Bottle candidates(FindMatchingServices(criteria));

        OD_LOG_S1("candidates <- ", candidates.toString().c_str());//####
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
                        _servicePortName = candidateList->get(0).toString();
                        OD_LOG_S1("_servicePortName <- ", _servicePortName.c_str());
                        result = true;
                    }
                    else
                    {
                        OD_LOG("! ((! allowOnlyOneMatch) || (1 == candidateCount))");//####
                    }
                }
                else
                {
                    OD_LOG("! (candidateList)");//####
                }
            }
            else
            {
                OD_LOG("! (! strcmp(YPP_OK_RESPONSE, candidatesFirstString.c_str()))");//####
            }
        }
        else
        {
            OD_LOG("! (YPP_EXPECTED_MATCH_RESPONSE_SIZE == candidates.size())");//####
        }
        if (! result)
        {
            _servicePortName = "";
            OD_LOG_S1("_servicePortName <- ", _servicePortName.c_str());
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT_B(result);//####
    return result;
} // BaseClient::findService

bool BaseClient::send(const char *             request,
                      const yarp::os::Bottle & parameters,
                      ServiceResponse *        response)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_S2("request = ", request, "parameters = ", parameters.toString().c_str());//####
    OD_LOG_P1("response = ", response);//####
    bool result = false;

    try
    {
        if (_connected)
        {
            if (0 < _servicePortName.length())
            {
                ServiceRequest actualRequest(request, parameters);
                
                result = actualRequest.send(*_clientPort, response);
            }
            else
            {
                OD_LOG("! (0 < _servicePortName.length())");//####
            }
        }
        else
        {
            OD_LOG("! (_connected)");//####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT_B(result);//####
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
    OD_LOG_ENTER();//####
    OD_LOG_S1("criteria = ", criteria);//####
    yarp::os::Bottle result;

    try
    {
        yarp::os::ConstString aName(GetRandomPortName("/findmatch/port_"));
        yarp::os::Port *      newPort = new yarp::os::Port;
        
        if (newPort)
        {
            if (newPort->open(aName))
            {
                if (yarp::os::Network::connect(aName, YPP_SERVICE_REGISTRY_PORT_NAME))
                {
                    yarp::os::Bottle parameters;
                    
                    parameters.addString(criteria); // Note that we can't simply initialize the Bottle with the
                                                    // criteria, as it will be parsed by YARP.
                    ServiceRequest  request(YPP_MATCH_REQUEST, parameters);
                    ServiceResponse response;
                    
                    if (request.send(*newPort, &response))
                    {
                        OD_LOG_S1("response <- ", response.asString().c_str());//####
                        result = validateMatchResponse(response.values());
                    }
                    else
                    {
                        OD_LOG("! (request.send(YPP_SERVICE_REGISTRY_PORT_NAME, *newPort, &response))");//####
                    }
                    if (! yarp::os::Network::disconnect(aName, YPP_SERVICE_REGISTRY_PORT_NAME))
                    {
                        OD_LOG("(! yarp::os::Network::disconnect(aName, YPP_SERVICE_REGISTRY_PORT_NAME))");//####
                    }
                }
                else
                {
                    OD_LOG("! (yarp::os::Network::connect(aName, YPP_SERVICE_REGISTRY_PORT_NAME))");//####
                }
                newPort->close();
            }
            delete newPort;
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_EXIT();//####
    return result;
} // YarpPlusPlus::FindMatchingServices
