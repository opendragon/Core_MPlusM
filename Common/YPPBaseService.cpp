//--------------------------------------------------------------------------------------
//
//  File:       YPPBaseService.cpp
//
//  Project:    YarpPlusPlus
//
//  Contains:   The class definition for the minimal functionality required for a Yarp++
//              service.
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

#include "YPPBaseService.h"
//#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPBaseContext.h"
#include "YPPBaseServiceInputHandler.h"
#include "YPPBaseServiceInputHandlerCreator.h"
#include "YPPEndpoint.h"
#include "YPPException.h"
#include "YPPInfoRequestHandler.h"
#include "YPPListRequestHandler.h"
#include "YPPNameRequestHandler.h"
#include "YPPRequests.h"
#include "YPPServiceRequest.h"

using namespace YarpPlusPlus;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and destructors
#endif // defined(__APPLE__)

BaseService::BaseService(const bool                    useMultipleHandlers,
                         const yarp::os::ConstString & canonicalName,
                         const yarp::os::ConstString & description,
                         const yarp::os::ConstString & serviceEndpointName,
                         const yarp::os::ConstString & serviceHostName,
                         const yarp::os::ConstString & servicePortNumber) :
        _requestHandlers(*this), _contexts(), _canonicalName(canonicalName), _description(description),
        _endpoint(NULL), _handler(NULL), _handlerCreator(NULL), _started(false),
        _useMultipleHandlers(useMultipleHandlers)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_B1("useMultipleHandlers = ", useMultipleHandlers);//####
    OD_SYSLOG_S4("canonicalName = ", canonicalName.c_str(), "description = ", description.c_str(),//####
                 "serviceEndpointName = ", serviceEndpointName.c_str(), "serviceHostName = ",//####
                 serviceHostName.c_str());//####
    OD_SYSLOG_S1("servicePortNumber = ", servicePortNumber.c_str());//####
    _endpoint = new Endpoint(serviceEndpointName, serviceHostName, servicePortNumber);
    setUpRequestHandlers();
    OD_SYSLOG_EXIT_P(this);//####
} // BaseService::BaseService

BaseService::BaseService(const bool                    useMultipleHandlers,
                         const yarp::os::ConstString & canonicalName,
                         const yarp::os::ConstString & description,
                         const int                     argc,
                         char **                       argv) :
        _requestHandlers(*this), _contexts(), _canonicalName(canonicalName), _description(description),
        _endpoint(NULL), _handler(NULL), _handlerCreator(NULL), _started(false),
        _useMultipleHandlers(useMultipleHandlers)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_B1("useMultipleHandlers = ", useMultipleHandlers);//####
    OD_SYSLOG_S2("canonicalName = ", canonicalName.c_str(), "description = ", description.c_str());//####
    switch (argc)
    {
            // Argument order for tests = endpoint name [, IP address / name [, port [, carrier]]]
        case 1:
            _endpoint = new Endpoint(*argv);
            break;
            
        case 2:
            _endpoint = new Endpoint(*argv, argv[1]);
            break;
            
        case 3:
            _endpoint = new Endpoint(*argv, argv[1], argv[2]);
            break;
            
        default:
            OD_SYSLOG_EXIT_THROW_S("Invalid parameters for service endpoint");//####
            throw new Exception("Invalid parameters for service endpoint");
            
    }
    setUpRequestHandlers();
    OD_SYSLOG_EXIT_P(this);//####
} // BaseService::BaseService

BaseService::~BaseService(void)
{
    OD_SYSLOG_ENTER();//####
    delete _endpoint;
    delete _handler;
    delete _handlerCreator;
    clearContexts();
    OD_SYSLOG_EXIT();//####
} // BaseService::~BaseService

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

void BaseService::addContext(const yarp::os::ConstString & key,
                             BaseContext *                 context)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("key = ", key.c_str());//####
    OD_SYSLOG_P1("context = ", context);//####
    try
    {
        if (context)
        {
            _contexts.insert(ContextMapValue(std::string(key), context));
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT();//####
} // BaseService::addContext

void BaseService::clearContexts(void)
{
    OD_SYSLOG_ENTER();//####
    for (ContextMap::iterator walker(_contexts.begin()); _contexts.end() != walker; ++walker)
    {
        BaseContext * value = walker->second;
        
        if (value)
        {
            delete value;
        }
    }
    _contexts.clear();
    OD_SYSLOG_EXIT();//####
} // BaseService::clearContexts

BaseContext * BaseService::findContext(const yarp::os::ConstString & key)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("key = ", key.c_str());//####
    BaseContext * result = NULL;
    
    try
    {
        ContextMap::const_iterator match(_contexts.find(std::string(key)));
        
        if (_contexts.cend() != match)
        {
            result = match->second;
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT_P(result);//####
    return result;
} // BaseService::findContext

bool BaseService::processRequest(const yarp::os::ConstString & request,
                                 const yarp::os::Bottle &      restOfInput,
                                 const yarp::os::ConstString & senderPort,
                                 yarp::os::ConnectionWriter *  replyMechanism)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S3("request = ", request.c_str(), "restOfInput = ", restOfInput.toString().c_str(),//####
                 "senderPort = ", senderPort.c_str());//####
    OD_SYSLOG_P1("replyMechanism = ", replyMechanism);//####
    bool result;
    
    try
    {
        BaseRequestHandler * handler = _requestHandlers.lookupRequestHandler(request);
        
        if (handler)
        {
            OD_SYSLOG("(handler)");//####
            result = (*handler) (restOfInput, senderPort, replyMechanism);
        }
        else
        {
            OD_SYSLOG("! (handler)");//####
            if (replyMechanism)
            {
                yarp::os::Bottle errorMessage("unrecognized request");
                
                errorMessage.write(*replyMechanism);
            }
            result = false;
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // BaseService::processRequest

void BaseService::removeContext(const yarp::os::ConstString & key)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("key = ", key.c_str());//####
    try
    {
        ContextMap::iterator match(_contexts.find(std::string(key)));
        
        if (_contexts.end() != match)
        {
            BaseContext * value = match->second;
            
            if (value)
            {
                delete value;
            }
            _contexts.erase(match);
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT();//####
} // BaseService::removeContext

bool BaseService::setTimeout(const float timeout)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_D1("timeout = ", timeout);//####
    bool result = false;
    
    try
    {
        if (! _started)
        {
            result = _endpoint->setTimeout(timeout);
        }
        else
        {
            OD_SYSLOG("! (! _started)");//####
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // BaseService::setTimeout

void BaseService::setUpRequestHandlers(void)
{
    OD_SYSLOG_ENTER();//####
    try
    {
        _requestHandlers.registerRequestHandler(new InfoRequestHandler());
        _requestHandlers.registerRequestHandler(new ListRequestHandler());
        _requestHandlers.registerRequestHandler(new NameRequestHandler(*this));
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT();//####
} // BaseService::setUpRequestHandlers

bool BaseService::start(void)
{
    OD_SYSLOG_ENTER();//####
    try
    {
        if (! _started)
        {
            if (_useMultipleHandlers)
            {
                _handlerCreator = new BaseServiceInputHandlerCreator(*this);
                if (_handlerCreator)
                {
                    if (_endpoint->setInputHandlerCreator(*_handlerCreator) && _endpoint->open())
                    {
                        _started = true;
                    }
                    else
                    {
                        OD_SYSLOG("! (_endpoint->setInputHandlerCreator(*_handlerCreator) && _endpoint->open())");//####
                        delete _handlerCreator;
                        _handlerCreator = NULL;
                    }
                }
                else
                {
                    OD_SYSLOG("! (_handlerCreator)");//####
                }
            }
            else
            {
                _handler = new BaseServiceInputHandler(*this);
                if (_handler)
                {
                    if (_endpoint->setInputHandler(*_handler) && _endpoint->open())
                    {
                        _started = true;
                    }
                    else
                    {
                        OD_SYSLOG("! (_endpoint->setInputHandler(*_handler) && _endpoint->open())");//####
                        delete _handler;
                        _handler = NULL;
                    }
                }
                else
                {
                    OD_SYSLOG("! (_handler)");//####
                }
            }
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT_B(_started);//####
    return _started;
} // BaseService::start

bool BaseService::stop(void)
{
    OD_SYSLOG_ENTER();//####
    _started = false;
    OD_SYSLOG_EXIT_B(! _started);//####
    return (! _started);
} // BaseService::stop

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

bool YarpPlusPlus::RegisterLocalService(const yarp::os::ConstString & portName)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("portName = ", portName.c_str());//####
    bool result = false;
    
    try
    {
        yarp::os::Bottle parameters(portName);
        ServiceRequest   request(YPP_REGISTER_REQUEST, parameters);
        ServiceResponse  response;
        
        if (request.send(YPP_SERVICE_REGISTRY_PORT_NAME, NULL, &response))
        {
            // Check that we got a successful self-registration!
            if (1 == response.count())
            {
                yarp::os::Value theValue = response.element(0);
                
                if (theValue.isString())
                {
                    result = (theValue.toString() == YPP_OK_RESPONSE);
                }
                else
                {
                    OD_SYSLOG("! (theValue.isString())");//####
                }
            }
            else
            {
                OD_SYSLOG("! (1 == response.count())");//####
            }
        }
        else
        {
            OD_SYSLOG("! (request.send(YPP_SERVICE_REGISTRY_PORT_NAME, NULL, &response))");//####
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // RegisterLocalService

bool YarpPlusPlus::UnregisterLocalService(const yarp::os::ConstString & portName)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("portName = ", portName.c_str());//####
    bool result = false;
    
    try
    {
        yarp::os::Bottle parameters(portName);
        ServiceRequest   request(YPP_UNREGISTER_REQUEST, parameters);
        ServiceResponse  response;
        
        if (request.send(YPP_SERVICE_REGISTRY_PORT_NAME, NULL, &response))
        {
            // Check that we got a successful self-registration!
            if (1 == response.count())
            {
                yarp::os::Value theValue = response.element(0);
                
                if (theValue.isString())
                {
                    result = (theValue.toString() == YPP_OK_RESPONSE);
                }
                else
                {
                    OD_SYSLOG("! (theValue.isString())");//####
                }
            }
            else
            {
                OD_SYSLOG("! (1 == response.count())");//####
            }
        }
        else
        {
            OD_SYSLOG("! (request.send(YPP_SERVICE_REGISTRY_PORT_NAME, NULL, &response))");//####
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // UnregisterLocalService
