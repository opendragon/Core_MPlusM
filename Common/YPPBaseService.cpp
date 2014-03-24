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
//#define OD_ENABLE_LOGGING /* */
#include "ODLogging.h"
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
        _requestHandlers(*this),
#if defined(SERVICES_HAVE_CONTEXTS)
        _contexts(),
#endif // defined(SERVICES_HAVE_CONTEXTS)
        _canonicalName(canonicalName), _description(description),
        _endpoint(NULL), _handler(NULL), _handlerCreator(NULL), _started(false),
        _useMultipleHandlers(useMultipleHandlers)
{
    OD_LOG_ENTER();//####
    OD_LOG_B1("useMultipleHandlers = ", useMultipleHandlers);//####
    OD_LOG_S4("canonicalName = ", canonicalName.c_str(), "description = ", description.c_str(),//####
              "serviceEndpointName = ", serviceEndpointName.c_str(), "serviceHostName = ",//####
              serviceHostName.c_str());//####
    OD_LOG_S1("servicePortNumber = ", servicePortNumber.c_str());//####
    _endpoint = new Endpoint(serviceEndpointName, serviceHostName, servicePortNumber);
    setUpRequestHandlers();
    OD_LOG_EXIT_P(this);//####
} // BaseService::BaseService

BaseService::BaseService(const bool                    useMultipleHandlers,
                         const yarp::os::ConstString & canonicalName,
                         const yarp::os::ConstString & description,
                         const int                     argc,
                         char **                       argv) :
        _requestHandlers(*this),
#if defined(SERVICES_HAVE_CONTEXTS)
        _contexts(),
#endif // defined(SERVICES_HAVE_CONTEXTS)
        _canonicalName(canonicalName), _description(description),
        _endpoint(NULL), _handler(NULL), _handlerCreator(NULL), _started(false),
        _useMultipleHandlers(useMultipleHandlers)
{
    OD_LOG_ENTER();//####
    OD_LOG_B1("useMultipleHandlers = ", useMultipleHandlers);//####
    OD_LOG_S2("canonicalName = ", canonicalName.c_str(), "description = ", description.c_str());//####
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
            OD_LOG_EXIT_THROW_S("Invalid parameters for service endpoint");//####
            throw new Exception("Invalid parameters for service endpoint");
            
    }
    setUpRequestHandlers();
    OD_LOG_EXIT_P(this);//####
} // BaseService::BaseService

BaseService::~BaseService(void)
{
    OD_LOG_OBJENTER();//####
    delete _endpoint;
    delete _handler;
    delete _handlerCreator;
#if defined(SERVICES_HAVE_CONTEXTS)
    clearContexts();
#endif // defined(SERVICES_HAVE_CONTEXTS)
    OD_LOG_OBJEXIT();//####
} // BaseService::~BaseService

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

#if defined(SERVICES_HAVE_CONTEXTS)
void BaseService::addContext(const yarp::os::ConstString & key,
                             BaseContext *                 context)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_S1("key = ", key.c_str());//####
    OD_LOG_P1("context = ", context);//####
    try
    {
        if (context)
        {
            _contexts.insert(ContextMapValue(std::string(key), context));
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT();//####
} // BaseService::addContext
#endif // defined(SERVICES_HAVE_CONTEXTS)

#if defined(SERVICES_HAVE_CONTEXTS)
void BaseService::clearContexts(void)
{
    OD_LOG_OBJENTER();//####
    for (ContextMap::iterator walker(_contexts.begin()); _contexts.end() != walker; ++walker)
    {
        BaseContext * value = walker->second;
        
        if (value)
        {
            delete value;
        }
    }
    _contexts.clear();
    OD_LOG_OBJEXIT();//####
} // BaseService::clearContexts
#endif // defined(SERVICES_HAVE_CONTEXTS)

#if defined(SERVICES_HAVE_CONTEXTS)
BaseContext * BaseService::findContext(const yarp::os::ConstString & key)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_S1("key = ", key.c_str());//####
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
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT_P(result);//####
    return result;
} // BaseService::findContext
#endif // defined(SERVICES_HAVE_CONTEXTS)

bool BaseService::processRequest(const yarp::os::ConstString & request,
                                 const yarp::os::Bottle &      restOfInput,
                                 const yarp::os::ConstString & senderPort,
                                 yarp::os::ConnectionWriter *  replyMechanism)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_S3("request = ", request.c_str(), "restOfInput = ", restOfInput.toString().c_str(),//####
              "senderPort = ", senderPort.c_str());//####
    OD_LOG_P1("replyMechanism = ", replyMechanism);//####
    bool result;
    
    try
    {
        BaseRequestHandler * handler = _requestHandlers.lookupRequestHandler(request);
        
        if (handler)
        {
            OD_LOG("(handler)");//####
            result = handler->processRequest(restOfInput, senderPort, replyMechanism);
        }
        else
        {
            OD_LOG("! (handler)");//####
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
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT_B(result);//####
    return result;
} // BaseService::processRequest

#if defined(SERVICES_HAVE_CONTEXTS)
void BaseService::removeContext(const yarp::os::ConstString & key)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_S1("key = ", key.c_str());//####
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
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT();//####
} // BaseService::removeContext
#endif // defined(SERVICES_HAVE_CONTEXTS)

bool BaseService::setTimeout(const float timeout)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_D1("timeout = ", timeout);//####
    bool result = false;
    
    try
    {
        if (! _started)
        {
            result = _endpoint->setTimeout(timeout);
        }
        else
        {
            OD_LOG("! (! _started)");//####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT_B(result);//####
    return result;
} // BaseService::setTimeout

void BaseService::setUpRequestHandlers(void)
{
    OD_LOG_OBJENTER();//####
    try
    {
        _requestHandlers.registerRequestHandler(new InfoRequestHandler());
        _requestHandlers.registerRequestHandler(new ListRequestHandler());
        _requestHandlers.registerRequestHandler(new NameRequestHandler(*this));
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT();//####
} // BaseService::setUpRequestHandlers

bool BaseService::start(void)
{
    OD_LOG_OBJENTER();//####
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
                        OD_LOG("! (_endpoint->setInputHandlerCreator(*_handlerCreator) && _endpoint->open())");//####
                        delete _handlerCreator;
                        _handlerCreator = NULL;
                    }
                }
                else
                {
                    OD_LOG("! (_handlerCreator)");//####
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
                        OD_LOG("! (_endpoint->setInputHandler(*_handler) && _endpoint->open())");//####
                        delete _handler;
                        _handler = NULL;
                    }
                }
                else
                {
                    OD_LOG("! (_handler)");//####
                }
            }
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT_B(_started);//####
    return _started;
} // BaseService::start

bool BaseService::stop(void)
{
    OD_LOG_OBJENTER();//####
    _started = false;
    OD_LOG_OBJEXIT_B(! _started);//####
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
    OD_LOG_ENTER();//####
    OD_LOG_S1("portName = ", portName.c_str());//####
    bool result = false;
    
    try
    {
        yarp::os::ConstString aName(GetRandomPortName("/registerlocal/port_"));
        yarp::os::Port *      newPort = new yarp::os::Port();
        
        if (newPort)
        {
            if (newPort->open(aName))
            {
                if (yarp::os::Network::connect(aName, YPP_SERVICE_REGISTRY_PORT_NAME))
                {
                    yarp::os::Bottle parameters(portName);
                    ServiceRequest   request(YPP_REGISTER_REQUEST, parameters);
                    ServiceResponse  response;
                    
                    if (request.send(*newPort, &response))
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
                                OD_LOG("! (theValue.isString())");//####
                            }
                        }
                        else
                        {
                            OD_LOG("! (1 == response.count())");//####
                        }
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
    OD_LOG_EXIT_B(result);//####
    return result;
} // RegisterLocalService

bool YarpPlusPlus::UnregisterLocalService(const yarp::os::ConstString & portName)
{
    OD_LOG_ENTER();//####
    OD_LOG_S1("portName = ", portName.c_str());//####
    bool result = false;
    
    try
    {
        yarp::os::ConstString aName(GetRandomPortName("/unregisterlocal/port_"));
        yarp::os::Port *      newPort = new yarp::os::Port();
        
        if (newPort)
        {
            if (newPort->open(aName))
            {
                if (yarp::os::Network::connect(aName, YPP_SERVICE_REGISTRY_PORT_NAME))
                {
                    yarp::os::Bottle parameters(portName);
                    ServiceRequest   request(YPP_UNREGISTER_REQUEST, parameters);
                    ServiceResponse  response;
                    
                    if (request.send(*newPort, &response))
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
                                OD_LOG("! (theValue.isString())");//####
                            }
                        }
                        else
                        {
                            OD_LOG("! (1 == response.count())");//####
                        }
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
    OD_LOG_EXIT_B(result);//####
    return result;
} // UnregisterLocalService
