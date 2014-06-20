//--------------------------------------------------------------------------------------
//
//  File:       M+MBaseService.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for the minimal functionality required for a M+M
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

#include "M+MBaseService.h"
#include "M+MBaseContext.h"
#include "M+MBaseServiceInputHandler.h"
#include "M+MBaseServiceInputHandlerCreator.h"
#include "M+MCountRequestHandler.h"
#include "M+MDetachRequestHandler.h"
#include "M+MEndpoint.h"
#include "M+MException.h"
#include "M+MChannelsRequestHandler.h"
#include "M+MChannelStatusReporter.h"
#include "M+MClientChannel.h"
#include "M+MClientsRequestHandler.h"
#include "M+MInfoRequestHandler.h"
#include "M+MListRequestHandler.h"
#include "M+MNameRequestHandler.h"
#include "M+MPingThread.h"
#include "M+MRequests.h"
#include "M+MServiceRequest.h"
#include "M+MServiceResponse.h"

//#include "ODEnableLogging.h"
#include "ODLogging.h"

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The class definition for the minimal functionality required for an M+M service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;

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

BaseService::BaseService(const char *                  launchPath,
                         const bool                    useMultipleHandlers,
                         const yarp::os::ConstString & canonicalName,
                         const yarp::os::ConstString & description,
                         const yarp::os::ConstString & requestsDescription,
                         const yarp::os::ConstString & serviceEndpointName,
                         const yarp::os::ConstString & serviceHostName,
                         const yarp::os::ConstString & servicePortNumber) :
        _launchPath(launchPath), _contextsLock(), _requestHandlers(*this), _contexts(), _canonicalName(canonicalName),
        _description(description), _requestsDescription(requestsDescription), _requestCount(0), _channelsHandler(NULL),
        _clientsHandler(NULL), _detachHandler(NULL), _infoHandler(NULL), _listHandler(NULL), _nameHandler(NULL),
        _endpoint(NULL), _handler(NULL), _handlerCreator(NULL), _pinger(NULL), _started(false),
        _useMultipleHandlers(useMultipleHandlers)
{
    OD_LOG_ENTER();//####
    OD_LOG_S1("launchPath = ", launchPath);//####
    OD_LOG_B1("useMultipleHandlers = ", useMultipleHandlers);//####
    OD_LOG_S4("canonicalName = ", canonicalName.c_str(), "description = ", description.c_str(),//####
              "requestsDescription = ", requestsDescription.c_str(), "serviceEndpointName = ",//####
              serviceEndpointName.c_str());//####
    OD_LOG_S2("serviceHostName = ", serviceHostName.c_str(), "servicePortNumber = ", servicePortNumber.c_str());//####
    _endpoint = new Endpoint(serviceEndpointName, serviceHostName, servicePortNumber);
    attachRequestHandlers();
    OD_LOG_EXIT_P(this);//####
} // BaseService::BaseService

BaseService::BaseService(const bool                    useMultipleHandlers,
                         const yarp::os::ConstString & canonicalName,
                         const yarp::os::ConstString & description,
                         const yarp::os::ConstString & requestsDescription,
                         const int                     argc,
                         char * *                      argv) :
        _launchPath(*argv), _contextsLock(), _requestHandlers(*this), _contexts(), _canonicalName(canonicalName),
        _description(description), _requestCount(0), _channelsHandler(NULL), _clientsHandler(NULL),
        _detachHandler(NULL), _infoHandler(NULL), _listHandler(NULL), _nameHandler(NULL), _endpoint(NULL),
        _handler(NULL), _handlerCreator(NULL), _pinger(NULL), _started(false), _useMultipleHandlers(useMultipleHandlers)
{
    OD_LOG_ENTER();//####
    OD_LOG_B1("useMultipleHandlers = ", useMultipleHandlers);//####
    OD_LOG_S3("canonicalName = ", canonicalName.c_str(), "description = ", description.c_str(),//####
              "requestsDescription = ", requestsDescription.c_str());//####
    switch (argc)
    {
            // Argument order = endpoint name [, IP address / name [, port]]
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
    attachRequestHandlers();
    OD_LOG_EXIT_P(this);//####
} // BaseService::BaseService

BaseService::~BaseService(void)
{
    OD_LOG_OBJENTER();//####
    stop();
    detachRequestHandlers();
    delete _endpoint;
    delete _handler;
    delete _handlerCreator;
    clearContexts();
    OD_LOG_OBJEXIT();//####
} // BaseService::~BaseService

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

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
            lockContexts();
            _contexts.insert(ContextMapValue(key, context));
            unlockContexts();
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT();//####
} // BaseService::addContext

void BaseService::attachRequestHandlers(void)
{
    OD_LOG_OBJENTER();//####
    try
    {
        _channelsHandler = new ChannelsRequestHandler(*this);
        _clientsHandler = new ClientsRequestHandler(*this);
        _countHandler = new CountRequestHandler(*this);
        _detachHandler = new DetachRequestHandler(*this);
        _infoHandler = new InfoRequestHandler;
        _listHandler = new ListRequestHandler;
        _nameHandler = new NameRequestHandler(*this);
        if (_channelsHandler && _clientsHandler && _countHandler &&  _detachHandler && _infoHandler && _listHandler &&
            _nameHandler)
        {
            _requestHandlers.registerRequestHandler(_channelsHandler);
            _requestHandlers.registerRequestHandler(_clientsHandler);
            _requestHandlers.registerRequestHandler(_countHandler);
            _requestHandlers.registerRequestHandler(_detachHandler);
            _requestHandlers.registerRequestHandler(_infoHandler);
            _requestHandlers.registerRequestHandler(_listHandler);
            _requestHandlers.registerRequestHandler(_nameHandler);
        }
        else
        {
            OD_LOG("! (_channelsHandler && _clientsHandler && _countHandler &&  _detachHandler && "//####
                   "_infoHandler && _listHandler _nameHandler)");//####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT();//####
} // BaseService::attachRequestHandlers

void BaseService::clearContexts(void)
{
    OD_LOG_OBJENTER();//####
    lockContexts();
    for (ContextMap::iterator walker(_contexts.begin()); _contexts.end() != walker; ++walker)
    {
        BaseContext * value = walker->second;
        
        if (value)
        {
            delete value;
        }
    }
    _contexts.clear();
    unlockContexts();
    OD_LOG_OBJEXIT();//####
} // BaseService::clearContexts

void BaseService::detachClient(const yarp::os::ConstString & key)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_S1("key = ", key.c_str());//####
    try
    {
        removeContext(key);
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT();//####
} // BaseService::detachClient

void BaseService::detachRequestHandlers(void)
{
    OD_LOG_OBJENTER();//####
    try
    {
        if (_channelsHandler)
        {
            _requestHandlers.unregisterRequestHandler(_channelsHandler);
            delete _channelsHandler;
            _channelsHandler = NULL;
        }
        if (_clientsHandler)
        {
            _requestHandlers.unregisterRequestHandler(_clientsHandler);
            delete _clientsHandler;
            _clientsHandler = NULL;
        }
        if (_countHandler)
        {
            _requestHandlers.unregisterRequestHandler(_countHandler);
            delete _countHandler;
            _countHandler = NULL;
        }
        if (_detachHandler)
        {
            _requestHandlers.unregisterRequestHandler(_detachHandler);
            delete _detachHandler;
            _detachHandler = NULL;
        }
        if (_infoHandler)
        {
            _requestHandlers.unregisterRequestHandler(_infoHandler);
            delete _infoHandler;
            _infoHandler = NULL;
        }
        if (_listHandler)
        {
            _requestHandlers.unregisterRequestHandler(_listHandler);
            delete _listHandler;
            _listHandler = NULL;
        }
        if (_nameHandler)
        {
            _requestHandlers.unregisterRequestHandler(_nameHandler);
            delete _nameHandler;
            _nameHandler = NULL;
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT();//####
} // BaseService::detachRequestHandlers

void BaseService::fillInSecondaryInputChannelsList(StringVector & channels)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_P1("channels = ", &channels);//####
    channels.clear();
    OD_LOG_OBJEXIT();//####
} // BaseService::fillInSecondaryInputChannelsList

void BaseService::fillInSecondaryOutputChannelsList(StringVector & channels)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_P1("channels = ", &channels);//####
    channels.clear();
    OD_LOG_OBJEXIT();//####
} // BaseService::fillInSecondaryOutputChannelsList

void BaseService::fillInClientList(StringVector & clients)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_P1("clients = ", &clients);//####
    lockContexts();
    
    for (ContextMap::const_iterator walker(_contexts.begin()); _contexts.end() != walker; ++walker)
    {
        clients.push_back(walker->first.c_str());
    }
    unlockContexts();
    OD_LOG_OBJEXIT();//####
} // BaseService::fillInClientList

BaseContext * BaseService::findContext(const yarp::os::ConstString & key)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_S1("key = ", key.c_str());//####
    BaseContext * result = NULL;
    
    try
    {
        lockContexts();
        ContextMap::const_iterator match(_contexts.find(key));
        
        if (_contexts.end() != match)
        {
            result = match->second;
        }
        unlockContexts();
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT_P(result);//####
    return result;
} // BaseService::findContext

void BaseService::getStatistics(long long & count,
                                double &    currentTime)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_P2("count = ", &count, "currentTime = ", &currentTime);//####
    count = _requestCount;
    currentTime = yarp::os::Time::now();
    OD_LOG_OBJEXIT();//####
} // BaseService::getStatistics

bool BaseService::processRequest(const yarp::os::ConstString & request,
                                 const Package &               restOfInput,
                                 const yarp::os::ConstString & senderChannel,
                                 yarp::os::ConnectionWriter *  replyMechanism)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_S3("request = ", request.c_str(), "restOfInput = ", restOfInput.toString().c_str(), "senderChannel = ",//####
              senderChannel.c_str());//####
    OD_LOG_P1("replyMechanism = ", replyMechanism);//####
    bool result = true;
    
    try
    {
        BaseRequestHandler * handler = _requestHandlers.lookupRequestHandler(request);
        
        if (handler)
        {
            OD_LOG("(handler)");//####
            ++_requestCount;
            result = handler->processRequest(request, restOfInput, senderChannel, replyMechanism);
        }
        else
        {
            OD_LOG("! (handler)");//####
            if (replyMechanism)
            {
                Package errorMessage("unrecognized request");
                
                if (! errorMessage.write(*replyMechanism))
                {
                    OD_LOG("(! errorMessage.write(*replyMechanism))");//####
#if defined(MpM_StallOnSendProblem)
                    Common::Stall();
#endif // defined(MpM_StallOnSendProblem)
                }
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

void BaseService::registerRequestHandler(BaseRequestHandler * handler)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_P1("handler = ", handler);//####
    _requestHandlers.registerRequestHandler(handler);
    OD_LOG_OBJEXIT();//####
} // BaseService::registerRequestHandler

void BaseService::removeContext(const yarp::os::ConstString & key)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_S1("key = ", key.c_str());//####
    try
    {
        lockContexts();
        ContextMap::iterator match(_contexts.find(key));
        
        if (_contexts.end() != match)
        {
            BaseContext * value = match->second;
            
            if (value)
            {
                delete value;
            }
            _contexts.erase(match);
        }
        unlockContexts();
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT();//####
} // BaseService::removeContext

bool BaseService::sendPingForChannel(const yarp::os::ConstString & channelName)
{
    OD_LOG_ENTER();//####
    OD_LOG_S1("channelName = ", channelName.c_str());//####
    bool result = false;
    
    try
    {
        yarp::os::ConstString aName(GetRandomChannelName("_ping_/" DEFAULT_CHANNEL_ROOT));
        ClientChannel *       newChannel = new ClientChannel;
        
        if (newChannel)
        {
#if defined(MpM_ReportOnConnections)
            ChannelStatusReporter reporter;
#endif // defined(MpM_ReportOnConnections)
            
#if defined(MpM_ReportOnConnections)
            newChannel->setReporter(reporter);
            newChannel->getReport(reporter);
#endif // defined(MpM_ReportOnConnections)
            if (newChannel->openWithRetries(aName, STANDARD_WAIT_TIME))
            {
                if (NetworkConnectWithRetries(aName, MpM_REGISTRY_CHANNEL_NAME, STANDARD_WAIT_TIME, false))
                {
                    Package         parameters(channelName);
                    ServiceRequest  request(MpM_PING_REQUEST, parameters);
                    ServiceResponse response;
                    
                    if (request.send(*newChannel, &response))
                    {
                        // Check that we got a successful self-registration!
                        if (1 == response.count())
                        {
                            yarp::os::Value theValue = response.element(0);
                            
                            if (theValue.isString())
                            {
                                result = (theValue.toString() == MpM_OK_RESPONSE);
                            }
                            else
                            {
                                OD_LOG("! (theValue.isString())");//####
                            }
                        }
                        else
                        {
                            OD_LOG("! (1 == response.count())");//####
                            OD_LOG_S1("response = ", response.asString().c_str());//####
                        }
                    }
                    else
                    {
                        OD_LOG("! (request.send(*newChannel, &response))");//####
                    }
#if defined(MpM_DoExplicitDisconnect)
                    if (! NetworkDisconnectWithRetries(aName, MpM_REGISTRY_CHANNEL_NAME, STANDARD_WAIT_TIME))
                    {
                        OD_LOG("(! NetworkDisconnectWithRetries(aName, MpM_REGISTRY_CHANNEL_NAME, "//####
                               "STANDARD_WAIT_TIME))");//####
                    }
#endif // defined(MpM_DoExplicitDisconnect)
                }
                else
                {
                    OD_LOG("! (NetworkConnectWithRetries(aName, MpM_REGISTRY_CHANNEL_NAME, STANDARD_WAIT_TIME, "//####
                           "false))");//####
                }
#if defined(MpM_DoExplicitClose)
                newChannel->close();
#endif // defined(MpM_DoExplicitClose)
            }
            else
            {
                OD_LOG("! (newChannel->openWithRetries(aName, STANDARD_WAIT_TIME))");//####
            }
            ClientChannel::RelinquishChannel(newChannel);
        }
        else
        {
            OD_LOG("! (newChannel)");//####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_EXIT_B(result);//####
    return result;
} // BaseService::sendPingForChannel

void BaseService::setDefaultRequestHandler(BaseRequestHandler * handler)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_P1("handler = ", handler);//####
    _requestHandlers.setDefaultRequestHandler(handler);
    OD_LOG_OBJEXIT();//####
} // BaseService::setDefaultRequestHandler

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
                    if (_endpoint->setInputHandlerCreator(*_handlerCreator) && _endpoint->open(STANDARD_WAIT_TIME))
                    {
                        _started = true;
                    }
                    else
                    {
                        OD_LOG("! (_endpoint->setInputHandlerCreator(*_handlerCreator) && "//####
                               "_endpoint->open(STANDARD_WAIT_TIME))");//####
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
                    if (_endpoint->setInputHandler(*_handler) && _endpoint->open(STANDARD_WAIT_TIME))
                    {
                        _started = true;
                    }
                    else
                    {
                        OD_LOG("! (_endpoint->setInputHandler(*_handler) && "//####
                               "_endpoint->open(STANDARD_WAIT_TIME))");//####
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

void BaseService::startPinger(void)
{
    OD_LOG_OBJENTER();//####
    if ((! _pinger) && _endpoint)
    {
        _pinger = new PingThread(_endpoint->getName());
        _pinger->start();
    }
    OD_LOG_OBJEXIT();//####
} // BaseService::startPinger

bool BaseService::stop(void)
{
    OD_LOG_OBJENTER();//####
    if (_pinger)
    {
        _pinger->stop();
        delete _pinger;
        _pinger = NULL;
    }
    _started = false;
    OD_LOG_OBJEXIT_B(! _started);//####
    return (! _started);
} // BaseService::stop

void BaseService::unregisterRequestHandler(BaseRequestHandler * handler)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_P1("handler = ", handler);//####
    _requestHandlers.unregisterRequestHandler(handler);
    OD_LOG_OBJEXIT();//####
} // BaseService::unregisterRequestHandler

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

bool Common::RegisterLocalService(const yarp::os::ConstString & channelName)
{
    OD_LOG_ENTER();//####
    OD_LOG_S1("channelName = ", channelName.c_str());//####
    bool result = false;
    
    try
    {
        yarp::os::ConstString aName(GetRandomChannelName("_registerlocal_/" DEFAULT_CHANNEL_ROOT));
        ClientChannel *       newChannel = new ClientChannel;
        
        if (newChannel)
        {
#if defined(MpM_ReportOnConnections)
            ChannelStatusReporter reporter;
#endif // defined(MpM_ReportOnConnections)
            
#if defined(MpM_ReportOnConnections)
            newChannel->setReporter(reporter);
            newChannel->getReport(reporter);
#endif // defined(MpM_ReportOnConnections)
            if (newChannel->openWithRetries(aName, STANDARD_WAIT_TIME))
            {
                if (NetworkConnectWithRetries(aName, MpM_REGISTRY_CHANNEL_NAME, STANDARD_WAIT_TIME, false))
                {
                    Package         parameters(channelName);
                    ServiceRequest  request(MpM_REGISTER_REQUEST, parameters);
                    ServiceResponse response;
                    
                    if (request.send(*newChannel, &response))
                    {
                        // Check that we got a successful self-registration!
                        if (1 == response.count())
                        {
                            yarp::os::Value theValue = response.element(0);
                            
                            if (theValue.isString())
                            {
                                result = (theValue.toString() == MpM_OK_RESPONSE);
                            }
                            else
                            {
                                OD_LOG("! (theValue.isString())");//####
                            }
                        }
                        else
                        {
                            OD_LOG("! (1 == response.count())");//####
                            OD_LOG_S1("response = ", response.asString().c_str());//####
                        }
                    }
                    else
                    {
                        OD_LOG("! (request.send(*newChannel, &response))");//####
                    }
#if defined(MpM_DoExplicitDisconnect)
                    if (! NetworkDisconnectWithRetries(aName, MpM_REGISTRY_CHANNEL_NAME, STANDARD_WAIT_TIME))
                    {
                        OD_LOG("(! NetworkDisconnectWithRetries(aName, MpM_REGISTRY_CHANNEL_NAME, "//####
                               "STANDARD_WAIT_TIME))");//####
                    }
#endif // defined(MpM_DoExplicitDisconnect)
                }
                else
                {
                    OD_LOG("! (NetworkConnectWithRetries(aName, MpM_REGISTRY_CHANNEL_NAME, STANDARD_WAIT_TIME, "//####
                           "false))");//####
                }
#if defined(MpM_DoExplicitClose)
                newChannel->close();
#endif // defined(MpM_DoExplicitClose)
            }
            else
            {
                OD_LOG("! (newChannel->openWithRetries(aName, STANDARD_WAIT_TIME))");//####
            }
            ClientChannel::RelinquishChannel(newChannel);
        }
        else
        {
            OD_LOG("! (newChannel)");//####
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

bool Common::UnregisterLocalService(const yarp::os::ConstString & channelName)
{
    OD_LOG_ENTER();//####
    OD_LOG_S1("channelName = ", channelName.c_str());//####
    bool result = false;
    
    try
    {
        yarp::os::ConstString aName(GetRandomChannelName("_unregisterlocal_/" DEFAULT_CHANNEL_ROOT));
        ClientChannel *       newChannel = new ClientChannel;
        
        if (newChannel)
        {
#if defined(MpM_ReportOnConnections)
            ChannelStatusReporter reporter;
#endif // defined(MpM_ReportOnConnections)
            
#if defined(MpM_ReportOnConnections)
            newChannel->setReporter(reporter);
            newChannel->getReport(reporter);
#endif // defined(MpM_ReportOnConnections)
            if (newChannel->openWithRetries(aName, STANDARD_WAIT_TIME))
            {
                if (NetworkConnectWithRetries(aName, MpM_REGISTRY_CHANNEL_NAME, STANDARD_WAIT_TIME, false))
                {
                    Package         parameters(channelName);
                    ServiceRequest  request(MpM_UNREGISTER_REQUEST, parameters);
                    ServiceResponse response;
                    
                    if (request.send(*newChannel, &response))
                    {
                        // Check that we got a successful self-registration!
                        if (1 == response.count())
                        {
                            yarp::os::Value theValue = response.element(0);
                            
                            if (theValue.isString())
                            {
                                result = (theValue.toString() == MpM_OK_RESPONSE);
                            }
                            else
                            {
                                OD_LOG("! (theValue.isString())");//####
                            }
                        }
                        else
                        {
                            OD_LOG("! (1 == response.count())");//####
                            OD_LOG_S1("response = ", response.asString().c_str());//####
                       }
                    }
                    else
                    {
                        OD_LOG("! (request.send(*newChannel, &response))");//####
                    }
#if defined(MpM_DoExplicitDisconnect)
                    if (! NetworkDisconnectWithRetries(aName, MpM_REGISTRY_CHANNEL_NAME, STANDARD_WAIT_TIME))
                    {
                        OD_LOG("(! NetworkDisconnectWithRetries(aName, MpM_REGISTRY_CHANNEL_NAME, "//####
                               "STANDARD_WAIT_TIME))");//####
                    }
#endif // defined(MpM_DoExplicitDisconnect)
                }
                else
                {
                    OD_LOG("! (NetworkConnectWithRetries(aName, MpM_REGISTRY_CHANNEL_NAME, STANDARD_WAIT_TIME, "//####
                           "false))");//####
                }
#if defined(MpM_DoExplicitClose)
                newChannel->close();
#endif // defined(MpM_DoExplicitClose)
            }
            else
            {
                OD_LOG("! (newChannel->openWithRetries(aName, STANDARD_WAIT_TIME))");//####
            }
            ClientChannel::RelinquishChannel(newChannel);
        }
        else
        {
            OD_LOG("! (newChannel)");//####
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
