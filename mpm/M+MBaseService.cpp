//--------------------------------------------------------------------------------------------------
//
//  File:       mpm/M+MBaseService.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for the minimal functionality required for an M+M service.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2014 by H Plus Technologies Ltd. and Simon Fraser University.
//
//              All rights reserved. Redistribution and use in source and binary forms, with or
//              without modification, are permitted provided that the following conditions are met:
//                * Redistributions of source code must retain the above copyright notice, this list
//                  of conditions and the following disclaimer.
//                * Redistributions in binary form must reproduce the above copyright notice, this
//                  list of conditions and the following disclaimer in the documentation and / or
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
//  Created:    2014-02-06
//
//--------------------------------------------------------------------------------------------------

#include "M+MArgumentsRequestHandler.h"
#include "M+MChannelsRequestHandler.h"
#include "M+MClientsRequestHandler.h"
#include "M+MDetachRequestHandler.h"
#include "M+MGetMetricsRequestHandler.h"
#include "M+MGetMetricsStateRequestHandler.h"
#include "M+MInfoRequestHandler.h"
#include "M+MListRequestHandler.h"
#include "M+MNameRequestHandler.h"
#include "M+MPingThread.h"
#include "M+MSetMetricsStateRequestHandler.h"
#include "M+MStopRequestHandler.h"

#include <mpm/M+MBaseService.h>
#include <mpm/M+MBaseContext.h>
#include <mpm/M+MClientChannel.h>
#include <mpm/M+MEndpoint.h>
#include <mpm/M+MException.h>
#include <mpm/M+MRequests.h>
#include <mpm/M+MServiceInputHandler.h>
#include <mpm/M+MServiceInputHandlerCreator.h>
#include <mpm/M+MServiceRequest.h>
#include <mpm/M+MServiceResponse.h>
#include <mpm/M+MUtilities.h>
#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4512)
#endif // ! MAC_OR_LINUX_
#include <mpm/optionparser.h>
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the minimal functionality required for an M+M service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using std::cout;
using std::endl;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief Whether or not metrics are initially being gathered. */
#if defined(MpM_MetricsInitiallyOn)
# define MEASUREMENTS_ON true
#else // ! defined(MpM_MetricsInitiallyOn)
# define MEASUREMENTS_ON false
#endif // ! defined(MpM_MetricsInitiallyOn)

/*! @brief The accepted command line arguments for services. */
#define STANDARD_SERVICE_OPTIONS "e:hp:rt:"

#if defined(__APPLE__)
# pragma mark Global constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

BaseService::BaseService(const ServiceKind  theKind,
                         const YarpString & launchPath,
                         const int          argc,
                         char * *           argv,
                         const YarpString & tag,
                         const bool         useMultipleHandlers,
                         const YarpString & canonicalName,
                         const YarpString & description,
                         const YarpString & requestsDescription,
                         const YarpString & serviceEndpointName,
                         const YarpString & servicePortNumber) :
    _launchPath(launchPath), _contextsLock(), _requestHandlers(*this), _contexts(),
    _description(description), _requestsDescription(requestsDescription), _tag(tag),
    _auxCounters(), _argumentsHandler(NULL), _channelsHandler(NULL), _clientsHandler(NULL),
    _detachHandler(NULL), _getMetricsHandler(NULL), _getMetricsStateHandler(NULL),
    _infoHandler(NULL), _listHandler(NULL), _nameHandler(NULL), _setMetricsStateHandler(NULL),
    _stopHandler(NULL), _endpoint(NULL), _handler(NULL), _handlerCreator(NULL), _pinger(NULL),
    _kind(theKind), _metricsEnabled(MEASUREMENTS_ON), _started(false),
    _useMultipleHandlers(useMultipleHandlers)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S4s("launchPath = ", launchPath, "canonicalName = ", canonicalName, //####
               "description = ", description, "requestsDescription = ", requestsDescription); //####
    OD_LOG_S2s("serviceEndpointName = ", serviceEndpointName, "servicePortNumber = ", //####
               servicePortNumber); //####
    OD_LOG_LL1("argc = ", argc); //####
    OD_LOG_P1("argv = ", argv); //####
    OD_LOG_B1("useMultipleHandlers = ", useMultipleHandlers); //####
    if (0 < _tag.size())
    {
        _serviceName = canonicalName + " " + _tag;
    }
    else
    {
        _serviceName = canonicalName;
    }
    _endpoint = new Endpoint(serviceEndpointName, servicePortNumber);
    if (_metricsEnabled)
    {
        _endpoint->enableMetrics();
    }
    else
    {
        _endpoint->disableMetrics();
    }
    for (int ii = 0; argc > ii; ++ii)
    {
        _originalArguments.push_back(argv[ii]);
    }
    attachRequestHandlers();
    OD_LOG_EXIT_P(this); //####
} // BaseService::BaseService

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
BaseService::BaseService(const ServiceKind  theKind,
                         const YarpString & launchPath,
                         const int          argc,
                         char * *           argv,
                         const bool         useMultipleHandlers,
                         const YarpString & canonicalName,
                         const YarpString & description,
                         const YarpString & requestsDescription) :
    _launchPath(launchPath), _contextsLock(), _requestHandlers(*this), _contexts(),
    _description(description), _requestsDescription(requestsDescription),
    _serviceName(canonicalName), _tag(), _auxCounters(), _argumentsHandler(NULL),
    _channelsHandler(NULL), _clientsHandler(NULL), _detachHandler(NULL), _getMetricsHandler(NULL),
    _getMetricsStateHandler(NULL), _infoHandler(NULL), _listHandler(NULL), _nameHandler(NULL),
    _setMetricsStateHandler(NULL), _stopHandler(NULL), _endpoint(NULL), _handler(NULL),
    _handlerCreator(NULL), _pinger(NULL), _kind(theKind), _metricsEnabled(MEASUREMENTS_ON),
    _started(false), _useMultipleHandlers(useMultipleHandlers)
{
#if (! defined(OD_ENABLE_LOGGING))
# if MAC_OR_LINUX_
#  pragma unused(requestsDescription)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING)
    OD_LOG_ENTER(); //####
    OD_LOG_S4s("launchPath = ", launchPath, "canonicalName = ", canonicalName, //####
               "description = ", description, "requestsDescription = ", requestsDescription); //####
    OD_LOG_LL1("argc = ", argc); //####
    OD_LOG_P1("argv = ", argv); //####
    OD_LOG_B1("useMultipleHandlers = ", useMultipleHandlers); //####
    switch (argc)
    {
            // Argument order = endpoint name [, port]
	    case 1 :
            _endpoint = new Endpoint(*argv);
            break;
            
	    case 2 :
            _endpoint = new Endpoint(*argv, argv[1]);
            break;
            
	    default :
            OD_LOG_EXIT_THROW_S("Invalid parameters for service endpoint"); //####
            throw new Exception("Invalid parameters for service endpoint");
            
    }
    if (_metricsEnabled)
    {
        _endpoint->enableMetrics();
    }
    else
    {
        _endpoint->disableMetrics();
    }
    for (int ii = 0; argc > ii; ++ii)
    {
        _originalArguments.push_back(argv[ii]);
    }
    attachRequestHandlers();
    OD_LOG_EXIT_P(this); //####
} // BaseService::BaseService
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

BaseService::~BaseService(void)
{
    OD_LOG_OBJENTER(); //####
    stop();
    detachRequestHandlers();
    delete _endpoint;
    delete _handler;
    delete _handlerCreator;
    clearContexts();
    OD_LOG_OBJEXIT(); //####
} // BaseService::~BaseService

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void BaseService::addContext(const YarpString & key,
                             BaseContext *      context)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_S1s("key = ", key); //####
    OD_LOG_P1("context = ", context); //####
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
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT(); //####
} // BaseService::addContext

void BaseService::attachRequestHandlers(void)
{
    OD_LOG_OBJENTER(); //####
    try
    {
        _argumentsHandler = new ArgumentsRequestHandler(*this);
        _channelsHandler = new ChannelsRequestHandler(*this);
        _clientsHandler = new ClientsRequestHandler(*this);
        _detachHandler = new DetachRequestHandler(*this);
        _getMetricsHandler = new GetMetricsRequestHandler(*this);
        _getMetricsStateHandler = new GetMetricsStateRequestHandler(*this);
        _infoHandler = new InfoRequestHandler(*this);
        _listHandler = new ListRequestHandler(*this);
        _nameHandler = new NameRequestHandler(*this);
        _setMetricsStateHandler = new SetMetricsStateRequestHandler(*this);
        _stopHandler = new StopRequestHandler(*this);
        if (_argumentsHandler && _channelsHandler && _clientsHandler && _detachHandler &&
            _getMetricsHandler && _getMetricsStateHandler && _infoHandler && _listHandler &&
            _nameHandler && _setMetricsStateHandler && _stopHandler)
        {
            _requestHandlers.registerRequestHandler(_argumentsHandler);
            _requestHandlers.registerRequestHandler(_channelsHandler);
            _requestHandlers.registerRequestHandler(_clientsHandler);
            _requestHandlers.registerRequestHandler(_detachHandler);
            _requestHandlers.registerRequestHandler(_getMetricsHandler);
            _requestHandlers.registerRequestHandler(_getMetricsStateHandler);
            _requestHandlers.registerRequestHandler(_infoHandler);
            _requestHandlers.registerRequestHandler(_listHandler);
            _requestHandlers.registerRequestHandler(_nameHandler);
            _requestHandlers.registerRequestHandler(_setMetricsStateHandler);
            _requestHandlers.registerRequestHandler(_stopHandler);
        }
        else
        {
            OD_LOG("! (_argumentsHandler && _channelsHandler && _clientsHandler && " //####
                   "_detachHandler && _getMetricsHandler && _getMetricsStateHandler && " //####
                   "_infoHandler && _listHandler && _nameHandler && " //####
                   "_setMetricsStateHandler && _stopHandler)"); //####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT(); //####
} // BaseService::attachRequestHandlers

void BaseService::clearContexts(void)
{
    OD_LOG_OBJENTER(); //####
    lockContexts();
    if (0 < _contexts.size())
    {
        for (ContextMap::const_iterator walker(_contexts.begin()); _contexts.end() != walker;
             ++walker)
        {
            BaseContext * value = walker->second;
            
            if (value)
            {
                delete value;
            }
        }
        _contexts.clear();
    }
    unlockContexts();
    OD_LOG_OBJEXIT(); //####
} // BaseService::clearContexts

void BaseService::detachClient(const YarpString & key)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_S1s("key = ", key); //####
    try
    {
        removeContext(key);
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT(); //####
} // BaseService::detachClient

void BaseService::detachRequestHandlers(void)
{
    OD_LOG_OBJENTER(); //####
    try
    {
        if (_argumentsHandler)
        {
            _requestHandlers.unregisterRequestHandler(_argumentsHandler);
            delete _argumentsHandler;
            _argumentsHandler = NULL;
        }
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
        if (_detachHandler)
        {
            _requestHandlers.unregisterRequestHandler(_detachHandler);
            delete _detachHandler;
            _detachHandler = NULL;
        }
        if (_getMetricsHandler)
        {
            _requestHandlers.unregisterRequestHandler(_getMetricsHandler);
            delete _getMetricsHandler;
            _getMetricsHandler = NULL;
        }
        if (_getMetricsStateHandler)
        {
            _requestHandlers.unregisterRequestHandler(_getMetricsStateHandler);
            delete _getMetricsStateHandler;
            _getMetricsStateHandler = NULL;
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
        if (_setMetricsStateHandler)
        {
            _requestHandlers.unregisterRequestHandler(_setMetricsStateHandler);
            delete _setMetricsStateHandler;
            _setMetricsStateHandler = NULL;
        }
        if (_stopHandler)
        {
            _requestHandlers.unregisterRequestHandler(_stopHandler);
            delete _stopHandler;
            _stopHandler = NULL;
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT(); //####
} // BaseService::detachRequestHandlers

void BaseService::disableMetrics(void)
{
    OD_LOG_OBJENTER(); //####
    _metricsEnabled = false;
    if (_endpoint)
    {
        _endpoint->disableMetrics();
    }
    OD_LOG_OBJEXIT(); //####
} // BaseService::disableMetrics

void BaseService::enableMetrics(void)
{
    OD_LOG_OBJENTER(); //####
    if (_endpoint)
    {
        _endpoint->enableMetrics();
    }
    _metricsEnabled = true;
    OD_LOG_OBJEXIT(); //####
} // BaseService::enableMetrics

void BaseService::fillInClientList(YarpStringVector & clients)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("clients = ", &clients); //####
    lockContexts();
    if (0 < _contexts.size())
    {
        for (ContextMap::const_iterator walker(_contexts.begin()); _contexts.end() != walker;
             ++walker)
        {
            clients.push_back(walker->first.c_str());
        }
    }
    unlockContexts();
    OD_LOG_OBJEXIT(); //####
} // BaseService::fillInClientList

void BaseService::fillInSecondaryClientChannelsList(ChannelVector & channels)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("channels = ", &channels); //####
    channels.clear();
    OD_LOG_OBJEXIT(); //####
} // BaseService::fillInSecondaryClientChannelsList

void BaseService::fillInSecondaryInputChannelsList(ChannelVector & channels)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("channels = ", &channels); //####
    channels.clear();
    OD_LOG_OBJEXIT(); //####
} // BaseService::fillInSecondaryInputChannelsList

void BaseService::fillInSecondaryOutputChannelsList(ChannelVector & channels)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("channels = ", &channels); //####
    channels.clear();
    OD_LOG_OBJEXIT(); //####
} // BaseService::fillInSecondaryOutputChannelsList

BaseContext * BaseService::findContext(const YarpString & key)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_S1s("key = ", key); //####
    BaseContext * result = NULL;
    
    try
    {
        lockContexts();
        ContextMap::iterator match(_contexts.find(key));
        
        if (_contexts.end() != match)
        {
            result = match->second;
        }
        unlockContexts();
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_P(result); //####
    return result;
} // BaseService::findContext

void BaseService::gatherMetrics(yarp::os::Bottle & metrics)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("metrics = ", &metrics); //####
    if (_endpoint)
    {
        SendReceiveCounters counters;
    
        _endpoint->getSendReceiveCounters(counters);
        counters.addToList(metrics, _endpoint->getName());
    }
    _auxCounters.addToList(metrics, "auxiliary");
    OD_LOG_OBJEXIT(); //####
} // BaseService::gatherMetrics

void BaseService::incrementAuxiliaryCounters(const SendReceiveCounters & additionalCounters)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("additionalCounters = ", &additionalCounters); //####
    _auxCounters += additionalCounters;
    OD_LOG_OBJEXIT(); //####
} // BaseService::incrementAuxiliaryCounters

bool BaseService::processRequest(const YarpString &           request,
                                 const yarp::os::Bottle &     restOfInput,
                                 const YarpString &           senderChannel,
                                 yarp::os::ConnectionWriter * replyMechanism)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_S3s("request = ", request, "restOfInput = ", restOfInput.toString(), //####
               "senderChannel = ", senderChannel); //####
    OD_LOG_P1("replyMechanism = ", replyMechanism); //####
    bool result = true;
    
    try
    {
        BaseRequestHandler * handler = _requestHandlers.lookupRequestHandler(request);
        
        if (handler)
        {
            OD_LOG("(handler)"); //####
            result = handler->processRequest(request, restOfInput, senderChannel, replyMechanism);
        }
        else
        {
            OD_LOG("! (handler)"); //####
            if (replyMechanism)
            {
                OD_LOG("(replyMechanism)"); //####
                yarp::os::Bottle errorMessage("unrecognized request");
                
                OD_LOG_S1s("errorMessage <- ", errorMessage.toString()); //####
                if (! errorMessage.write(*replyMechanism))
                {
                    OD_LOG("(! errorMessage.write(*replyMechanism))"); //####
#if defined(MpM_StallOnSendProblem)
                    Stall();
#endif // defined(MpM_StallOnSendProblem)
                }
            }
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
} // BaseService::processRequest

void BaseService::registerRequestHandler(BaseRequestHandler * handler)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("handler = ", handler); //####
    _requestHandlers.registerRequestHandler(handler);
    OD_LOG_OBJEXIT(); //####
} // BaseService::registerRequestHandler

void BaseService::removeContext(const YarpString & key)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_S1s("key = ", key); //####
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
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT(); //####
} // BaseService::removeContext

bool BaseService::sendPingForChannel(const YarpString & channelName,
                                     CheckFunction      checker,
                                     void *             checkStuff)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_S1s("channelName = ", channelName); //####
    OD_LOG_P1("checkStuff = ", checkStuff); //####
    bool result = false;
    
    try
    {
        YarpString              aName(GetRandomChannelName(HIDDEN_CHANNEL_PREFIX "ping_/"
                                                           DEFAULT_CHANNEL_ROOT));
        ClientChannel *         newChannel = new ClientChannel;
#if defined(MpM_ReportOnConnections)
        ChannelStatusReporter * reporter = Utilities::GetGlobalStatusReporter();
#endif // defined(MpM_ReportOnConnections)
        
        if (newChannel)
        {
            if (_metricsEnabled)
            {
                newChannel->enableMetrics();
            }
            else
            {
                newChannel->disableMetrics();
            }
#if defined(MpM_ReportOnConnections)
            newChannel->setReporter(*reporter);
            newChannel->getReport(*reporter);
#endif // defined(MpM_ReportOnConnections)
            if (newChannel->openWithRetries(aName, STANDARD_WAIT_TIME))
            {
                if (Utilities::NetworkConnectWithRetries(aName, MpM_REGISTRY_ENDPOINT_NAME,
                                                         STANDARD_WAIT_TIME, false, checker,
                                                         checkStuff))
                {
                    yarp::os::Bottle parameters(channelName);
                    ServiceRequest   request(MpM_PING_REQUEST, parameters);
                    ServiceResponse  response;
                    
                    if (request.send(*newChannel, response))
                    {
                        // Check that we got a successful ping!
                        if (MpM_EXPECTED_PING_RESPONSE_SIZE == response.count())
                        {
                            yarp::os::Value theValue = response.element(0);
                            
                            if (theValue.isString())
                            {
                                result = (theValue.toString() == MpM_OK_RESPONSE);
                            }
                            else
                            {
                                OD_LOG("! (theValue.isString())"); //####
                            }
                        }
                        else
                        {
                            OD_LOG("! (MpM_EXPECTED_PING_RESPONSE_SIZE == " //####
                                   "response.count())"); //####
                            OD_LOG_S1s("response = ", response.asString()); //####
                        }
                    }
                    else
                    {
                        OD_LOG("! (request.send(*newChannel, response))"); //####
                    }
#if defined(MpM_DoExplicitDisconnect)
                    if (! Utilities::NetworkDisconnectWithRetries(aName, MpM_REGISTRY_ENDPOINT_NAME,
                                                                  STANDARD_WAIT_TIME, checker,
                                                                  checkStuff))
                    {
                        OD_LOG("(! Utilities::NetworkDisconnectWithRetries(aName, " //####
                               "MpM_REGISTRY_ENDPOINT_NAME, STANDARD_WAIT_TIME, checker, " //####
                               "checkStuff))"); //####
                    }
#endif // defined(MpM_DoExplicitDisconnect)
                }
                else
                {
                    OD_LOG("! (Utilities::NetworkConnectWithRetries(aName, " //####
                           "MpM_REGISTRY_ENDPOINT_NAME, STANDARD_WAIT_TIME, false, checker, " //####
                           "checkStuff))"); //####
                }
#if defined(MpM_DoExplicitClose)
                newChannel->close();
#endif // defined(MpM_DoExplicitClose)
            }
            else
            {
                OD_LOG("! (newChannel->openWithRetries(aName, STANDARD_WAIT_TIME))"); //####
            }
            SendReceiveCounters newCounters;
            
            newChannel->getSendReceiveCounters(newCounters);
            incrementAuxiliaryCounters(newCounters);
            BaseChannel::RelinquishChannel(newChannel);
        }
        else
        {
            OD_LOG("! (newChannel)"); //####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // BaseService::sendPingForChannel

void BaseService::setDefaultRequestHandler(BaseRequestHandler * handler)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("handler = ", handler); //####
    _requestHandlers.setDefaultRequestHandler(handler);
    OD_LOG_OBJEXIT(); //####
} // BaseService::setDefaultRequestHandler

bool BaseService::start(void)
{
    OD_LOG_OBJENTER(); //####
    try
    {
        if (! _started)
        {
            if (_useMultipleHandlers)
            {
                _handlerCreator = new ServiceInputHandlerCreator(*this);
                if (_handlerCreator)
                {
                    if (_endpoint->setInputHandlerCreator(*_handlerCreator) &&
                        _endpoint->open(STANDARD_WAIT_TIME))
                    {
                        _started = true;
                    }
                    else
                    {
                        OD_LOG("! (_endpoint->setInputHandlerCreator(*_handlerCreator) && " //####
                               "_endpoint->open(STANDARD_WAIT_TIME))"); //####
                        delete _handlerCreator;
                        _handlerCreator = NULL;
                    }
                }
                else
                {
                    OD_LOG("! (_handlerCreator)"); //####
                }
            }
            else
            {
                _handler = new ServiceInputHandler(*this);
                if (_handler)
                {
                    if (_endpoint->setInputHandler(*_handler) &&
                        _endpoint->open(STANDARD_WAIT_TIME))
                    {
                        _started = true;
                    }
                    else
                    {
                        OD_LOG("! (_endpoint->setInputHandler(*_handler) && " //####
                               "_endpoint->open(STANDARD_WAIT_TIME))"); //####
                        delete _handler;
                        _handler = NULL;
                    }
                }
                else
                {
                    OD_LOG("! (_handler)"); //####
                }
            }
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(_started); //####
    return _started;
} // BaseService::start

void BaseService::startPinger(void)
{
    OD_LOG_OBJENTER(); //####
    if ((! _pinger) && _endpoint)
    {
        _pinger = new PingThread(_endpoint->getName(), *this);
        _pinger->start();
    }
    OD_LOG_OBJEXIT(); //####
} // BaseService::startPinger

bool BaseService::stop(void)
{
    OD_LOG_OBJENTER(); //####
    if (_pinger)
    {
        _pinger->stop();
        delete _pinger;
        _pinger = NULL;
    }
    _started = false;
    OD_LOG_OBJEXIT_B(! _started); //####
    return ! _started;
} // BaseService::stop

void BaseService::unregisterRequestHandler(BaseRequestHandler * handler)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("handler = ", handler); //####
    _requestHandlers.unregisterRequestHandler(handler);
    OD_LOG_OBJEXIT(); //####
} // BaseService::unregisterRequestHandler

void BaseService::updateResponseCounters(const size_t numBytes)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_LL1("numBytes = ", numBytes); //####
    if (_endpoint && _metricsEnabled)
    {
        _endpoint->updateSendCounters(numBytes);
    }
    OD_LOG_OBJEXIT(); //####
} // BaseService::updateResponseCounters

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

bool Common::ProcessStandardServiceOptions(const int                     argc,
                                           char * *                      argv,
                                           Utilities::DescriptorVector & argumentDescriptions,
                                           const YarpString &            defaultEndpointNameRoot,
                                           const YarpString &            serviceDescription,
                                           const YarpString &            matchingCriteria,
                                           const int                     year,
                                           const char *                  copyrightHolder,
                                           bool &                        goWasSet,
                                           bool &                        nameWasSet,
                                           bool &                        reportOnExit,
                                           YarpString &                  tag,
                                           YarpString &                  serviceEndpointName,
                                           YarpString &                  servicePortNumber,
                                           const OptionsMask             skipOptions,
                                           YarpStringVector *            arguments)
{
    OD_LOG_ENTER(); //####
    OD_LOG_L2("argc = ", argc, "year = ", year); //####
    OD_LOG_P4("argv = ", argv, "argumentDescriptions = ", &argumentDescriptions, //####
              "nameWasSet = ", &nameWasSet, "reportOnExit = ", &reportOnExit); //####
    OD_LOG_P1("arguments = ", arguments); //####
    OD_LOG_S3s("defaultEndpointNameRoot = ", defaultEndpointNameRoot, //####
               "serviceDescription = ", serviceDescription, "matchingCriteria = ", //####
               matchingCriteria); //####
    OD_LOG_S1("copyrightHolder = ", copyrightHolder); //####
    enum optionIndex
    {
        kOptionUNKNOWN,
        kOptionARGS,
        kOptionCHANNEL,
        kOptionENDPOINT,
        kOptionGO,
        kOptionHELP,
        kOptionINFO,
        kOptionPORT,
        kOptionREPORT,
        kOptionTAG,
        kOptionVERSION
    }; // optionIndex
    
    bool       isAdapter = (0 < matchingCriteria.length());
    bool       keepGoing = true;
    bool       reportEndpoint = false;
    YarpString serviceKindName(isAdapter ? "adapter" : "service");
    YarpString goPartText("  --go, -g          Start the ");
    YarpString infoPartText("  --info, -i        Print executable type, supported ");
    YarpString reportPartText("  --report, -r      Report the ");
    YarpString tagPartText("  --tag, -t         Specify the tag to be used as part of the ");
    
    goPartText += serviceKindName + " immediately";
    infoPartText += serviceKindName + " options";
    if (isAdapter)
    {
        infoPartText += ", matching criteria";
    }
    infoPartText += " and description and exit";
    reportPartText += serviceKindName + " metrics when the application exits";
    tagPartText += serviceKindName + " name";
    Option_::Descriptor   firstDescriptor(kOptionUNKNOWN, 0, "", "", Option_::Arg::None, NULL);
    Option_::Descriptor   argsDescriptor(kOptionARGS, 0, "a", "args", Option_::Arg::None,
                                         T_("  --args, -a        Report the argument formats"));
    Option_::Descriptor   channelDescriptor(kOptionCHANNEL, 0, "c", "channel", Option_::Arg::None,
                                            T_("  --channel, -c     Report the actual endpoint "
                                               "name"));
    Option_::Descriptor   endpointDescriptor(kOptionENDPOINT, 0, "e", "endpoint",
                                             Option_::Arg::Required,
                                             T_("  --endpoint, -e    Specify an alternative "
                                                "endpoint name to be used"));
    Option_::Descriptor   goDescriptor(kOptionGO, 0, "g", "go", Option_::Arg::None,
                                       goPartText.c_str());
    Option_::Descriptor   helpDescriptor(kOptionHELP, 0, "h", "help", Option_::Arg::None,
                                         T_("  --help, -h        Print usage and exit"));
    Option_::Descriptor   infoDescriptor(kOptionINFO, 0, "i", "info", Option_::Arg::None,
                                         infoPartText.c_str());
    Option_::Descriptor   portDescriptor(kOptionPORT, 0, "p", "port", Option_::Arg::Required,
                                         T_("  --port, -p        Specify a non-default port to be "
                                            "used"));
    Option_::Descriptor   reportDescriptor(kOptionREPORT, 0, "r", "report", Option_::Arg::None,
                                           reportPartText.c_str());
    Option_::Descriptor   tagDescriptor(kOptionTAG, 0, "t", "tag", Option_::Arg::Required,
                                        tagPartText.c_str());
    Option_::Descriptor   versionDescriptor(kOptionVERSION, 0, "v", "vers", Option_::Arg::None,
                                            T_("  --vers, -v        Print version information and "
                                               "exit"));
    Option_::Descriptor   lastDescriptor(0, 0, NULL, NULL, NULL, NULL);
    Option_::Descriptor   usage[12];
    Option_::Descriptor * usageWalker = usage;
    int                   argcWork = argc;
    char * *              argvWork = argv;
    YarpString            usageString("USAGE: ");
    YarpString            argList(ArgumentsToArgString(argumentDescriptions));

    reportOnExit = nameWasSet = goWasSet = false;
    tag = serviceEndpointName = serviceEndpointName = "";
    if (arguments)
    {
        arguments->clear();
    }
    usageString += *argv;
    usageString += " [options]";
    if (0 < argList.length())
    {
        YarpStringVector descriptions;

        Utilities::ArgumentsToDescriptionArray(argumentDescriptions, descriptions, 2);
        usageString += " ";
        usageString += argList + "\n\n";
        for (int ii = 0, mm = descriptions.size(); mm > ii; ++ii)
        {
            if (0 < ii)
            {
                usageString += "\n";
            }
            usageString += "  ";
            usageString += descriptions[ii];
        }
    }
    usageString += "\n\nOptions:";
#if MAC_OR_LINUX_
    firstDescriptor.help = strdup(usageString.c_str());
#else // ! MAC_OR_LINUX_
    firstDescriptor.help = _strdup(usageString.c_str());
#endif // ! MAC_OR_LINUX_
	memcpy(usageWalker++, &firstDescriptor, sizeof(firstDescriptor));
    if (! (skipOptions & kSkipArgsOption))
    {
        memcpy(usageWalker++, &argsDescriptor, sizeof(argsDescriptor));
    }
    if (! (skipOptions & kSkipChannelOption))
    {
        memcpy(usageWalker++, &channelDescriptor, sizeof(channelDescriptor));
    }
    if (! (skipOptions & kSkipEndpointOption))
    {
		memcpy(usageWalker++, &endpointDescriptor, sizeof(endpointDescriptor));
    }
    if (! (skipOptions & kSkipGoOption))
    {
        memcpy(usageWalker++, &goDescriptor, sizeof(goDescriptor));
    }
    memcpy(usageWalker++, &helpDescriptor, sizeof(helpDescriptor));
    if (! (skipOptions & kSkipInfoOption))
    {
        memcpy(usageWalker++, &infoDescriptor, sizeof(infoDescriptor));
    }
    if (! (skipOptions & kSkipPortOption))
    {
        memcpy(usageWalker++, &portDescriptor, sizeof(portDescriptor));
    }
    if (! (skipOptions & kSkipReportOption))
    {
        memcpy(usageWalker++, &reportDescriptor, sizeof(reportDescriptor));
    }
    if (! (skipOptions & kSkipTagOption))
    {
		memcpy(usageWalker++, &tagDescriptor, sizeof(tagDescriptor));
    }
	memcpy(usageWalker++, &versionDescriptor, sizeof(versionDescriptor));
	memcpy(usageWalker++, &lastDescriptor, sizeof(lastDescriptor));
    argcWork -= (argc > 0);
    argvWork += (argc > 0); // skip program name argv[0] if present
    Option_::Stats    stats(usage, argcWork, argvWork);
    Option_::Option * options = new Option_::Option[stats.options_max];
    Option_::Option * buffer = new Option_::Option[stats.buffer_max];
    Option_::Parser   parse(usage, argcWork, argvWork, options, buffer, 1);
    
    if (parse.error())
    {
        keepGoing = false;
    }
    else if (options[kOptionHELP] || options[kOptionUNKNOWN])
    {
        Option_::printUsage(cout, usage, HELP_LINE_LENGTH);
        keepGoing = false;
    }
    else if (options[kOptionVERSION])
    {
        YarpString mpmVersionString(SanitizeString(MpM_VERSION, true));
        
        cout << "Version " << mpmVersionString.c_str() << ": Copyright (c) " << year << " by " <<
                copyrightHolder << "." << endl;
        keepGoing = false;
    }
    else if (options[kOptionARGS])
    {
        for (int ii = 0, mm = argumentDescriptions.size(); mm > ii; ++ii)
        {
            Utilities::BaseArgumentDescriptor * anArg = argumentDescriptions[ii];

            if (0 < ii)
            {
                cout << "\t";
            }
            if (anArg)
            {
                cout << anArg->toString().c_str();
            }
        }
        cout << endl;
        keepGoing = false;
    }
    else if (options[kOptionINFO])
    {
        bool needTab = true;
        
        // Note that we don't report the 'h' and 'v' options, as they are not involved in
        // determining what choices to offer when launching a service.
        cout << (isAdapter ? "Adapter" : "Service");
        if (! (skipOptions & kSkipArgsOption))
        {
            if (needTab)
            {
                cout << "\t";
                needTab = false;
            }
            cout << "a";
        }
        if (! (skipOptions & kSkipChannelOption))
        {
            if (needTab)
            {
                cout << "\t";
                needTab = false;
            }
            cout << "c";
        }
        if (! (skipOptions & kSkipEndpointOption))
        {
            if (needTab)
            {
                cout << "\t";
                needTab = false;
            }
            cout << "e";
        }
        if (! (skipOptions & kSkipGoOption))
        {
            if (needTab)
            {
                cout << "\t";
                needTab = false;
            }
            cout << "g";
        }
        if (! (skipOptions & kSkipInfoOption))
        {
            if (needTab)
            {
                cout << "\t";
                needTab = false;
            }
            cout << "i";
        }
        if (! (skipOptions & kSkipPortOption))
        {
            if (needTab)
            {
                cout << "\t";
                needTab = false;
            }
            cout << "p";
        }
        if (! (skipOptions & kSkipReportOption))
        {
            if (needTab)
            {
                cout << "\t";
                needTab = false;
            }
            cout << "r";
        }
        if (! (skipOptions & kSkipTagOption))
        {
            if (needTab)
            {
                cout << "\t";
                needTab = false;
            }
            cout << "t";
        }
        if (needTab)
        {
            cout << "\t";
        }
        cout << "\t" << matchingCriteria.c_str() << "\t" << serviceDescription.c_str() << endl;
        keepGoing = false;
    }
    else if (ProcessArguments(argumentDescriptions, parse))
    {
        if (options[kOptionGO])
        {
            goWasSet = true;
        }
        if (options[kOptionCHANNEL])
        {
            reportEndpoint = true;
        }
        if (options[kOptionREPORT])
        {
            reportOnExit = true;
        }
        if (options[kOptionENDPOINT])
        {
            serviceEndpointName = options[kOptionENDPOINT].arg;
            OD_LOG_S1s("serviceEndpointName <- ", serviceEndpointName); //####
        }
        if (options[kOptionPORT])
        {
            servicePortNumber = options[kOptionPORT].arg;
            OD_LOG_S1s("servicePortNumber <- ", servicePortNumber); //####
            if (0 < servicePortNumber.length())
            {
                const char * startPtr = servicePortNumber.c_str();
                char *       endPtr;
                int          aPort = static_cast<int>(strtol(startPtr, &endPtr, 10));

                if ((startPtr == endPtr) || *endPtr || (! Utilities::ValidPortNumber(aPort)))
                {
                    cout << "Bad port number." << endl;
                    keepGoing = false;
                }
            }
        }
        if (options[kOptionTAG])
        {
            tag = options[kOptionTAG].arg;
            OD_LOG_S1s("tag <- ", tag); //####
        }
        if (arguments)
        {
            for (int ii = 0; ii < parse.nonOptionsCount(); ++ii)
            {
                arguments->push_back(parse.nonOption(ii));
            }
        }
    }
    else
    {
        cout << "One or more invalid or missing arguments." << endl;
        keepGoing = false;
    }
    delete[] options;
    delete[] buffer;
    if (0 < serviceEndpointName.size())
    {
        nameWasSet = true;
    }
    else if (0 < tag.size())
    {
        serviceEndpointName = defaultEndpointNameRoot + "/" + tag;
    }
    else
    {
        serviceEndpointName = defaultEndpointNameRoot;
    }
    if (reportEndpoint)
    {
        cout << serviceEndpointName.c_str() << endl;
        keepGoing = false;
    }
    OD_LOG_EXIT_B(keepGoing); //####
    return keepGoing;
} // Common::ProcessStandardServiceOptions

bool Common::RegisterLocalService(const YarpString & channelName,
                                  BaseService &      service,
                                  CheckFunction      checker,
                                  void *             checkStuff)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("channelName = ", channelName); //####
    OD_LOG_P2("service = ", &service, "checkStuff = ", checkStuff); //####
    bool result = false;
    
    try
    {
        YarpString              aName(GetRandomChannelName(HIDDEN_CHANNEL_PREFIX "registerlocal_/"
                                                           DEFAULT_CHANNEL_ROOT));
        ClientChannel *         newChannel = new ClientChannel;
#if defined(MpM_ReportOnConnections)
        ChannelStatusReporter * reporter = Utilities::GetGlobalStatusReporter();
#endif // defined(MpM_ReportOnConnections)
        
        if (newChannel)
        {
            if (service.metricsAreEnabled())
            {
                newChannel->enableMetrics();
            }
            else
            {
                newChannel->disableMetrics();
            }
#if defined(MpM_ReportOnConnections)
            newChannel->setReporter(*reporter);
            newChannel->getReport(*reporter);
#endif // defined(MpM_ReportOnConnections)
            if (newChannel->openWithRetries(aName, STANDARD_WAIT_TIME))
            {
                if (Utilities::NetworkConnectWithRetries(aName, MpM_REGISTRY_ENDPOINT_NAME,
                                                         STANDARD_WAIT_TIME, false, checker,
                                                         checkStuff))
                {
                    yarp::os::Bottle parameters(channelName);
                    ServiceRequest   request(MpM_REGISTER_REQUEST, parameters);
                    ServiceResponse  response;
                    
                    if (request.send(*newChannel, response))
                    {
                        // Check that we got a successful self-registration!
                        if (MpM_EXPECTED_REGISTER_RESPONSE_SIZE == response.count())
                        {
                            yarp::os::Value theValue = response.element(0);
                            
                            if (theValue.isString())
                            {
                                result = (theValue.toString() == MpM_OK_RESPONSE);
                            }
                            else
                            {
                                OD_LOG("! (theValue.isString())"); //####
                            }
                        }
                        else
                        {
                            OD_LOG("! (MpM_EXPECTED_REGISTER_RESPONSE_SIZE == " //####
                                   "response.count())"); //####
                            OD_LOG_S1s("response = ", response.asString()); //####
                        }
                    }
                    else
                    {
                        OD_LOG("! (request.send(*newChannel, response))"); //####
                    }
#if defined(MpM_DoExplicitDisconnect)
                    if (! Utilities::NetworkDisconnectWithRetries(aName, MpM_REGISTRY_ENDPOINT_NAME,
                                                                  STANDARD_WAIT_TIME, checker,
                                                                  checkStuff))
                    {
                        OD_LOG("(! Utilities::NetworkDisconnectWithRetries(aName, " //####
                               "MpM_REGISTRY_ENDPOINT_NAME, STANDARD_WAIT_TIME, checker, " //####
                               "checkStuff))"); //####
                    }
#endif // defined(MpM_DoExplicitDisconnect)
                }
                else
                {
                    OD_LOG("! (Utilities::NetworkConnectWithRetries(aName, " //####
                           "MpM_REGISTRY_ENDPOINT_NAME, STANDARD_WAIT_TIME, false, checker, " //####
                           "checkStuff))"); //####
                }
#if defined(MpM_DoExplicitClose)
                newChannel->close();
#endif // defined(MpM_DoExplicitClose)
            }
            else
            {
                OD_LOG("! (newChannel->openWithRetries(aName, STANDARD_WAIT_TIME))"); //####
            }
            SendReceiveCounters newCounters;
            
            newChannel->getSendReceiveCounters(newCounters);
            service.incrementAuxiliaryCounters(newCounters);
            BaseChannel::RelinquishChannel(newChannel);
        }
        else
        {
            OD_LOG("! (newChannel)"); //####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // Common::RegisterLocalService

bool Common::UnregisterLocalService(const YarpString & channelName,
                                    BaseService &      service,
                                    CheckFunction      checker,
                                    void *             checkStuff)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("channelName = ", channelName); //####
    OD_LOG_P2("service = ", &service, "checkStuff = ", checkStuff); //####
    bool result = false;
    
    try
    {
        YarpString              aName(GetRandomChannelName(HIDDEN_CHANNEL_PREFIX "unregisterlocal_/"
                                                           DEFAULT_CHANNEL_ROOT));
        ClientChannel *         newChannel = new ClientChannel;
#if defined(MpM_ReportOnConnections)
        ChannelStatusReporter * reporter = Utilities::GetGlobalStatusReporter();
#endif // defined(MpM_ReportOnConnections)
        
        if (newChannel)
        {
            if (service.metricsAreEnabled())
            {
                newChannel->enableMetrics();
            }
            else
            {
                newChannel->disableMetrics();
            }
#if defined(MpM_ReportOnConnections)
            newChannel->setReporter(*reporter);
            newChannel->getReport(*reporter);
#endif // defined(MpM_ReportOnConnections)
            if (newChannel->openWithRetries(aName, STANDARD_WAIT_TIME))
            {
                if (Utilities::NetworkConnectWithRetries(aName, MpM_REGISTRY_ENDPOINT_NAME,
                                                         STANDARD_WAIT_TIME, false, checker,
                                                         checkStuff))
                {
                    yarp::os::Bottle parameters(channelName);
                    ServiceRequest   request(MpM_UNREGISTER_REQUEST, parameters);
                    ServiceResponse  response;
                    
                    if (request.send(*newChannel, response))
                    {
                        // Check that we got a successful self-deregistration!
                        if (MpM_EXPECTED_UNREGISTER_RESPONSE_SIZE == response.count())
                        {
                            yarp::os::Value theValue = response.element(0);
                            
                            if (theValue.isString())
                            {
                                result = (theValue.toString() == MpM_OK_RESPONSE);
                            }
                            else
                            {
                                OD_LOG("! (theValue.isString())"); //####
                            }
                        }
                        else
                        {
                            OD_LOG("! (MpM_EXPECTED_UNREGISTER_RESPONSE_SIZE == " //####
                                   "response.count())"); //####
                            OD_LOG_S1s("response = ", response.asString()); //####
                        }
                    }
                    else
                    {
                        OD_LOG("! (request.send(*newChannel, response))"); //####
                    }
#if defined(MpM_DoExplicitDisconnect)
                    if (! Utilities::NetworkDisconnectWithRetries(aName, MpM_REGISTRY_ENDPOINT_NAME,
                                                                  STANDARD_WAIT_TIME, checker,
                                                                  checkStuff))
                    {
                        OD_LOG("(! Utilities::NetworkDisconnectWithRetries(aName, " //####
                               "MpM_REGISTRY_ENDPOINT_NAME, STANDARD_WAIT_TIME, checker, " //####
                               "checkStuff))"); //####
                    }
#endif // defined(MpM_DoExplicitDisconnect)
                }
                else
                {
                    OD_LOG("! (Utilities::NetworkConnectWithRetries(aName, " //####
                           "MpM_REGISTRY_ENDPOINT_NAME, STANDARD_WAIT_TIME, false, checker, " //####
                           "checkStuff))"); //####
                }
#if defined(MpM_DoExplicitClose)
                newChannel->close();
#endif // defined(MpM_DoExplicitClose)
            }
            else
            {
                OD_LOG("! (newChannel->openWithRetries(aName, STANDARD_WAIT_TIME))"); //####
            }
            SendReceiveCounters newCounters;
            
            newChannel->getSendReceiveCounters(newCounters);
            service.incrementAuxiliaryCounters(newCounters);
            BaseChannel::RelinquishChannel(newChannel);
        }
        else
        {
            OD_LOG("! (newChannel)"); //####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // Common::UnregisterLocalService
