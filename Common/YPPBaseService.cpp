//
//  YPPBaseService.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-06.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPBaseService.h"
#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPException.h"
#include "YPPBaseServiceInputHandler.h"
#include "YPPBaseServiceInputHandlerCreator.h"
#include "YPPInfoRequestHandler.h"
#include "YPPListRequestHandler.h"
#include "YPPRequests.h"
#include <string>

using namespace YarpPlusPlus;

#pragma mark Private structures and constants

#pragma mark Local functions

#pragma mark Class methods

#pragma mark Constructors and destructors

BaseService::BaseService(const bool                    useMultipleHandlers,
                         const yarp::os::ConstString & serviceEndpointName,
                         const yarp::os::ConstString & serviceHostName,
                         const yarp::os::ConstString & servicePortNumber) :
        _endpoint(NULL), _handler(NULL), _handlerCreator(NULL), _requestHandlers(), _defaultHandler(NULL),
        _started(false), _useMultipleHandlers(useMultipleHandlers)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S3("serviceEndpointName = ", serviceEndpointName.c_str(), "serviceHostName = ",//####
                 serviceHostName.c_str(), "servicePortNumber = ", servicePortNumber.c_str());//####
    _endpoint = new Endpoint(serviceEndpointName, serviceHostName, servicePortNumber);
    setUpStandardHandlers();
    OD_SYSLOG_EXIT();//####
} // BaseService::BaseService

BaseService::BaseService(const bool useMultipleHandlers,
                         const int  argc,
                         char **    argv) :
        _endpoint(NULL), _handler(NULL), _handlerCreator(NULL), _requestHandlers(), _defaultHandler(NULL),
        _started(false), _useMultipleHandlers(useMultipleHandlers)
{
    OD_SYSLOG_ENTER();//####
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
    setUpStandardHandlers();
    OD_SYSLOG_EXIT();//####
} // BaseService::BaseService

BaseService::~BaseService(void)
{
    OD_SYSLOG_ENTER();//####
    delete _endpoint;
    delete _handler;
    delete _handlerCreator;
    OD_SYSLOG_EXIT();//####
} // BaseService::~BaseService

#pragma mark Actions

void BaseService::fillInListReply(yarp::os::Bottle & reply)
{
    OD_SYSLOG_ENTER();//####
    RequestHandlerMap::const_iterator requestWalker(_requestHandlers.cbegin());

    for ( ; requestWalker != _requestHandlers.cend(); ++requestWalker)
    {
        yarp::os::Property & aDict = reply.addDict();
        RequestHandler *     aHandler = requestWalker->second;
        
        aHandler->fillInDescription(aDict);
    }
    OD_SYSLOG_EXIT();//####
} // BaseService::fillInListReply

void BaseService::fillInRequestInfo(yarp::os::Bottle &            reply,
                                    const yarp::os::ConstString & requestName)
{
    OD_SYSLOG_ENTER();//####
    RequestHandlerMap::const_iterator match(_requestHandlers.find(std::string(requestName)));
    
    if (_requestHandlers.cend() != match)
    {
        yarp::os::Property & aDict = reply.addDict();
        RequestHandler *     aHandler = match->second;
        
        aHandler->fillInDescription(aDict);
    }
    OD_SYSLOG_EXIT();//####
} // BaseService::fillInRequestInfo

RequestHandler * BaseService::lookupRequestHandler(const yarp::os::ConstString & request)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("request = ", request.c_str());//####
    RequestHandlerMap::const_iterator match(_requestHandlers.find(std::string(request)));
    RequestHandler *                  result;
    
    if (_requestHandlers.cend() == match)
    {
        OD_SYSLOG("(_requestHandlers.end() == match)");//####
        result = _defaultHandler;
    }
    else
    {
        OD_SYSLOG("! (_requestHandlers.end() == match)");//####
        result = match->second;
    }
    OD_SYSLOG_EXIT();//####
    return result;
} // BaseService::lookupRequestHandler

bool BaseService::processRequest(const yarp::os::ConstString & request,
                                 const yarp::os::Bottle &      restOfInput,
                                 yarp::os::ConnectionWriter *  replyMechanism)
{
    OD_SYSLOG_ENTER();//####
    bool             result;
    RequestHandler * handler = lookupRequestHandler(request);
    
    if (handler)
    {
        OD_SYSLOG("(handler)");//####
        result = (*handler)(/*this, request,*/ restOfInput, replyMechanism);
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
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // BaseService::processRequest

void BaseService::registerRequestHandler(const yarp::os::ConstString & request,
                                         RequestHandler *              handler)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("request = ", request.c_str());//####
    _requestHandlers.insert(RequestHandlerMapValue(std::string(request), handler));
    OD_SYSLOG_EXIT();//####
} // BaseService::registerRequestHandler

void BaseService::setDefaultRequestHandler(RequestHandler * handler)
{
    OD_SYSLOG_ENTER();//####
    _defaultHandler = handler;
    OD_SYSLOG_EXIT();//####
} // BaseService::setDefaultRequestHandler

void BaseService::setUpStandardHandlers(void)
{
    OD_SYSLOG_ENTER();//####
    registerRequestHandler(YPP_INFO_REQUEST, new InfoRequestHandler(*this));
    registerRequestHandler(YPP_LIST_REQUEST, new ListRequestHandler(*this));
    OD_SYSLOG_EXIT();//####
} // BaseService::setUpStandardHandlers

bool BaseService::start(void)
{
    OD_SYSLOG_ENTER();//####
    if (! _started)
    {
        if (_useMultipleHandlers)
        {
            _handlerCreator = new BaseServiceInputHandlerCreator(*this);
            if (_handlerCreator)
            {
                Endpoint & ourEndpoint = getEndpoint();
                
                if (ourEndpoint.setInputHandlerCreator(*_handlerCreator) && ourEndpoint.open())
                {
                    _started = true;
                }
                else
                {
                    delete _handlerCreator;
                    _handlerCreator = NULL;
                }
            }
        }
        else
        {
            _handler = new BaseServiceInputHandler(*this);
            if (_handler)
            {
                Endpoint & ourEndpoint = getEndpoint();
                
                if (ourEndpoint.setInputHandler(*_handler) && ourEndpoint.open())
                {
                    _started = true;
                }
                else
                {
                    delete _handler;
                    _handler = NULL;
                }
            }
        }
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

#pragma mark Accessors

#pragma mark Global functions
