//
//  YPPBaseService.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-06.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPBaseService.h"
//#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPBaseServiceInputHandler.h"
#include "YPPBaseServiceInputHandlerCreator.h"
#include "YPPEndpoint.h"
#include "YPPException.h"
#include "YPPInfoRequestHandler.h"
#include "YPPListRequestHandler.h"
#include "YPPRequests.h"
#include "YPPServiceRequest.h"

using namespace YarpPlusPlus;

#if defined(__APPLE__)
# pragma mark Private structures and constants
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
                         const yarp::os::ConstString & serviceEndpointName,
                         const yarp::os::ConstString & serviceHostName,
                         const yarp::os::ConstString & servicePortNumber) :
        _requestHandlers(*this), _endpoint(NULL), _handler(NULL), _handlerCreator(NULL), _started(false),
        _useMultipleHandlers(useMultipleHandlers)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S3("serviceEndpointName = ", serviceEndpointName.c_str(), "serviceHostName = ",//####
                 serviceHostName.c_str(), "servicePortNumber = ", servicePortNumber.c_str());//####
    _endpoint = new Endpoint(serviceEndpointName, serviceHostName, servicePortNumber);
    setUpRequestHandlers();
    OD_SYSLOG_EXIT_P(this);//####
} // BaseService::BaseService

BaseService::BaseService(const bool useMultipleHandlers,
                         const int  argc,
                         char **    argv) :
        _requestHandlers(*this), _endpoint(NULL), _handler(NULL), _handlerCreator(NULL), _started(false),
        _useMultipleHandlers(useMultipleHandlers)
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
    setUpRequestHandlers();
    OD_SYSLOG_EXIT_P(this);//####
} // BaseService::BaseService

BaseService::~BaseService(void)
{
    OD_SYSLOG_ENTER();//####
    delete _endpoint;
    delete _handler;
    delete _handlerCreator;
    OD_SYSLOG_EXIT();//####
} // BaseService::~BaseService

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

bool BaseService::processRequest(const yarp::os::ConstString & request,
                                 const yarp::os::Bottle &      restOfInput,
                                 yarp::os::ConnectionWriter *  replyMechanism)
{
    OD_SYSLOG_ENTER();//####
    bool             result;
    RequestHandler * handler = _requestHandlers.lookupRequestHandler(request);
    
    if (handler)
    {
        OD_SYSLOG("(handler)");//####
        result = (*handler)(restOfInput, replyMechanism);
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

void BaseService::setUpRequestHandlers(void)
{
    OD_SYSLOG_ENTER();//####
    _requestHandlers.registerRequestHandler(new InfoRequestHandler());
    _requestHandlers.registerRequestHandler(new ListRequestHandler());
    OD_SYSLOG_EXIT();//####
} // BaseService::setUpRequestHandlers

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

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

/*! @brief Find one or more matching local services that are registered with a running Service Registry service.
 @param criteria The matching conditions.
 @returns A (possibly empty) list of matching services. */
yarp::os::Bottle YarpPlusPlus::findMatchingServices(const char * criteria)
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
        result = response.values();
    }
    OD_SYSLOG_EXIT();//####
    return result;
} // YarpPlusPlus::findMatchingServices

bool YarpPlusPlus::registerLocalService(const yarp::os::ConstString & portName)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("portName = ", portName.c_str());//####
    bool             result = false;
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
        }
    }
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // registerLocalService

bool YarpPlusPlus::unregisterLocalService(const yarp::os::ConstString & portName)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("portName = ", portName.c_str());//####
    bool             result = false;
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
        }
    }
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // unregisterLocalService
