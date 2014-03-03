//
//  YPPRegistryService.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-06.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPRegistryService.h"
#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPRegisterRequestHandler.h"
#include "YPPRequests.h"
#include "YPPServiceRequest.h"
#include "YPPUnregisterRequestHandler.h"

using namespace YarpPlusPlus;

#pragma mark Private structures and constants

#pragma mark Local functions

#pragma mark Class methods

bool RegistryService::registerLocalService(const yarp::os::ConstString & portName)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("portName = ", portName.c_str());//####
    bool             result = false;
#if 0
    yarp::os::Bottle parameters(portName);
    ServiceRequest   request(YPP_REGISTER_REQUEST, parameters);
    ServiceResponse  response;
    
    if (request.send(getEndpoint(), &response))
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
#endif//0
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // RegistryService::registerLocalService

#pragma mark Constructors and destructors

RegistryService::RegistryService(const yarp::os::ConstString & serviceHostName,
                                 const yarp::os::ConstString & servicePortNumber) :
        inherited(true, YPP_SERVICE_REGISTRY_PORT_NAME, serviceHostName, servicePortNumber), _isActive(false)
{
    OD_SYSLOG_ENTER();//####
    setUpRequestHandlers();
    OD_SYSLOG_EXIT_P(this);//####
} // RegistryService::RegistryService

RegistryService::~RegistryService(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // RegistryService::~RegistryService

#pragma mark Actions

void RegistryService::setUpRequestHandlers(void)
{
    OD_SYSLOG_ENTER();//####
    _requestHandlers.registerRequestHandler(YPP_REGISTER_REQUEST, new RegisterRequestHandler(*this));
    _requestHandlers.registerRequestHandler(YPP_UNREGISTER_REQUEST, new UnregisterRequestHandler(*this));
    OD_SYSLOG_EXIT();//####
} // RegistryService::setUpRequestHandlers

bool RegistryService::start(void)
{
    OD_SYSLOG_ENTER();//####
    if ((! isActive()) && (! isStarted()))
    {
        BaseService::start();
        if (isStarted())
        {
            // Register ourselves!!!
            yarp::os::Bottle parameters(YPP_SERVICE_REGISTRY_PORT_NAME);
            ServiceRequest   request(YPP_REGISTER_REQUEST, parameters);
            ServiceResponse  response;
            
            if (request.send(getEndpoint(), &response))
            {
                // Check that we got a successful self-registration!
                if (1 == response.count())
                {
                    yarp::os::Value theValue = response.element(0);
                    
                    if (theValue.isString())
                    {
                        _isActive = (theValue.toString() == YPP_OK_RESPONSE);
                    }
                }
            }
        }
    }
    OD_SYSLOG_EXIT_B(isStarted());//####
    return isStarted();
} // RegistryService::start

bool RegistryService::stop(void)
{
    OD_SYSLOG_ENTER();//####
    bool result = BaseService::stop();
    
    _isActive = false;
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // RegistryService::stop

#pragma mark Accessors

#pragma mark Global functions
