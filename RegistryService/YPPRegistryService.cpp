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

#pragma mark Private structures and constants

#pragma mark Local functions

#pragma mark Class methods

#pragma mark Constructors and destructors

YarpPlusPlus::RegistryService::RegistryService(const yarp::os::ConstString & serviceEndpointName,
                                               const yarp::os::ConstString & serviceHostName,
                                               const yarp::os::ConstString & servicePortNumber) :
            BaseService(serviceEndpointName, serviceHostName, servicePortNumber)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // YarpPlusPlus::RegistryService::RegistryService

YarpPlusPlus::RegistryService::RegistryService(const int argc,
                                               char **   argv) :
        BaseService(argc, argv)
{
    OD_SYSLOG_ENTER();//####
    
    OD_SYSLOG_EXIT();//####
} // YarpPlusPlus::RegistryService::RegistryService

YarpPlusPlus::RegistryService::~RegistryService(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // YarpPlusPlus::RegistryService::~RegistryService

#pragma mark Actions

bool YarpPlusPlus::RegistryService::processRequest(yarp::os::Bottle &           input,
                                                   yarp::os::ConnectionWriter * replyMechanism)
{
#pragma unused(input,replyMechanism)
    OD_SYSLOG_ENTER();//####
    bool result = true;
    
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // YarpPlusPlus::RegistryService::processRequest

bool YarpPlusPlus::RegistryService::start(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_ENTER();//####
    if (! _started)
    {
        if (BaseService::start())
        {
//            YarpPlusPlus::Endpoint & ourEndpoint = getEndpoint();
            
        }
    }
    OD_SYSLOG_EXIT_B(_started);//####
    return _started;
} // YarpPlusPlus::RegistryService::start

bool YarpPlusPlus::RegistryService::stop(void)
{
    OD_SYSLOG_ENTER();//####
    bool result = BaseService::stop();
    
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // YarpPlusPlus::RegistryService::stop

#pragma mark Accessors

#pragma mark Global functions
