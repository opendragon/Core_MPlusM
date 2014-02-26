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
#include "YPPRequests.h"

#pragma mark Private structures and constants

#pragma mark Local functions

#pragma mark Class methods

#pragma mark Constructors and destructors

YarpPlusPlus::RegistryService::RegistryService(const yarp::os::ConstString & serviceEndpointName,
                                               const yarp::os::ConstString & serviceHostName,
                                               const yarp::os::ConstString & servicePortNumber) :
        inherited(true, serviceEndpointName, serviceHostName, servicePortNumber)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // YarpPlusPlus::RegistryService::RegistryService

YarpPlusPlus::RegistryService::RegistryService(const int argc,
                                               char **   argv) :
        BaseService(true, argc, argv)
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

void YarpPlusPlus::RegistryService::fillInListReply(yarp::os::Bottle & reply)
{
    OD_SYSLOG_ENTER();//####
    inherited::fillInListReply(reply);
    OD_SYSLOG_EXIT();//####
} // YarpPlusPlus::RegistryService::fillInListReply

bool YarpPlusPlus::RegistryService::start(void)
{
    OD_SYSLOG_ENTER();//####
    if (! isStarted())
    {
        BaseService::start();
        if (isStarted())
        {
            
        }
    }
    OD_SYSLOG_EXIT_B(isStarted());//####
    return isStarted();
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
