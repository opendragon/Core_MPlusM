//
//  YPPExampleService.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-06.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPExampleService.h"
#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPRequests.h"

#pragma mark Private structures and constants

#pragma mark Local functions

#pragma mark Class methods

#pragma mark Constructors and destructors

YarpPlusPlus::ExampleService::ExampleService(const yarp::os::ConstString & serviceEndpointName,
                                             const yarp::os::ConstString & serviceHostName,
                                             const yarp::os::ConstString & servicePortNumber) :
        inherited(true, serviceEndpointName, serviceHostName, servicePortNumber)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // YarpPlusPlus::ExampleService::ExampleService

YarpPlusPlus::ExampleService::ExampleService(const int argc,
                                             char **   argv) :
        BaseService(true, argc, argv)
{
    OD_SYSLOG_ENTER();//####
    
    OD_SYSLOG_EXIT();//####
} // YarpPlusPlus::ExampleService::ExampleService

YarpPlusPlus::ExampleService::~ExampleService(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // YarpPlusPlus::ExampleService::~ExampleService

#pragma mark Actions

void YarpPlusPlus::ExampleService::fillInListReply(yarp::os::Bottle & reply)
{
    OD_SYSLOG_ENTER();//####
    inherited::fillInListReply(reply);
    OD_SYSLOG_EXIT();//####
} // YarpPlusPlus::ExampleService::fillInListReply

bool YarpPlusPlus::ExampleService::start(void)
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
} // YarpPlusPlus::ExampleService::start

bool YarpPlusPlus::ExampleService::stop(void)
{
    OD_SYSLOG_ENTER();//####
    bool result = BaseService::stop();
    
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // YarpPlusPlus::ExampleService::stop

#pragma mark Accessors

#pragma mark Global functions
