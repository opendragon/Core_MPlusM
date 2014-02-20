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

#pragma mark Private structures and constants

#pragma mark Local functions

#pragma mark Class methods

#pragma mark Constructors and destructors

YarpPlusPlus::ExampleService::ExampleService(const yarp::os::ConstString & serviceEndpointName,
                                             const yarp::os::ConstString & serviceHostName,
                                             const yarp::os::ConstString & servicePortNumber) :
        BaseService(serviceEndpointName, serviceHostName, servicePortNumber)
{
    OD_SYSLOG_ENTER();//####

    OD_SYSLOG_EXIT();//####
} // YarpPlusPlus::ExampleService::ExampleService

YarpPlusPlus::ExampleService::ExampleService(const int argc,
                                             char **   argv) :
        BaseService(argc, argv)
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

bool YarpPlusPlus::ExampleService::processRequest(yarp::os::Bottle &           input,
                                                  yarp::os::ConnectionWriter * replyMechanism)
{
#pragma unused(input,replyMechanism)
    OD_SYSLOG_ENTER();//####
    bool result = true;
    
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // YarpPlusPlus::ExampleService::processRequest

bool YarpPlusPlus::ExampleService::start(void)
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
