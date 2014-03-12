//
//  YPPExampleRandomNumberService.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-06.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPExampleRandomNumberService.h"
//#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPRandomRequestHandler.h"
#include "YPPRequests.h"

using namespace YarpPlusPlusExample;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

ExampleRandomNumberService::ExampleRandomNumberService(const yarp::os::ConstString & serviceEndpointName,
                                                       const yarp::os::ConstString & serviceHostName,
                                                       const yarp::os::ConstString & servicePortNumber) :
        inherited(true, serviceEndpointName, serviceHostName, servicePortNumber)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S3("serviceEndpointName = ", serviceEndpointName.c_str(), "serviceHostName = ",//####
                 serviceHostName.c_str(), "servicePortNumber = ", servicePortNumber.c_str());//####
    setUpRequestHandlers();
    OD_SYSLOG_EXIT();//####
} // ExampleRandomNumberService::ExampleRandomNumberService

ExampleRandomNumberService::~ExampleRandomNumberService(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // ExampleRandomNumberService::~ExampleRandomNumberService

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

void ExampleRandomNumberService::setUpRequestHandlers(void)
{
    OD_SYSLOG_ENTER();//####
    _requestHandlers.registerRequestHandler(new RandomRequestHandler());
    OD_SYSLOG_EXIT();//####
} // ExampleRandomNumberService::setUpRequestHandlers

bool ExampleRandomNumberService::start(void)
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
} // ExampleRandomNumberService::start

bool ExampleRandomNumberService::stop(void)
{
    OD_SYSLOG_ENTER();//####
    bool result = BaseService::stop();
    
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // ExampleRandomNumberService::stop
