//
//  YPPExampleEchoService.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-06.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPExampleEchoService.h"
#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPEchoRequestHandler.h"
#include "YPPRequests.h"

using namespace YarpPlusPlusExample;

#if defined(__APPLE__)
# pragma mark Private structures and constants
#endif // defined(__APPLE__)

ExampleEchoService::ExampleEchoService(const yarp::os::ConstString & serviceEndpointName,
                                       const yarp::os::ConstString & serviceHostName,
                                       const yarp::os::ConstString & servicePortNumber) :
        inherited(true, serviceEndpointName, serviceHostName, servicePortNumber)
{
    OD_SYSLOG_ENTER();//####
    setUpRequestHandlers();
    OD_SYSLOG_EXIT();//####
} // ExampleEchoService::ExampleEchoService

ExampleEchoService::ExampleEchoService(const int argc,
                               char **   argv) :
        BaseService(true, argc, argv)
{
    OD_SYSLOG_ENTER();//####
    setUpRequestHandlers();
    OD_SYSLOG_EXIT();//####
} // ExampleEchoService::ExampleEchoService

ExampleEchoService::~ExampleEchoService(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // ExampleEchoService::~ExampleEchoService

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

void ExampleEchoService::setUpRequestHandlers(void)
{
    OD_SYSLOG_ENTER();//####
    _requestHandlers.registerRequestHandler(new EchoRequestHandler());
    OD_SYSLOG_EXIT();//####
} // ExampleEchoService::setUpRequestHandlers

bool ExampleEchoService::start(void)
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
} // ExampleEchoService::start

bool ExampleEchoService::stop(void)
{
    OD_SYSLOG_ENTER();//####
    bool result = BaseService::stop();
    
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // ExampleEchoService::stop
