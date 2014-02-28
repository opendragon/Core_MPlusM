//
//  YPPTTest11EchoRequestHandler.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-28.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPTTest11EchoRequestHandler.h"
#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "../YPPRequests.h"

using namespace YarpPlusPlusTest;

#pragma mark Private structures and constants

#pragma mark Local functions

#pragma mark Class methods

#pragma mark Constructors and destructors

Test11EchoRequestHandler::Test11EchoRequestHandler(YarpPlusPlus::BaseService & service) :
        inherited(YPP_ECHO_REQUEST, service)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // Test11EchoRequestHandler::Test11EchoRequestHandler

Test11EchoRequestHandler::~Test11EchoRequestHandler(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // Test11EchoRequestHandler::~Test11EchoRequestHandler

#pragma mark Actions

void Test11EchoRequestHandler::fillInDescription(yarp::os::Property & info)
{
#pragma unused(info)
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // Test11EchoRequestHandler::fillInDescription

bool Test11EchoRequestHandler::operator() (const yarp::os::Bottle &     restOfInput,
                                           yarp::os::ConnectionWriter * replyMechanism)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("restOfInput = ", restOfInput.toString().c_str());//####
    bool result = true;
    
    if (replyMechanism)
    {
        yarp::os::Bottle argsCopy(restOfInput);
        
        argsCopy.write(*replyMechanism);
    }
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // Test11EchoRequestHandler::operator()

#pragma mark Accessors

#pragma mark Global functions
