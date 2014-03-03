//
//  YPPTTest09DefaultRequestHandler.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-28.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPTTest09DefaultRequestHandler.h"
#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"

using namespace YarpPlusPlusTest;

#pragma mark Private structures and constants

#pragma mark Local functions

#pragma mark Class methods

#pragma mark Constructors and destructors

Test09DefaultRequestHandler::Test09DefaultRequestHandler(void) :
        inherited("")
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT_P(this);//####
} // Test09DefaultRequestHandler::Test09DefaultRequestHandler

Test09DefaultRequestHandler::~Test09DefaultRequestHandler(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // Test09DefaultRequestHandler::~Test09DefaultRequestHandler

#pragma mark Actions

void Test09DefaultRequestHandler::fillInDescription(yarp::os::Property & info)
{
#pragma unused(info)
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // Test09DefaultRequestHandler::fillInDescription

bool Test09DefaultRequestHandler::operator() (const yarp::os::Bottle &     restOfInput,
                                              yarp::os::ConnectionWriter * replyMechanism)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("restOfInput = ", restOfInput.toString().c_str());//####
    bool result = true;
    
    if (replyMechanism)
    {
        yarp::os::Bottle argsCopy(name());
        
        argsCopy.append(restOfInput);
        argsCopy.write(*replyMechanism);
    }
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // Test09DefaultRequestHandler::operator()

#pragma mark Accessors

#pragma mark Global functions
