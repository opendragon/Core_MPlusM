//
//  YPPTTest10DefaultRequestHandler.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-28.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPTTest10DefaultRequestHandler.h"
#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"

using namespace YarpPlusPlusTest;

#pragma mark Private structures and constants

#pragma mark Local functions

#pragma mark Class methods

#pragma mark Constructors and destructors

Test10DefaultRequestHandler::Test10DefaultRequestHandler(void) :
        inherited("")
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT_P(this);//####
} // Test10DefaultRequestHandler::Test10DefaultRequestHandler

Test10DefaultRequestHandler::~Test10DefaultRequestHandler(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // Test10DefaultRequestHandler::~Test10DefaultRequestHandler

#pragma mark Actions

void Test10DefaultRequestHandler::fillInDescription(yarp::os::Property & info)
{
#pragma unused(info)
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // Test10DefaultRequestHandler::fillInDescription

bool Test10DefaultRequestHandler::operator() (const yarp::os::Bottle &     restOfInput,
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
} // Test10DefaultRequestHandler::operator()

#pragma mark Accessors

#pragma mark Global functions
