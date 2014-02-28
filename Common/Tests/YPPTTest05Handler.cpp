//
//  YPPTTest05Handler.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-28.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPTTest05Handler.h"
#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"

using namespace YarpPlusPlusTest;

#pragma mark Private structures and constants

#pragma mark Local functions

#pragma mark Class methods

#pragma mark Constructors and destructors

Test05Handler::Test05Handler(void) :
        inherited()
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // Test05Handler::Test05Handler

Test05Handler::~Test05Handler(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // Test05Handler::~Test05Handler

#pragma mark Actions

bool Test05Handler::handleInput(const yarp::os::Bottle &     input,
                                yarp::os::ConnectionWriter * replyMechanism)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P1("replyMechanism = ", replyMechanism);//####
    OD_SYSLOG_S1("got ", input.toString().c_str());//####
    if (replyMechanism)
    {
        yarp::os::Bottle inputCopy(input);
        
        inputCopy.write(*replyMechanism);
    }
    OD_SYSLOG_EXIT_B(TRUE);//####
    return true;
} // Test05Handler::handleInput

#pragma mark Accessors

#pragma mark Global functions
