//
//  YPPTTest03Handler.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-28.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPTTest03Handler.h"
//#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"

using namespace YarpPlusPlusTest;

#if defined(__APPLE__)
# pragma mark Private structures and constants
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and destructors
#endif // defined(__APPLE__)

Test03Handler::Test03Handler(void) :
        inherited()
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT_P(this);//####
} // Test03Handler::Test03Handler

Test03Handler::~Test03Handler(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // Test03Handler::~Test03Handler

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

bool Test03Handler::handleInput(const yarp::os::Bottle &     input,
                                yarp::os::ConnectionWriter * replyMechanism)
{
#if (! defined(ENABLE_OD_SYSLOG))
# pragma unused(input,replyMechanism)
#endif // ! defined(ENABLE_OD_SYSLOG)
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P1("replyMechanism = ", replyMechanism);//####
    OD_SYSLOG_S1("got ", input.toString().c_str());//####
    OD_SYSLOG_EXIT_B(true);//####
    return true;
} // Test03Handler::handleInput

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
