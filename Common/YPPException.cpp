//
//  YPPException.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-07.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPException.h"
//#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"

using namespace YarpPlusPlus;

#pragma mark Private structures and constants

#pragma mark Local functions

#pragma mark Class methods

#pragma mark Constructors and destructors

Exception::Exception(const yarp::os::ConstString & reason)
{
#if (! defined(ENABLE_OD_SYSLOG))
# pragma unused(reason)
#endif // ! defined(ENABLE_OD_SYSLOG)
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("reason = ", reason.c_str());//####
    OD_SYSLOG_EXIT_P(this);//####
} // Exception::Exception

Exception::~Exception(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // Exception::~Exception

#pragma mark Actions

#pragma mark Accessors

#pragma mark Global functions
