//
//  YPPException.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-07.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPException.h"
#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPConfig.h"

YarpPlusPlus::Exception::Exception(const ConstString & reason)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("reason = ", reason.c_str());//####
    OD_SYSLOG_EXIT();//####
} // YarpPlusPlus::Exception::Exception

YarpPlusPlus::Exception::~Exception(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // YarpPlusPlus::Exception::~Exception
