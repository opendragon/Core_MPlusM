//
//  YPPBaseService.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-06.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPBaseService.h"
#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPConfig.h"

YarpPlusPlus::BaseService::BaseService(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // YarpPlusPlus::BaseService::BaseService

YarpPlusPlus::BaseService::~BaseService(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####    
} // YarpPlusPlus::BaseService::~BaseService
