//
//  YPPRegistryService.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-06.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPRegistryService.h"
#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPConfig.h"

YarpPlusPlus::RegistryService::RegistryService(void) : BaseService()
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // YarpPlusPlus::RegistryService::RegistryService

YarpPlusPlus::RegistryService::~RegistryService(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // YarpPlusPlus::RegistryService::~RegistryService
