//
//  YPPExampleService.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-06.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPExampleService.h"
#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPConfig.h"

YarpPlusPlus::ExampleService::ExampleService(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // YarpPlusPlus::ExampleService::ExampleService

YarpPlusPlus::ExampleService::~ExampleService(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // YarpPlusPlus::ExampleService::~ExampleService
