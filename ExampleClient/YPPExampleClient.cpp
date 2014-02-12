//
//  YPPExampleClient.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-06.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPExampleClient.h"
#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPConfig.h"

YarpPlusPlus::ExampleClient::ExampleClient(void) : BaseClient()
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // YarpPlusPlus::ExampleClient::ExampleClient

YarpPlusPlus::ExampleClient::~ExampleClient(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // YarpPlusPlus::ExampleClient::~ExampleClient
