//
//  YPPServiceRequest.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-06.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPServiceRequest.h"
#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPConfig.h"

YarpPlusPlus::ServiceRequest::ServiceRequest(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // YarpPlusPlus::ServiceRequest::ServiceRequest

YarpPlusPlus::ServiceRequest::~ServiceRequest(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####    
} // YarpPlusPlus::ServiceRequest::~ServiceRequest
