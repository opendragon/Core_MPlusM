//
//  YPPTTest12Service.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-28.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPTTest12Service.h"
#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPTTest12EchoRequestHandler.h"
#include "../YPPRequests.h"

using namespace YarpPlusPlusTest;

#pragma mark Private structures and constants

#pragma mark Local functions

#pragma mark Class methods

#pragma mark Constructors and destructors

Test12Service::Test12Service(const int argc,
                             char **   argv) :
        inherited(true, argc, argv)
{
    OD_SYSLOG_ENTER();//####
    _requestHandlers.registerRequestHandler(YPP_ECHO_REQUEST, new Test12EchoRequestHandler());
    OD_SYSLOG_EXIT();//####
} // Test12Service::Test12Service

Test12Service::~Test12Service(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // Test12Service::~Test12Service

#pragma mark Actions

#pragma mark Accessors

#pragma mark Global functions
