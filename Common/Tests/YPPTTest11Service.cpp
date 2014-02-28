//
//  YPPTTest11Service.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-28.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPTTest11Service.h"
#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPTTest11EchoRequestHandler.h"
#include "../YPPRequests.h"

using namespace YarpPlusPlusTest;

#pragma mark Private structures and constants

#pragma mark Local functions

#pragma mark Class methods

#pragma mark Constructors and destructors

Test11Service::Test11Service(const int argc,
                             char **   argv) :
        inherited(true, argc, argv)
{
    OD_SYSLOG_ENTER();//####
    registerRequestHandler(YPP_ECHO_REQUEST, new Test11EchoRequestHandler(*this));
    OD_SYSLOG_EXIT();//####
} // Test11Service::Test11Service

Test11Service::~Test11Service(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // Test11Service::~Test11Service

#pragma mark Actions

#pragma mark Accessors

#pragma mark Global functions
