//
//  YPPTTest09Service.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-28.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPTTest09Service.h"
#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPTTest09DefaultRequestHandler.h"

using namespace YarpPlusPlusTest;

#pragma mark Private structures and constants

#pragma mark Local functions

#pragma mark Class methods

#pragma mark Constructors and destructors

Test09Service::Test09Service(const int argc,
                             char **   argv) :
        inherited(false, argc, argv)
{
    OD_SYSLOG_ENTER();//####
    setDefaultRequestHandler(new YarpPlusPlusTest::Test09DefaultRequestHandler(*this));
    OD_SYSLOG_EXIT();//####
} // Test09Service::Test09Service

Test09Service::~Test09Service(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // Test09Service::~Test09Service

#pragma mark Actions

#pragma mark Accessors

#pragma mark Global functions
