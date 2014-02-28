//
//  YPPTTest10Service.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-28.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPTTest10Service.h"
#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPTTest10DefaultRequestHandler.h"

using namespace YarpPlusPlusTest;

#pragma mark Private structures and constants

#pragma mark Local functions

#pragma mark Class methods

#pragma mark Constructors and destructors

Test10Service::Test10Service(const int argc,
                             char **   argv) :
        inherited(true, argc, argv)
{
    OD_SYSLOG_ENTER();//####
    setDefaultRequestHandler(new Test10DefaultRequestHandler(*this));
    OD_SYSLOG_EXIT();//####
} // Test10Service::Test10Service

Test10Service::~Test10Service(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // Test10Service::~Test10Service

#pragma mark Actions

#pragma mark Accessors

#pragma mark Global functions
