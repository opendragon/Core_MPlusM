//
//  YPPTTest05HandlerCreator.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-28.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPTTest05HandlerCreator.h"
#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPTTest05Handler.h"

using namespace YarpPlusPlusTest;

#pragma mark Private structures and constants

#pragma mark Local functions

#pragma mark Class methods

#pragma mark Constructors and destructors

Test05HandlerCreator::Test05HandlerCreator(void) :
        inherited()
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT_P(this);//####
} // Test05HandlerCreator::Test05HandlerCreator

Test05HandlerCreator::~Test05HandlerCreator(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // Test05HandlerCreator::~Test05HandlerCreator

#pragma mark Actions

YarpPlusPlus::InputHandler * Test05HandlerCreator::create(void)
{
    return new Test05Handler;
} // Test05HandlerCreator::create

#pragma mark Accessors

#pragma mark Global functions
