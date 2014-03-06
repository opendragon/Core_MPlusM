//
//  YPPTTest05HandlerCreator.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-28.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPTTest05HandlerCreator.h"
//#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPTTest05Handler.h"

using namespace YarpPlusPlusTest;

#if defined(__APPLE__)
# pragma mark Private structures and constants
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and destructors
#endif // defined(__APPLE__)

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

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

YarpPlusPlus::InputHandler * Test05HandlerCreator::create(void)
{
    return new Test05Handler;
} // Test05HandlerCreator::create

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
