//
//  YPPBaseServiceInputHandlerCreator.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-21.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//
//

#include "YPPBaseServiceInputHandlerCreator.h"
//#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPBaseServiceInputHandler.h"

using namespace YarpPlusPlus;

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

BaseServiceInputHandlerCreator::BaseServiceInputHandlerCreator(BaseService & service) :
        inherited(), _service(service)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P1("service = ", &service);
    OD_SYSLOG_EXIT_P(this);//####
} // BaseServiceInputHandlerCreator::BaseServiceInputHandlerCreator

BaseServiceInputHandlerCreator::~BaseServiceInputHandlerCreator(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT_P(this);//####
} // BaseServiceInputHandlerCreator::~BaseServiceInputHandlerCreator

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

InputHandler * BaseServiceInputHandlerCreator::create(void)
{
    OD_SYSLOG_ENTER();//####
    InputHandler * result = new BaseServiceInputHandler(_service);
    
    OD_SYSLOG_EXIT_P(result);//####
    return result;
} // BaseServiceInputHandlerCreator::create

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
