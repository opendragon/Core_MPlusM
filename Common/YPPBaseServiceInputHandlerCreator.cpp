//
//  YPPBaseServiceInputHandlerCreator.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-21.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//
//

#include "YPPBaseServiceInputHandlerCreator.h"
#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPBaseServiceInputHandler.h"

using namespace YarpPlusPlus;

#pragma mark Private structures and constants

#pragma mark Local functions

#pragma mark Class methods

#pragma mark Constructors and destructors

BaseServiceInputHandlerCreator::BaseServiceInputHandlerCreator(BaseService & service) :
        inherited(), _service(service)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P1("service = ", &service);
    OD_SYSLOG_EXIT();//####
} // BaseServiceInputHandlerCreator::BaseServiceInputHandlerCreator

BaseServiceInputHandlerCreator::~BaseServiceInputHandlerCreator(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // BaseServiceInputHandlerCreator::~BaseServiceInputHandlerCreator

#pragma mark Actions

InputHandler * BaseServiceInputHandlerCreator::create(void)
{
    OD_SYSLOG_ENTER();//####
    InputHandler * result = new BaseServiceInputHandler(_service);
    
    OD_SYSLOG_EXIT_P(result);//####
    return result;
} // BaseServiceInputHandlerCreator::create

#pragma mark Accessors

#pragma mark Global functions
