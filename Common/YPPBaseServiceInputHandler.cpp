//
//  YPPBaseServiceInputHandler.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-21.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//
//

#include "YPPBaseServiceInputHandler.h"
//#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPBaseService.h"

using namespace YarpPlusPlus;

#pragma mark Private structures and constants

#pragma mark Local functions

#pragma mark Class methods

#pragma mark Constructors and destructors

BaseServiceInputHandler::BaseServiceInputHandler(BaseService & service) :
        inherited(), _service(service)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P1("service = ", &service);
    OD_SYSLOG_EXIT_P(this);//####
} // BaseServiceInputHandler::BaseServiceInputHandler

BaseServiceInputHandler::~BaseServiceInputHandler(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // BaseServiceInputHandler::~BaseServiceInputHandler

#pragma mark Actions

bool BaseServiceInputHandler::handleInput(const yarp::os::Bottle &     input,
                                          yarp::os::ConnectionWriter * replyMechanism)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P1("replyMechanism = ", replyMechanism);//####
    OD_SYSLOG_S1("got ", input.toString().c_str());//####
    bool result;
    
    if (0 < input.size())
    {
        result = _service.processRequest(input.get(0).toString(), input.tail(), replyMechanism);
    }
    else
    {
        result = true;
    }
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // BaseServiceInputHandler::handleInput

#pragma mark Accessors

#pragma mark Global functions
