//
//  YPPInputHandler.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-11.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPInputHandler.h"
#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPCommon.h"

using namespace YarpPlusPlus;

#pragma mark Private structures and constants

#pragma mark Local functions

#pragma mark Class methods

#pragma mark Constructors and destructors

InputHandler::InputHandler(void) :
        inherited(), _canProcessInput(true)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // InputHandler::InputHandler

InputHandler::~InputHandler(void)
{
    OD_SYSLOG_ENTER();//####
    stopProcessing();
    OD_SYSLOG_EXIT();//####
} // InputHandler::~InputHandler

#pragma mark Actions

bool InputHandler::read(yarp::os::ConnectionReader & connection)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P1("connection = ", &connection);//####
    bool result;
    
    if (_canProcessInput)
    {
//        dumpContact(connection.getRemoteContact());//####
        yarp::os::Bottle aBottle;
        
        aBottle.read(connection);
        result = handleInput(aBottle, connection.getWriter());
    }
    else
    {
        result = true;
    }
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // InputHandler::read

void InputHandler::stopProcessing(void)
{
    OD_SYSLOG_ENTER();//####
    _canProcessInput = false;
    OD_SYSLOG_EXIT();//####
} // InputHandler::stopProcessing

#pragma mark Accessors

#pragma mark Global functions
