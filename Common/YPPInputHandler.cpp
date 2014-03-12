//
//  YPPInputHandler.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-11.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPInputHandler.h"
//#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPCommon.h"

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

InputHandler::InputHandler(void) :
        inherited(), _canProcessInput(true)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT_P(this);//####
} // InputHandler::InputHandler

InputHandler::~InputHandler(void)
{
    OD_SYSLOG_ENTER();//####
    stopProcessing();
    OD_SYSLOG_EXIT();//####
} // InputHandler::~InputHandler

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

bool InputHandler::read(yarp::os::ConnectionReader & connection)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P1("connection = ", &connection);//####
    bool result;
    
    if (_canProcessInput)
    {
//        DumpContact(connection.getRemoteContact());//####
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

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
