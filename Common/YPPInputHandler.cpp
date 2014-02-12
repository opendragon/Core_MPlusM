//
//  YPPInputHandler.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-11.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include <yarp/os/Contact.h>
#include "YPPInputHandler.h"
#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPConfig.h"

using yarp::os::Contact;

#if 0
static void dumpContact(const Contact & aContact)//####
{//####
    OD_SYSLOG_S4("contact.name = ", aContact.getName().c_str(), "contact.host = ", aContact.getHost().c_str(),//####
                 "contact.carrier = ", aContact.getCarrier().c_str(), "contact.toString = ", aContact.toString().c_str());//####
    OD_SYSLOG_L1("contact.port = ", aContact.getPort());//####
    OD_SYSLOG_B1("contact.isValid = ", aContact.isValid());//####
} // dumpContact
#endif//0

YarpPlusPlus::InputHandler::InputHandler(void) :
        _canProcessInput(true)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // YarpPlusPlus::InputHandler::InputHandler

YarpPlusPlus::InputHandler::~InputHandler(void)
{
    OD_SYSLOG_ENTER();//####
    stopProcessing();
    OD_SYSLOG_EXIT();//####
} // YarpPlusPlus::InputHandler::~InputHandler

bool YarpPlusPlus::InputHandler::read(ConnectionReader & connection)
{
    OD_SYSLOG_ENTER();//####
    bool result;
    
    if (_canProcessInput)
    {
//        dumpContact(connection.getRemoteContact());//####
        Bottle aBottle;
        
        aBottle.read(connection);
        result = handleInput(aBottle, connection.getWriter());
    }
    else
    {
        result = true;
    }
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // YarpPlusPlus::InputHandler::read

void YarpPlusPlus::InputHandler::stopProcessing(void)
{
    OD_SYSLOG_ENTER();//####
    _canProcessInput = false;
    OD_SYSLOG_EXIT();//####
} // YarpPlusPlus::InputHandler::stopProcessing
