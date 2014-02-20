//
//  YPPServiceRequest.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-06.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPServiceRequest.h"
#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPEndpoint.h"
#include "YPPServiceResponse.h"

#pragma mark Private structures and constants

#pragma mark Local functions

#pragma mark Class methods

#pragma mark Constructors and destructors

YarpPlusPlus::ServiceRequest::ServiceRequest(const yarp::os::ConstString & requestName,
                                             const yarp::os::Bottle &      parameters)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("requestName = ", requestName.c_str());//####
    OD_SYSLOG_P1("parameters = ", parameters.toString().c_str());//####
    _name = requestName;
    OD_SYSLOG_LL1("parameter size = ", parameters.size());//####
    for (int ii = 0; ii < parameters.size(); ++ii)
    {
        OD_SYSLOG_S1("parameter = ", parameters.get(ii).asString().c_str());//####
    }
    _parameters = parameters;
    OD_SYSLOG_EXIT();//####
} // YarpPlusPlus::ServiceRequest::ServiceRequest

YarpPlusPlus::ServiceRequest::~ServiceRequest(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####    
} // YarpPlusPlus::ServiceRequest::~ServiceRequest

#pragma mark Actions

bool YarpPlusPlus::ServiceRequest::send(Endpoint &        destination,
                                        ServiceResponse * response)
{
    OD_SYSLOG_ENTER();//####
    // Now we try to connect!
    yarp::os::Port        outPort;
    yarp::os::ConstString aName = YarpPlusPlus::Endpoint::getRandomPortName();
    bool                  result;

    OD_SYSLOG_S1("opening ", aName.c_str());//####
    if (outPort.open(aName))
    {
        OD_SYSLOG("(outPort.open(aName))");//####
        if (outPort.addOutput(destination.getName()))
        {
            OD_SYSLOG("(outPort.addOutput(destination.getName()))");//####
            yarp::os::Bottle message;
            
            OD_SYSLOG_LL1("parameter size = ", _parameters.size());//####
            for (int ii = 0; ii < _parameters.size(); ++ii)
            {
                OD_SYSLOG_S1("parameter = ", _parameters.get(ii).asString().c_str());//####
            }
            message.addString(_name);
            message.append(_parameters);
            if (response)
            {
                yarp::os::Bottle holder;
                
                if (outPort.write(message, holder))
                {
                    OD_SYSLOG("(outPort.write(message, holder))");//####
                    OD_SYSLOG_S1("got ", holder.toString().c_str());//####
                    *response = holder;
                    result = true;
                }
                else
                {
                    OD_SYSLOG("! (outPort.write(message, holder))");//####
                    result = false;
                }
            }
            else if (outPort.write(message))
            {
                OD_SYSLOG("(outPort.write(message))");//####
                result = true;
            }
            else
            {
                OD_SYSLOG("! (outPort.write(message))");//####
                result = false;
            }
        }
        else
        {
            OD_SYSLOG("! (outPort.addOutput(destination.getName()))");//####
            result = false;
        }
        outPort.close();
    }
    else
    {
        OD_SYSLOG("! (outPort.open(aName))");//####
        result = false;
    }
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // YarpPlusPlus::ServiceRequest::send

#pragma mark Accessors

#pragma mark Global functions
