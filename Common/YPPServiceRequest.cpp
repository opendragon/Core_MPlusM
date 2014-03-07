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

ServiceRequest::ServiceRequest(const yarp::os::ConstString & requestName,
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
    OD_SYSLOG_EXIT_P(this);//####
} // ServiceRequest::ServiceRequest

ServiceRequest::~ServiceRequest(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####    
} // ServiceRequest::~ServiceRequest

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

bool ServiceRequest::send(Endpoint &        destinationPort,
                          yarp::os::Port *  usingPort,
                          ServiceResponse * response)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P3("destinationPort = ", &destinationPort, "usingPort = ", usingPort, "response = ", response);//####
    // Now we try to connect!
    yarp::os::Bottle message;
    bool             result;
    
    OD_SYSLOG_LL1("parameter size = ", _parameters.size());//####
    for (int ii = 0; ii < _parameters.size(); ++ii)
    {
        OD_SYSLOG_S1("parameter = ", _parameters.get(ii).asString().c_str());//####
    }
    message.addString(_name);
    message.append(_parameters);
    if (usingPort)
    {
        if (usingPort->getOutputCount())
        {
            OD_SYSLOG("(usingPort->getOutputCount())");//####
            result = true;
        }
        else
        {
            OD_SYSLOG("(! usingPort->getOutputCount())");//####
            if (usingPort->addOutput(destinationPort.getName()))
            {
                OD_SYSLOG("(usingPort->addOutput(destinationPort.getName()))");//####
                result = true;
            }
            else
            {
                OD_SYSLOG("! (usingPort->addOutput(destinationPort.getName()))");//####
                result = false;
            }
        }
        if (result)
        {
            OD_SYSLOG("(result)");//####
            if (response)
            {
                OD_SYSLOG("(response)");//####
                yarp::os::Bottle holder;
                
                if (usingPort->write(message, holder))
                {
                    OD_SYSLOG("(usingPort->write(message, holder))");//####
                    OD_SYSLOG_S1("got ", holder.toString().c_str());//####
                    *response = holder;
                }
                else
                {
                    OD_SYSLOG("! (usingPort->write(message, holder))");//####
                    result = false;
                }
            }
            else if (usingPort->write(message))
            {
                OD_SYSLOG("(usingPort->write(message))");//####
            }
            else
            {
                OD_SYSLOG("! (usingPort->write(message))");//####
                result = false;
            }
        }
    }
    else
    {
        yarp::os::ConstString aName(Endpoint::getRandomPortName());
        yarp::os::Port        outPort;

        if (outPort.open(aName))
        {
            OD_SYSLOG("(outPort.open(aName))");//####
            if (outPort.addOutput(destinationPort.getName()))
            {
                OD_SYSLOG("(outPort.addOutput(destinationPort.getName()))");//####
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
    }
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // ServiceRequest::send

bool ServiceRequest::send(const yarp::os::ConstString & destinationPortName,
                          yarp::os::Port *              usingPort,
                          ServiceResponse *             response)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("destinationPortName = ", destinationPortName.c_str());//####
    OD_SYSLOG_P2("usingPort = ", usingPort, "response = ", response);//####
    // Now we try to connect!
    yarp::os::Bottle message;
    bool             result;
    
    OD_SYSLOG_LL1("parameter size = ", _parameters.size());//####
    for (int ii = 0; ii < _parameters.size(); ++ii)
    {
        OD_SYSLOG_S1("parameter = ", _parameters.get(ii).asString().c_str());//####
    }
    message.addString(_name);
    message.append(_parameters);
    if (usingPort)
    {
        if (usingPort->getOutputCount())
        {
            OD_SYSLOG("(usingPort->getOutputCount())");//####
            result = true;
        }
        else
        {
            OD_SYSLOG("(! usingPort->getOutputCount())");//####
            if (usingPort->addOutput(destinationPortName))
            {
                OD_SYSLOG("(usingPort->addOutput(destinationPortName))");//####
                result = true;
            }
            else
            {
                OD_SYSLOG("! (usingPort->addOutput(destinationPortName))");//####
                result = false;
            }
        }
        if (result)
        {
            OD_SYSLOG("(result)");//####
            if (response)
            {
                OD_SYSLOG("(response)");//####
                yarp::os::Bottle holder;
                
                if (usingPort->write(message, holder))
                {
                    OD_SYSLOG("(usingPort->write(message, holder))");//####
                    OD_SYSLOG_S1("got ", holder.toString().c_str());//####
                    *response = holder;
                }
                else
                {
                    OD_SYSLOG("! (usingPort->write(message, holder))");//####
                    result = false;
                }
            }
            else if (usingPort->write(message))
            {
                OD_SYSLOG("(usingPort->write(message))");//####
            }
            else
            {
                OD_SYSLOG("! (usingPort->write(message))");//####
                result = false;
            }
        }
    }
    else
    {
        yarp::os::Port        outPort;
        yarp::os::ConstString aName(Endpoint::getRandomPortName());

        if (outPort.open(aName))
        {
            OD_SYSLOG("(outPort.open(aName))");//####
            if (outPort.addOutput(destinationPortName))
            {
                OD_SYSLOG("(outPort.addOutput(destinationPortName))");//####
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
                OD_SYSLOG("! (outPort.addOutput(destinationPortName))");//####
                result = false;
            }
            outPort.close();
        }
        else
        {
            OD_SYSLOG("! (outPort.open(aName))");//####
            result = false;
        }
    }
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // ServiceRequest::send

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
