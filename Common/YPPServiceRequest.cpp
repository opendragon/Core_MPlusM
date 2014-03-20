//--------------------------------------------------------------------------------------
//
//  File:       YPPServiceRequest.cpp
//
//  Project:    YarpPlusPlus
//
//  Contains:   The class definition for a Yarp++ request.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2014 by HPlus Technologies Ltd. and Simon Fraser University.
//
//              All rights reserved. Redistribution and use in source and binary forms,
//              with or without modification, are permitted provided that the following
//              conditions are met:
//                * Redistributions of source code must retain the above copyright
//                  notice, this list of conditions and the following disclaimer.
//                * Redistributions in binary form must reproduce the above copyright
//                  notice, this list of conditions and the following disclaimer in the
//                  documentation and/or other materials provided with the
//                  distribution.
//                * Neither the name of the copyright holders nor the names of its
//                  contributors may be used to endorse or promote products derived
//                  from this software without specific prior written permission.
//
//              THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
//              "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
//              LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
//              PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
//              OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
//              SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
//              LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
//              DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
//              THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
//              (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
//              OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
//  Created:    2014-02-06
//
//--------------------------------------------------------------------------------------

#include "YPPServiceRequest.h"
//#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPEndpoint.h"
#include <yarp/os/Time.h>

using namespace YarpPlusPlus;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
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
                               const yarp::os::Bottle &      parameters) :
        _name(requestName), _parameters(parameters)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("requestName = ", requestName.c_str());//####
    OD_SYSLOG_P1("parameters = ", parameters.toString().c_str());//####
    OD_SYSLOG_LL1("parameter size = ", parameters.size());//####
    for (int ii = 0; ii < parameters.size(); ++ii)
    {
        OD_SYSLOG_S1("parameter = ", parameters.get(ii).asString().c_str());//####
    }
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
    bool result = false;

    // Now we try to connect!
    try
    {
        yarp::os::Bottle message;
        
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
                result = true;
            }
            else if (usingPort->addOutput(destinationPort.getName()))
            {
                result = true;
            }
            else
            {
                OD_SYSLOG("! (usingPort->addOutput(destinationPort.getName()))");//####
            }
            if (result)
            {
                if (response)
                {
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
                else if (! usingPort->write(message))
                {
                    OD_SYSLOG("(! usingPort->write(message))");//####
                    result = false;
                }
            }
        }
        else
        {
            yarp::os::ConstString aName(GetRandomPortName("request/byendpoint_"));
            yarp::os::Port *      outPort = new yarp::os::Port();
            
            if (outPort)
            {
                if (outPort->open(aName))
                {
                    if (outPort->addOutput(destinationPort.getName()))
                    {
                        if (response)
                        {
                            yarp::os::Bottle holder;
                            
                            if (outPort->write(message, holder))
                            {
                                OD_SYSLOG("(outPort->write(message, holder))");//####
                                OD_SYSLOG_S1("got ", holder.toString().c_str());//####
                                *response = holder;
                                result = true;
                            }
                            else
                            {
                                OD_SYSLOG("! (outPort->write(message, holder))");//####
                            }
                        }
                        else if (outPort->write(message))
                        {
                            OD_SYSLOG("(outPort->write(message))");//####
                            result = true;
                        }
                        else
                        {
                            OD_SYSLOG("! (outPort->write(message))");//####
                        }
                    }
                    else
                    {
                        OD_SYSLOG("! (outPort->addOutput(destination.getName()))");//####
                    }
                    OD_SYSLOG_S1("about to close, port = ", aName.c_str());//####
                    outPort->close();
                    OD_SYSLOG("close completed.");//####
                }
                else
                {
                    OD_SYSLOG("! (outPort->open(aName))");//####
                }
                delete outPort;
            }
            else
            {
                OD_SYSLOG("! (outPort)");
            }
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
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
    bool result = false;

    // Now we try to connect!
    try
    {
        yarp::os::Bottle message;
        
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
                result = true;
            }
            else if (usingPort->addOutput(destinationPortName))
            {
                result = true;
            }
            else
            {
                OD_SYSLOG("! (usingPort->addOutput(destinationPortName))");//####
            }
            if (result)
            {
                if (response)
                {
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
                else if (! usingPort->write(message))
                {
                    OD_SYSLOG("(! usingPort->write(message))");//####
                    result = false;
                }
            }
        }
        else
        {
            yarp::os::ConstString aName(GetRandomPortName("request/byname_"));
            yarp::os::Port *      outPort = new yarp::os::Port();
            
            if (outPort)
            {
                if (outPort->open(aName))
                {
                    if (outPort->addOutput(destinationPortName))
                    {
                        if (response)
                        {
                            yarp::os::Bottle holder;
                            
                            if (outPort->write(message, holder))
                            {
                                OD_SYSLOG("(outPort->write(message, holder))");//####
                                OD_SYSLOG_S1("got ", holder.toString().c_str());//####
                                *response = holder;
                                result = true;
                            }
                            else
                            {
                                OD_SYSLOG("! (outPort->write(message, holder))");//####
                            }
                        }
                        else if (outPort->write(message))
                        {
                            OD_SYSLOG("(outPort->write(message))");//####
                            result = true;
                        }
                        else
                        {
                            OD_SYSLOG("! (outPort->write(message))");//####
                        }
                    }
                    else
                    {
                        OD_SYSLOG("! (outPort->addOutput(destinationPortName))");//####
                    }
                    OD_SYSLOG_S1("about to close, port = ", aName.c_str());//####
                    outPort->close();
                    OD_SYSLOG("close completed.");//####
                }
                else
                {
                    OD_SYSLOG("! (outPort->open(aName))");//####
                }
                delete outPort;
            }
            else
            {
                OD_SYSLOG("! (outPort)");
            }
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
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
