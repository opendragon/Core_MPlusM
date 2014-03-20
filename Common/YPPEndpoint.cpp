//--------------------------------------------------------------------------------------
//
//  File:       YPPEndpoint.cpp
//
//  Project:    YarpPlusPlus
//
//  Contains:   The class definition for the connection endpoint for a Yarp++ service.
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
//  Created:    2014-02-07
//
//--------------------------------------------------------------------------------------

#include "YPPEndpoint.h"
//#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPCommon.h"
#include "YPPException.h"
#include <iostream>
#include <yarp/os/Network.h>
#include <yarp/os/Time.h>

using namespace YarpPlusPlus;
using std::cout;
using std::endl;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

//#define REPORT_CONTACT_DETAILS /* Report details of the open() method. */

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

/*! @brief Check if the given port number is valid.
 @param realPort The numeric value of 'portNumber'.
 @param portNumber The port number as a string to be checked.
 @returns @c true if the port number string is numeric or empty. */
static bool checkHostPort(int &                         realPort,
                          const yarp::os::ConstString & portNumber)
{
    bool result = true;

    try
    {
        int  portLength = portNumber.length();
        
        if (0 < portLength)
        {
            for (int ii = 0; result && (ii < portLength); ++ii)
            {
                result = isdigit(portNumber[ii]);
            }
            if (result)
            {
                realPort = atoi(portNumber.c_str());
            }
        }
        else
        {
            // Empty port number - YARP will pick a port for us.
            realPort = 0;
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    return result;
} // checkHostPort

/*! @brief Check if the given host name is valid.
 @param workingContact The connection information that is to be filled in.
 @param hostName The host name that is to be validated.
 @param portNumber The port number to be applied to the connection.
 @returns @c true if the connection information has been constructed and @c false otherwise. */
static bool checkHostName(yarp::os::Contact &            workingContact,
                          const yarp::os::ConstString &  hostName,
                          const int                      portNumber)
{
#if defined(REPORT_CONTACT_DETAILS)
    DumpContact("enter checkHostName", workingContact);//####
#endif // defined(REPORT_CONTACT_DETAILS)
    bool result = false;
    
    try
    {
        if (0 < hostName.length())
        {
            // Non-empty hostname - check it...
            yarp::os::ConstString ipAddress(yarp::os::Contact::convertHostToIp(hostName));
            
            OD_SYSLOG_S1("ipAddress = ", ipAddress.c_str());//####
            workingContact = workingContact.addSocket("tcp", ipAddress, portNumber);
#if defined(REPORT_CONTACT_DETAILS)
            DumpContact("after addSocket", workingContact);//####
#endif // defined(REPORT_CONTACT_DETAILS)
            result = workingContact.isValid();
        }
        else
        {
            // Empty host name - YARP will use the local machine name.
            result = true;
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    return result;
} // checkHostName

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

bool Endpoint::CheckEndpointName(const yarp::os::ConstString & portName)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P1("this = ", this);//####
    OD_SYSLOG_S1("portName = ", portName.c_str());//####
    bool result = false;

    try
    {
        int  nameLength = portName.length();
        
        if (0 < nameLength)
        {
            char firstChar = portName[0];
            
            result = ('/' == firstChar);
            for (int ii = 1; result && (ii < nameLength); ++ii)
            {
                result = isprint(portName[ii]);
            }
        }
        else
        {
            OD_SYSLOG("! (0 < nameLength)");//####
            result = false;
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // Endpoint::CheckEndpointName

#if defined(__APPLE__)
# pragma mark Constructors and destructors
#endif // defined(__APPLE__)

Endpoint::Endpoint(const yarp::os::ConstString & endpointName,
                   const yarp::os::ConstString & hostName,
                   const yarp::os::ConstString & portNumber) :
        _contact(), _handler(NULL), _handlerCreator(NULL), _port(NULL), _isOpen(false)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S3("endpointName = ", endpointName.c_str(), "hostName = ", hostName.c_str(),//####
                 "portNumber = ", portNumber.c_str());//####
    if (CheckEndpointName(endpointName))
    {
        int realPort;

        if (checkHostPort(realPort, portNumber))
        {
            _contact = yarp::os::Contact::byName(endpointName);
            if (checkHostName(_contact, hostName, realPort))
            {
                // Ready to be set up... we have a valid port, and either a blank URI or a valid one.
                _port = new yarp::os::Port();
                if (! _port)
                {
                    OD_SYSLOG_EXIT_THROW_S("Could not create port");//####
                    throw new Exception("Could not create port");
                }
            }
            else
            {
                OD_SYSLOG_EXIT_THROW_S("Bad host name");//####
                throw new Exception("Bad host name");
            }
        }
        else
        {
            OD_SYSLOG_EXIT_THROW_S("Bad port number");//####
            throw new Exception("Bad port number");
        }
    }
    else
    {
        OD_SYSLOG_EXIT_THROW_S("Bad endpoint name");//####
        throw new Exception("Bad endpoint name");
    }
    OD_SYSLOG_EXIT_P(this);//####
} // Endpoint::Endpoint

Endpoint::~Endpoint(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P1("this = ", this);//####
    close();
    OD_SYSLOG_EXIT();//####
} // Endpoint::~Endpoint

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

void Endpoint::close(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P1("this = ", this);//####
    try
    {
        if (isOpen())
        {
            if (_handler)
            {
                _handler->stopProcessing();
            }
            if (_port)
            {
                OD_SYSLOG_S1("about to close, port = ", getName().c_str());//####
                _port->close();
                OD_SYSLOG("close completed.");//####
                OD_SYSLOG("about to unregister port");//####
                if (0 < _contact.getHost().length())
                {
                    yarp::os::Network::unregisterContact(_contact);
                }
                else
                {
                    yarp::os::Network::unregisterName(_contact.getName());
                }
                delete _port;
                _port = NULL;
            }
            _handler = NULL;
            _handlerCreator = NULL;
            _isOpen = false;
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT();//####
} // Endpoint::close

bool Endpoint::open(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P1("this = ", this);//####
    bool result = false;
    
    try
    {
        if (! isOpen())
        {
            if (_port)
            {
                if (0 < _contact.getHost().length())
                {
                    OD_SYSLOG("(0 < _contact.getHost().length())");//####
                    _contact = yarp::os::Network::registerContact(_contact);
#if defined(REPORT_CONTACT_DETAILS)
                    DumpContact("after registerContact", _contact);//####
#endif // defined(REPORT_CONTACT_DETAILS)
                    if (_port->open(_contact))
                    {
                        _isOpen = true;
#if defined(REPORT_CONTACT_DETAILS)
                        DumpContact("after open", _port->where());//####
#endif // defined(REPORT_CONTACT_DETAILS)
                    }
                    else
                    {
                        OD_SYSLOG("Port could not be opened");//####
                    }
                }
                else if (_port->open(_contact.getName()))
                {
                    OD_SYSLOG("(_port->open(_contact.getName()))");//####
                    _isOpen = true;
#if defined(REPORT_CONTACT_DETAILS)
                    DumpContact("after open", _port->where());//####
#endif // defined(REPORT_CONTACT_DETAILS)
                }
                else
                {
                    OD_SYSLOG("Port could not be opened");//####
                }
                OD_SYSLOG_S1("_port->getName = ", _port->getName().c_str());//####
            }
            else
            {
                OD_SYSLOG("! (_port)");//####
            }
        }
        result = isOpen();
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // Endpoint::open

bool Endpoint::setInputHandler(InputHandler & handler)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P1("this = ", this);//####
    OD_SYSLOG_P1("handler = ", &handler);//####
    bool result = false;
    
    try
    {
        if (_handlerCreator)
        {
            OD_SYSLOG("(_handlerCreator)");//####
        }
        else if (_port)
        {
            OD_SYSLOG("(_port)");//####
            if (isOpen())
            {
                OD_SYSLOG("(isOpen())");//####
            }
            else
            {
                _handler = &handler;
                _port->setReader(handler);
                result = true;
            }
        }
        else
        {
            OD_SYSLOG("! (_port)");//####
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // Endpoint::setInputHandler

bool Endpoint::setInputHandlerCreator(InputHandlerCreator & handlerCreator)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P1("this = ", this);//####
    OD_SYSLOG_P1("handlerCreator = ", &handlerCreator);//####
    bool result = false;
    
    try
    {
        if (_handler)
        {
            OD_SYSLOG("(_handler)");//####
        }
        else if (_port)
        {
            if (isOpen())
            {
                OD_SYSLOG("(isOpen())");//####
            }
            else
            {
                _handlerCreator = &handlerCreator;
                _port->setReaderCreator(handlerCreator);
                result = true;
            }
        }
        else
        {
            OD_SYSLOG("! (_port)");//####
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // Endpoint::setInputHandlerCreator

bool Endpoint::setReporter(yarp::os::PortReport & reporter,
                           const bool             andReportNow)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P1("this = ", this);//####
    OD_SYSLOG_P1("reporter = ", &reporter);//####
    OD_SYSLOG_B1("andReportNow = ", andReportNow);//####
    bool result = false;
    
    try
    {
        if (_port)
        {
            _port->setReporter(reporter);
            if (andReportNow)
            {
                _port->getReport(reporter);
            }
            result = true;
        }
        else
        {
            OD_SYSLOG("! (_port)");//####
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // Endpoint::setReporter

bool Endpoint::setTimeout(const float timeout)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P1("this = ", this);//####
    OD_SYSLOG_D1("timeout = ", timeout);//####
    bool result = false;
    
    try
    {
        if (_port)
        {
            result = _port->setTimeout(timeout);
        }
        else
        {
            OD_SYSLOG("! (_port)");//####
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // Endpoint::setTimeout

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

yarp::os::ConstString Endpoint::getName(void)
const
{
    yarp::os::ConstString result;

    try
    {
        if (_port)
        {
            result = _port->getName();
        }
        else
        {
            result = "";
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    return result;
} // Endpoint::getName

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
