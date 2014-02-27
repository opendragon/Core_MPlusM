//
//  YPPEndpoint.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-07.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPEndpoint.h"
#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPException.h"
#include "YPPCommon.h"
#include <yarp/os/Network.h>
#include <cctype>
#include <cstdlib>
#include <cstdio>

using namespace YarpPlusPlus;

#pragma mark Private structures and constants

#pragma mark Local functions

/*! @brief Check the syntax of the given name to make sure that it conforms to YARP conventions.
 @param portName The name to be checked.
 @returns @c true if the name can be used as a YARP port name and @c false otherwise. */
static bool checkEndpointName(const yarp::os::ConstString & portName)
{
    bool result;
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
        result = false;
    }
    return result;
} // checkEndpointName

/*! @brief Check if the given port number is valid.
 @param realPort The numeric value of 'portNumber'.
 @param portNumber The port number as a string to be checked.
 @returns @c true if the port number string is numeric or empty. */
static bool checkHostPort(int &                         realPort,
                          const yarp::os::ConstString & portNumber)
{
    bool result = true;
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
//    dumpContact("enter checkHostName", workingContact);//####
    bool result;
    
    if (0 < hostName.length())
    {
        // Non-empty hostname - check it...
        yarp::os::ConstString ipAddress = yarp::os::Contact::convertHostToIp(hostName);
        
        OD_SYSLOG_S1("ipAddress = ", ipAddress.c_str());//####
        
        workingContact = workingContact.addSocket("tcp", ipAddress, portNumber);
//        dumpContact("after addSocket", workingContact);//####
        result = workingContact.isValid();
    }
    else
    {
        // Empty host name - YARP will use the local machine name.
        result = true;
    }
    return result;
} // checkHostName

#pragma mark Class methods

yarp::os::ConstString Endpoint::getRandomPortName(void)
{
    yarp::os::ConstString result;
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
    char *                temp = tempnam(NULL, "port_");
#pragma clang diagnostic pop
    
    result = temp;
    free(temp);
    return result;
} // Endpoint::getRandomPortName

#pragma mark Constructors and destructors

Endpoint::Endpoint(const yarp::os::ConstString & endpointName,
                   const yarp::os::ConstString & hostName,
                   const yarp::os::ConstString & portNumber) :
        _contact(), _handler(NULL), _handlerCreator(NULL), _port(NULL), _isOpen(false)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S3("endpointName = ", endpointName.c_str(), "hostName = ", hostName.c_str(),//####
                 "portNumber = ", portNumber.c_str());//####
    if (checkEndpointName(endpointName))
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
    OD_SYSLOG_EXIT();//####
} // Endpoint::Endpoint

Endpoint::~Endpoint(void)
{
    OD_SYSLOG_ENTER();//####
    close();
    OD_SYSLOG_EXIT();//####
} // Endpoint::~Endpoint

#pragma mark Actions

void Endpoint::close(void)
{
    OD_SYSLOG_ENTER();//####
    if (isOpen())
    {
        if (_handler)
        {
            _handler->stopProcessing();
        }
        if (_port)
        {
            _port->close();
            _contact = yarp::os::Network::unregisterContact(_contact);
            delete _port;
            _port = NULL;
        }
        _handler = NULL;
        _handlerCreator = NULL;
        _isOpen = false;
    }
    OD_SYSLOG_EXIT();//####
} // Endpoint::close

bool Endpoint::open(void)
{
    OD_SYSLOG_ENTER();//####
    if (! isOpen())
    {
        if (_port)
        {
            OD_SYSLOG_S1("_contact.getHost = ", _contact.getHost().c_str());//####
            if (0 < _contact.getHost().length())
            {
                _contact = yarp::os::Network::registerContact(_contact);
//                        dumpContact("after registerContact", _contact);//####
                if (_port->open(_contact))
                {
                    _isOpen = true;
//                   Contact where = _port->where();//####

//                   dumpContact("after open", where);//####
                }
                else
                {
                    OD_SYSLOG("Port could not be opened");//####
                }
            }
            else if (_port->open(_contact.getName()))
            {
                _isOpen = true;
//                Contact where = _port->where();//####

//                dumpContact("after open", where);//####
            }
            else
            {
                OD_SYSLOG("Port could not be opened");//####
            }
        }
    }
    OD_SYSLOG_EXIT_B(isOpen());//####
    return isOpen();
} // Endpoint::open

bool Endpoint::setInputHandler(InputHandler & handler)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P1("handler = ", &handler);//####
    bool result;
    
    if (_handlerCreator)
    {
        result = false;
    }
    else if (_port)
    {
        OD_SYSLOG("(_port)");//####
        if (isOpen())
        {
            result = false;
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
        result = false;
    }
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // Endpoint::setInputHandler

bool Endpoint::setInputHandlerCreator(InputHandlerCreator & handlerCreator)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P1("handlerCreator = ", &handlerCreator);//####
    bool result;
    
    if (_handler)
    {
        result = false;
    }
    else if (_port)
    {
        OD_SYSLOG("(_port)");//####
        if (isOpen())
        {
            result = false;
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
        result = false;
    }
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // Endpoint::setInputHandlerCreator

bool Endpoint::setReporter(yarp::os::PortReport & reporter,
                           const bool             andReportNow)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P1("reporter = ", &reporter);//####
    OD_SYSLOG_B1("andReportNow = ", andReportNow);//####
    bool result;
    
    if (_port)
    {
        OD_SYSLOG("(_port)");//####
        _port->setReporter(reporter);
        if (andReportNow)
        {
            OD_SYSLOG("(andReportNow)");//####
            _port->getReport(reporter);
        }
        result = true;
    }
    else
    {
        result = false;
    }
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // Endpoint::setReporter

#pragma mark Accessors

yarp::os::ConstString Endpoint::getName(void)
const
{
    yarp::os::ConstString result;
    
    if (_port)
    {
        result = _port->getName();
    }
    else
    {
        result = "";
    }
    return result;
} // Endpoint::getName

#pragma mark Global functions

void YarpPlusPlus::dumpContact(const char *              tag,
                               const yarp::os::Contact & aContact)
{
    OD_SYSLOG_S4("tag = ", tag, "contact.name = ", aContact.getName().c_str(),//####
                 "contact.host = ", aContact.getHost().c_str(), "contact.carrier = ",//####
                 aContact.getCarrier().c_str());//####
    OD_SYSLOG_LL1("contact.port = ", aContact.getPort());//####
    OD_SYSLOG_S1("contact.toString = ", aContact.toString().c_str());//####
    OD_SYSLOG_B1("contact.isValid = ", aContact.isValid());//####
} // dumpContact
