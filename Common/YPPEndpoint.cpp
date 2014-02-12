//
//  YPPEndpoint.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-07.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include <yarp/os/Port.h>
#include <yarp/os/impl/Carriers.h>
#include <cctype>
#include <cstdlib>
#include "YPPEndpoint.h"
#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPConfig.h"
#include "YPPException.h"

using yarp::os::Contact;
using yarp::os::Network;
using yarp::os::impl::Carrier;
using yarp::os::impl::Carriers;

static void dumpContact(const char *    tag,//####
                        const Contact & aContact)//####
{//####
    OD_SYSLOG_S4("tag = ", tag, "contact.name = ", aContact.getName().c_str(),//####
                 "contact.host = ", aContact.getHost().c_str(), "contact.carrier = ", aContact.getCarrier().c_str());//####
    OD_SYSLOG_L1("contact.port = ", aContact.getPort());//####
    OD_SYSLOG_S1("contact.toString = ", aContact.toString().c_str());//####
    OD_SYSLOG_B1("contact.isValid = ", aContact.isValid());//####
} // dumpContact

static bool checkEndpointName(const ConstString & name)
{
    bool   result;
    size_t nameLength = name.length();
    
    if (0 < nameLength)
    {
        char firstChar = name[0];
        
        result = ('/' == firstChar);
        for (size_t ii = 1; result && (ii < nameLength); ++ii)
        {
            result = isprint(name[ii]);
        }
    }
    else
    {
        result = false;
    }
    return result;
} // checkEndpointName

static bool checkCarrier(const String & carrierName)
{
    bool result;
    
    if (0 < carrierName.length())
    {
        Carrier * foundCarrier = Carriers::chooseCarrier(carrierName);
        
        if (foundCarrier)
        {
            delete foundCarrier;
            result = true;
        }
        else
        {
            result = false;
        }
    }
    else
    {
        // An empty carrier name will be treated as 'tcp'.
        result = true;
    }
    return result;
} // checkCarrier

static bool checkHostPort(int &               realPort,
                          const ConstString & portNumber)
{
    bool   result = true;
    size_t portLength = portNumber.length();
    
    if (0 < portLength)
    {
        for (size_t ii = 0; result && (ii < portLength); ++ii)
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

static bool checkHostName(Contact &           workingContact,
                          const ConstString & hostName,
                          const int           portNumber,
                          const String &      carrierName)
{
//    dumpContact("enter checkHostName", workingContact);//####
    bool result;
    
    if (0 < hostName.length())
    {
        // Non-empty hostname - check it...
        ConstString ipAddress = Contact::convertHostToIp(hostName);
        
        OD_SYSLOG_S1("ipAddress = ", ipAddress.c_str());//####

        if (0 < carrierName.length())
        {
            workingContact = workingContact.addSocket(carrierName.c_str(), ipAddress, portNumber);
        }
        else
        {
            workingContact = workingContact.addSocket("tcp", ipAddress, portNumber);
        }
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

YarpPlusPlus::Endpoint::Endpoint(const ConstString & endpointName,
                                 const ConstString & hostName,
                                 const ConstString & portNumber,
                                 const String &      carrierName) :
        _handler(NULL), _port(NULL)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S4("endpointName = ", endpointName.c_str(), "hostName = ", hostName.c_str(),//####
                 "portNumber = ", portNumber.c_str(), "carrierName = ", carrierName.c_str());//####
    if (checkEndpointName(endpointName))
    {
        if (checkCarrier(carrierName))
        {
            int realPort;

            if (checkHostPort(realPort, portNumber))
            {
                Contact workingContact = Contact::byName(endpointName);
                
                if (checkHostName(workingContact, hostName, realPort, carrierName))
                {
                    // Ready to be set up... we have a valid port, and either a blank URI or a valid one.
                    _port = new Port();
                    if (0 < hostName.length())
                    {
                        workingContact = Network::registerContact(workingContact);
//                        dumpContact("after registerContact", workingContact);//####
                        if (_port->open(workingContact))
                        {
//                            Contact where = _port->where();//####
                            
//                            dumpContact("after open", where);//####
                        }
                        else
                        {
                            OD_SYSLOG_EXIT_THROW_S("Port could not be opened");//####
                            throw new YarpPlusPlus::Exception("Port could not be opened");
                        }
                    }
                    else if (_port->open(endpointName))
                    {
//                        Contact where = _port->where();//####
                        
//                        dumpContact("after open", where);//####
                    }
                    else
                    {
                        OD_SYSLOG_EXIT_THROW_S("Port could not be opened");//####
                        throw new YarpPlusPlus::Exception("Port could not be opened");
                    }
                }
                else
                {
                    OD_SYSLOG_EXIT_THROW_S("Bad host name");//####
                    throw new YarpPlusPlus::Exception("Bad host name");
                }
            }
            else
            {
                OD_SYSLOG_EXIT_THROW_S("Bad port number");//####
                throw new YarpPlusPlus::Exception("Bad port number");
            }
        }
        else
        {
            OD_SYSLOG_EXIT_THROW_S("Bad carrier name");//####
            throw new YarpPlusPlus::Exception("Bad carrier name");
        }
    }
    else
    {
        OD_SYSLOG_EXIT_THROW_S("Bad endpoint name");//####
        throw new YarpPlusPlus::Exception("Bad endpoint name");
    }
    OD_SYSLOG_EXIT();//####
} // YarpPlusPlus::Endpoint::Endpoint

YarpPlusPlus::Endpoint::~Endpoint(void)
{
    OD_SYSLOG_ENTER();//####
    if (_handler)
    {
        _handler->stopProcessing();
    }
    if (_port)
    {
        _port->close();
        delete _port;
    }
    OD_SYSLOG_EXIT();//####
} // YarpPlusPlus::Endpoint::~Endpoint

ConstString YarpPlusPlus::Endpoint::getName(void)
const
{
    ConstString result;
    
    if (_port)
    {
        result = _port->getName();
    }
    else
    {
        result = "";
    }
    return result;
} // YarpPlusPlus::Endpoint::getName

ConstString YarpPlusPlus::Endpoint::getRandomPortName(void)
{
    ConstString result;
    char *      temp = tempnam(NULL, "port_");
    
    result = temp;
    free(temp);
    return result;
} // YarpPlusPlus::Endpoint::getRandomPortName

void YarpPlusPlus::Endpoint::setInputHandler(InputHandler & handler)
{
    OD_SYSLOG_ENTER();//####
    if (_port)
    {
        _handler = &handler;
        _port->setReader(handler);
    }
    OD_SYSLOG_EXIT();//####
} // YarpPlusPlus::Endpoint::setInputHandler

void YarpPlusPlus::Endpoint::setReporter(PortReport & reporter,
                                         const bool   andReportNow)
{
    OD_SYSLOG_ENTER();//####
    if (_port)
    {
        _port->setReporter(reporter);
        if (andReportNow)
        {
            _port->getReport(reporter);
        }
    }
    OD_SYSLOG_EXIT();//####
} // YarpPlusPlus::Endpoint::setReporter

