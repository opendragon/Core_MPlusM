//--------------------------------------------------------------------------------------
//
//  File:       M+MEndpoint.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for the connection endpoint for a M+M service.
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

#include "M+MEndpoint.h"
#include "M+MChannelStatusReporter.h"
#include "M+MException.h"
#include "M+MInputHandlerCreator.h"
#include "M+MServiceChannel.h"

//#include "ODEnableLogging.h"
#include "ODLogging.h"

#include <cstdlib>
#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wc++11-extensions"
# pragma clang diagnostic ignored "-Wdocumentation"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# pragma clang diagnostic ignored "-Wpadded"
# pragma clang diagnostic ignored "-Wshadow"
# pragma clang diagnostic ignored "-Wunused-parameter"
# pragma clang diagnostic ignored "-Wweak-vtables"
#endif // defined(__APPLE__)
#include <yarp/os/Network.h>
#include <yarp/os/Time.h>
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The class definition for the connection endpoint for a M+M service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using std::cout;
using std::endl;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

#if defined(OD_ENABLE_LOGGING)
# define REPORT_CONTACT_DETAILS /* Report details of the contacts during operations that might change them. */
#endif // defined(OD_ENABLE_LOGGING)

/*! @brief The carrier type to be used for service connections. */
#define SERVICE_CHANNEL_CARRIER_ "tcp"

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
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    return result;
} // checkHostPort

/*! @brief Check if the given host name is valid.
 @param workingContact The connection information that is to be filled in.
 @param hostName The host name that is to be validated.
 @param portNumber The port number to be applied to the connection.
 @returns @c true if the connection information has been constructed and @c false otherwise. */
static bool checkHostName(yarp::os::Contact &           workingContact,
                          const yarp::os::ConstString & hostName,
                          const int                     portNumber)
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
            yarp::os::ConstString ipAddress(yarp::os::Contact::convertHostToIp(hostName.c_str()));
            
            OD_LOG_S1("ipAddress = ", ipAddress.c_str());//####
            workingContact = workingContact.addSocket(SERVICE_CHANNEL_CARRIER_, ipAddress, portNumber);
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
        OD_LOG("Exception caught");//####
        throw;
    }
    return result;
} // checkHostName

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

bool Endpoint::CheckEndpointName(const yarp::os::ConstString & channelName)
{
    OD_LOG_ENTER();//####
    OD_LOG_S1("channelName = ", channelName.c_str());//####
    bool result = false;

    try
    {
        size_t nameLength = channelName.length();
        
        if (0 < nameLength)
        {
            char firstChar = channelName[0];
            
            result = ('/' == firstChar);
            for (size_t ii = 1; result && (ii < nameLength); ++ii)
            {
                result = isprint(channelName[ii]);
            }
        }
        else
        {
            OD_LOG("! (0 < nameLength)");//####
            result = false;
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_EXIT_B(result);//####
    return result;
} // Endpoint::CheckEndpointName

#if defined(__APPLE__)
# pragma mark Constructors and destructors
#endif // defined(__APPLE__)

Endpoint::Endpoint(const yarp::os::ConstString & endpointName,
                   const yarp::os::ConstString & hostName,
                   const yarp::os::ConstString & portNumber) :
        _channel(NULL), _contact(), _handler(NULL), _handlerCreator(NULL), _isOpen(false)
{
    OD_LOG_ENTER();//####
    OD_LOG_S3("endpointName = ", endpointName.c_str(), "hostName = ", hostName.c_str(),//####
              "portNumber = ", portNumber.c_str());//####
    if (CheckEndpointName(endpointName))
    {
        int realPort;

        if (checkHostPort(realPort, portNumber))
        {
            _contact = yarp::os::Contact::byName(endpointName);
#if defined(REPORT_CONTACT_DETAILS)
            DumpContact("after byName", _contact);//####
#endif // defined(REPORT_CONTACT_DETAILS)
            if (checkHostName(_contact, hostName, realPort))
            {
                // Ready to be set up... we have a valid port, and either a blank URI or a valid one.
                _channel = new ServiceChannel;
                if (! _channel)
                {
                    OD_LOG_EXIT_THROW_S("Could not create channel");//####
                    throw new Exception("Could not create channel");
                }
            }
            else
            {
                OD_LOG_EXIT_THROW_S("Bad host name");//####
                throw new Exception("Bad host name");
            }
        }
        else
        {
            OD_LOG_EXIT_THROW_S("Bad port number");//####
            throw new Exception("Bad port number");
        }
    }
    else
    {
        OD_LOG_EXIT_THROW_S("Bad endpoint name");//####
        throw new Exception("Bad endpoint name");
    }
    OD_LOG_EXIT_P(this);//####
} // Endpoint::Endpoint

Endpoint::~Endpoint(void)
{
    OD_LOG_OBJENTER();//####
    close();
    OD_LOG_OBJEXIT();//####
} // Endpoint::~Endpoint

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

void Endpoint::close(void)
{
    OD_LOG_OBJENTER();//####
    try
    {
        if (isOpen())
        {
            if (_handler)
            {
                _handler->stopProcessing();
            }
            if (_channel)
            {
                if (0 < _contact.getHost().length())
                {
                    yarp::os::Network::unregisterContact(_contact);
                }
                else
                {
                    yarp::os::Network::unregisterName(_contact.getName());
                }
#if defined(MpM_DO_EXPLICIT_CLOSE)
                _channel->close();
#endif // defined(MpM_DO_EXPLICIT_CLOSE)
                ServiceChannel::RelinquishChannel(_channel);
            }
            _handler = NULL;
            _handlerCreator = NULL;
            _isOpen = false;
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT();//####
} // Endpoint::close

bool Endpoint::open(void)
{
    OD_LOG_OBJENTER();//####
    bool result = false;
    
    try
    {
        if (! isOpen())
        {
            if (_channel)
            {
                if (0 < _contact.getHost().length())
                {
                    OD_LOG("(0 < _contact.getHost().length())");//####
                    _contact = yarp::os::Network::registerContact(_contact);
#if defined(REPORT_CONTACT_DETAILS)
                    DumpContact("after registerContact", _contact);//####
#endif // defined(REPORT_CONTACT_DETAILS)
                    if (_channel->openWithRetries(_contact))
                    {
                        _isOpen = true;
#if defined(REPORT_CONTACT_DETAILS)
                        DumpContact("after open", _channel->where());//####
#endif // defined(REPORT_CONTACT_DETAILS)
                    }
                    else
                    {
                        OD_LOG("Channel could not be opened");//####
                    }
                }
                else if (_channel->openWithRetries(_contact.getName()))
                {
                    OD_LOG("(_channel->openWithRetries(_contact.getName()))");//####
                    _isOpen = true;
#if defined(REPORT_CONTACT_DETAILS)
                    DumpContact("after open", _channel->where());//####
#endif // defined(REPORT_CONTACT_DETAILS)
                }
                else
                {
                    OD_LOG("Channel could not be opened");//####
                }
                OD_LOG_S1("_channel->getName = ", _channel->getName().c_str());//####
            }
            else
            {
                OD_LOG("! (_channel)");//####
            }
        }
        result = isOpen();
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT_B(result);//####
    return result;
} // Endpoint::open

bool Endpoint::setInputHandler(InputHandler & handler)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_P1("handler = ", &handler);//####
    bool result = false;
    
    try
    {
        if (_handlerCreator)
        {
            OD_LOG("(_handlerCreator)");//####
        }
        else if (_channel)
        {
            OD_LOG("(_channel)");//####
            if (isOpen())
            {
                OD_LOG("(isOpen())");//####
            }
            else
            {
                _handler = &handler;
                _channel->setReader(handler);
                result = true;
            }
        }
        else
        {
            OD_LOG("! (_channel)");//####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT_B(result);//####
    return result;
} // Endpoint::setInputHandler

bool Endpoint::setInputHandlerCreator(InputHandlerCreator & handlerCreator)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_P1("handlerCreator = ", &handlerCreator);//####
    bool result = false;
    
    try
    {
        if (_handler)
        {
            OD_LOG("(_handler)");//####
        }
        else if (_channel)
        {
            if (isOpen())
            {
                OD_LOG("(isOpen())");//####
            }
            else
            {
                _handlerCreator = &handlerCreator;
                _channel->setReaderCreator(handlerCreator);
                result = true;
            }
        }
        else
        {
            OD_LOG("! (_channel)");//####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT_B(result);//####
    return result;
} // Endpoint::setInputHandlerCreator

bool Endpoint::setReporter(ChannelStatusReporter & reporter,
                           const bool              andReportNow)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_P1("reporter = ", &reporter);//####
    OD_LOG_B1("andReportNow = ", andReportNow);//####
    bool result = false;
    
    try
    {
        if (_channel)
        {
            _channel->setReporter(reporter);
            if (andReportNow)
            {
                _channel->getReport(reporter);
            }
            result = true;
        }
        else
        {
            OD_LOG("! (_channel)");//####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT_B(result);//####
    return result;
} // Endpoint::setReporter

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

yarp::os::ConstString Endpoint::getName(void)
const
{
    yarp::os::ConstString result;

    try
    {
        if (_channel)
        {
            result = _channel->getName();
        }
        else
        {
            result = "";
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    return result;
} // Endpoint::getName

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
