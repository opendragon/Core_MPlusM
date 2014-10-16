//--------------------------------------------------------------------------------------------------
//
//  File:       mpm/M+MEndpoint.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for the connection endpoint for an M+M service.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2014 by HPlus Technologies Ltd. and Simon Fraser University.
//
//              All rights reserved. Redistribution and use in source and binary forms, with or
//              without modification, are permitted provided that the following conditions are met:
//                * Redistributions of source code must retain the above copyright notice, this list
//                  of conditions and the following disclaimer.
//                * Redistributions in binary form must reproduce the above copyright notice, this
//                  list of conditions and the following disclaimer in the documentation and/or
//                  other materials provided with the distribution.
//                * Neither the name of the copyright holders nor the names of its contributors may
//                  be used to endorse or promote products derived from this software without
//                  specific prior written permission.
//
//              THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY
//              EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
//              OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT
//              SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
//              INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
//              TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
//              BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
//              CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
//              ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
//              DAMAGE.
//
//  Created:    2014-02-07
//
//--------------------------------------------------------------------------------------------------

#include <mpm/M+MEndpoint.h>
#include <mpm/M+MBaseInputHandlerCreator.h>
#include <mpm/M+MChannelStatusReporter.h>
#include <mpm/M+MException.h>
#include <mpm/M+MServiceChannel.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The class definition for the connection endpoint for an M+M service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

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
                result = (0 != isdigit(portNumber[ii]));
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
        OD_LOG("Exception caught"); //####
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
#if defined(MpM_ReportContactDetails)
    DumpContactToLog("enter checkHostName", workingContact); //####
#endif // defined(MpM_ReportContactDetails)
    bool result = false;
    
    try
    {
        if (0 < hostName.length())
        {
            // Non-empty hostname - check it...
            yarp::os::ConstString ipAddress(yarp::os::Contact::convertHostToIp(hostName.c_str()));
            
            OD_LOG_S1s("ipAddress = ", ipAddress); //####
            workingContact = workingContact.addSocket(SERVICE_CHANNEL_CARRIER_, ipAddress,
                                                      portNumber);
#if defined(MpM_ReportContactDetails)
            DumpContactToLog("after addSocket", workingContact); //####
#endif // defined(MpM_ReportContactDetails)
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
        OD_LOG("Exception caught"); //####
        throw;
    }
    return result;
} // checkHostName

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

bool Endpoint::CheckEndpointName(const yarp::os::ConstString & channelName)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("channelName = ", channelName); //####
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
                result = (0 != isprint(channelName[ii]));
            }
        }
        else
        {
            OD_LOG("! (0 < nameLength)"); //####
            result = false;
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // Endpoint::CheckEndpointName

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

Endpoint::Endpoint(const yarp::os::ConstString & endpointName,
                   const yarp::os::ConstString & portNumber) :
    _channel(NULL), _contact(), _handler(NULL), _handlerCreator(NULL), _isOpen(false)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S2s("endpointName = ", endpointName, "portNumber = ", portNumber); //####
    if (CheckEndpointName(endpointName))
    {
        int realPort;
        
        if (checkHostPort(realPort, portNumber))
        {
            _contact = yarp::os::Contact::byName(endpointName);
#if defined(MpM_ReportContactDetails)
            DumpContactToLog("after byName", _contact); //####
#endif // defined(MpM_ReportContactDetails)
            if (checkHostName(_contact, STANDARD_HOST_NAME, realPort))
            {
                // Ready to be set up... we have a valid port, and either a blank URI or a valid one.
                _channel = new ServiceChannel;
                if (! _channel)
                {
                    OD_LOG_EXIT_THROW_S("Could not create channel"); //####
                    throw new Exception("Could not create channel");
                }
            }
            else
            {
                OD_LOG_EXIT_THROW_S("Bad host name"); //####
                throw new Exception("Bad host name");
            }
        }
        else
        {
            OD_LOG_EXIT_THROW_S("Bad port number"); //####
            throw new Exception("Bad port number");
        }
    }
    else
    {
        OD_LOG_EXIT_THROW_S("Bad endpoint name"); //####
        throw new Exception("Bad endpoint name");
    }
    OD_LOG_EXIT_P(this); //####
} // Endpoint::Endpoint

Endpoint::~Endpoint(void)
{
    OD_LOG_OBJENTER(); //####
    close();
    OD_LOG_OBJEXIT(); //####
} // Endpoint::~Endpoint

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void Endpoint::close(void)
{
    OD_LOG_OBJENTER(); //####
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
#if defined(MpM_DoExplicitClose)
                _channel->close();
#endif // defined(MpM_DoExplicitClose)
                BaseChannel::RelinquishChannel(_channel);
                _channel = NULL;
            }
            _handler = NULL;
            _handlerCreator = NULL;
            _isOpen = false;
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT(); //####
} // Endpoint::close

yarp::os::ConstString Endpoint::getName(void)
const
{
    yarp::os::ConstString result;
    
    try
    {
        if (_channel)
        {
            result = _channel->name();
        }
        else
        {
            result = "";
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    return result;
} // Endpoint::getName

void Endpoint::getSendReceiveCounters(SendReceiveCounters & counters)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("counters = ", &counters); //####
    if (_channel)
    {
        _channel->getSendReceiveCounters(counters);
    }
    else
    {
        counters.clearCounters();        
    }
    OD_LOG_OBJEXIT(); //####
} // Endpoint::getSendReceiveCounters

bool Endpoint::open(const double timeToWait)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_D1("timeToWait = ", timeToWait); //####
    bool result = false;
    
    try
    {
        if (! isOpen())
        {
            if (_channel)
            {
                if (0 < _contact.getHost().length())
                {
                    OD_LOG("(0 < _contact.getHost().length())"); //####
                    _contact = yarp::os::Network::registerContact(_contact);
#if defined(MpM_ReportContactDetails)
                    DumpContactToLog("after registerContact", _contact); //####
#endif // defined(MpM_ReportContactDetails)
                    if (_channel->openWithRetries(_contact, timeToWait))
                    {
                        _isOpen = true;
#if defined(MpM_ReportContactDetails)
                        DumpContactToLog("after open", _channel->where()); //####
#endif // defined(MpM_ReportContactDetails)
                    }
                    else
                    {
                        OD_LOG("Channel could not be opened"); //####
                    }
                }
                else if (_channel->openWithRetries(_contact.getName(), timeToWait))
                {
                    OD_LOG("(_channel->openWithRetries(_contact.getName()))"); //####
                    _isOpen = true;
#if defined(MpM_ReportContactDetails)
                    DumpContactToLog("after open", _channel->where()); //####
#endif // defined(MpM_ReportContactDetails)
                }
                else
                {
                    OD_LOG("Channel could not be opened"); //####
                }
                OD_LOG_S1s("_channel->name = ", _channel->name()); //####
            }
            else
            {
                OD_LOG("! (_channel)"); //####
            }
        }
        result = isOpen();
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // Endpoint::open

bool Endpoint::setInputHandler(BaseInputHandler & handler)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("handler = ", &handler); //####
    bool result = false;
    
    try
    {
        if (_handlerCreator)
        {
            OD_LOG("(_handlerCreator)"); //####
        }
        else if (_channel)
        {
            OD_LOG("(_channel)"); //####
            if (isOpen())
            {
                OD_LOG("(isOpen())"); //####
            }
            else
            {
                _handler = &handler;
                _handler->setChannel(_channel);
                _channel->setReader(handler);
                result = true;
            }
        }
        else
        {
            OD_LOG("! (_channel)"); //####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // Endpoint::setInputHandler

bool Endpoint::setInputHandlerCreator(BaseInputHandlerCreator & handlerCreator)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("handlerCreator = ", &handlerCreator); //####
    bool result = false;
    
    try
    {
        if (_handler)
        {
            OD_LOG("(_handler)"); //####
        }
        else if (_channel)
        {
            if (isOpen())
            {
                OD_LOG("(isOpen())"); //####
            }
            else
            {
                _handlerCreator = &handlerCreator;
                _handlerCreator->setChannel(_channel);
                _channel->setReaderCreator(handlerCreator);
                result = true;
            }
        }
        else
        {
            OD_LOG("! (_channel)"); //####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // Endpoint::setInputHandlerCreator

bool Endpoint::setReporter(ChannelStatusReporter & reporter,
                           const bool              andReportNow)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("reporter = ", &reporter); //####
    OD_LOG_B1("andReportNow = ", andReportNow); //####
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
            OD_LOG("! (_channel)"); //####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // Endpoint::setReporter

void Endpoint::updateSendCounters(const size_t numBytes)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_LL1("numBytes = ", numBytes); //####
    
    if (_channel)
    {
        _channel->updateSendCounters(numBytes);
    }
    OD_LOG_OBJEXIT(); //####
} // Endpoint::updateSendCounters

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
