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
//  Copyright:  (c) 2014 by H Plus Technologies Ltd. and Simon Fraser University.
//
//              All rights reserved. Redistribution and use in source and binary forms, with or
//              without modification, are permitted provided that the following conditions are met:
//                * Redistributions of source code must retain the above copyright notice, this list
//                  of conditions and the following disclaimer.
//                * Redistributions in binary form must reproduce the above copyright notice, this
//                  list of conditions and the following disclaimer in the documentation and / or
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
#include <mpm/M+MUtilities.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the connection endpoint for an M+M service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

/*! @brief Check if the given port number is valid.
 @param realPort The numeric value of 'portNumber'.
 @param portNumber The port number as a string to be checked.
 @returns @c true if the port number string is numeric or empty. */
static bool checkHostPort(int &              realPort,
                          const YarpString & portNumber)
{
	OD_LOG_ENTER(); //####
	OD_LOG_P1("realPort = ", &realPort); //####
	OD_LOG_S1s("portNumber = ", portNumber); //####
    bool result = true;
    
    try
    {
        size_t portLength = portNumber.length();
        
        if (0 < portLength)
        {
            if (result)
            {
                const char * startPtr = portNumber.c_str();
                char *       endPtr;
                int          realPort = strtol(startPtr, &endPtr, 10);
                
                if ((startPtr == endPtr) || *endPtr ||
                    (! Utilities::ValidPortNumber(realPort)))
                {
                    result = false;
                }
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
	OD_LOG_EXIT_B(result); //####
    return result;
} // checkHostPort

/*! @brief Set the IP address for the endpoint.
 @param workingContact The connection information that is to be filled in.
 @param endpointName The desired endpoint name.
 @param portNumber The port number to be applied to the connection.
 @returns @c true if the connection information has been constructed and @c false otherwise. */
static bool setEndpointIPAddress(yarp::os::Contact & workingContact,
                                 const YarpString &  endpointName,
                                 const int           portNumber)
{
	OD_LOG_ENTER(); //####
	OD_LOG_P1("workingContact = ", &workingContact); //####
	OD_LOG_S1s("endpointName = ", endpointName); //####
	OD_LOG_L1("portNumber = ", portNumber); //####
#if defined(MpM_ReportContactDetails)
    DumpContactToLog("enter setEndpointIPAddress", workingContact); //####
#endif // defined(MpM_ReportContactDetails)
    bool result = false;
    
    try
    {
        yarp::os::Contact aContact = yarp::os::Network::registerName(endpointName);
        YarpString        ipAddress = aContact.getHost();
        
        OD_LOG_S1s("ipAddress = ", ipAddress); //####
        yarp::os::Network::unregisterName(endpointName);
        workingContact = workingContact.addSocket(CHANNEL_CARRIER_, ipAddress, portNumber);
#if defined(MpM_ReportContactDetails)
        DumpContactToLog("after addSocket", workingContact); //####
#endif // defined(MpM_ReportContactDetails)
        result = workingContact.isValid();
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
	OD_LOG_EXIT_B(result); //####
    return result;
} // setEndpointIPAddress

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

bool Endpoint::CheckEndpointName(const YarpString & channelName)
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

Endpoint::Endpoint(const YarpString & endpointName,
                   const YarpString & portNumber) :
    _channel(NULL), _contact(), _handler(NULL), _handlerCreator(NULL), _metricsEnabled(false),
    _isOpen(false)
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
            if (setEndpointIPAddress(_contact, endpointName, realPort))
            {
                // Ready to be set up... we have a valid port, and a valid IP address.
                _channel = new ServiceChannel;
                if (! _channel)
                {
                    OD_LOG_EXIT_THROW_S("Could not create channel"); //####
                    throw new Exception("Could not create channel");
                }
            }
            else
            {
                OD_LOG_EXIT_THROW_S("Bad connection information"); //####
                throw new Exception("Bad connection information");
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

void Endpoint::disableMetrics(void)
{
    OD_LOG_OBJENTER(); //####
    _metricsEnabled = false;
    if (_handler)
    {
        _handler->disableMetrics();
    }
    if (_channel)
    {
        _channel->disableMetrics();
    }
    OD_LOG_OBJEXIT(); //####
} // Endpoint::disableMetrics

void Endpoint::enableMetrics(void)
{
    OD_LOG_OBJENTER(); //####
    if (_handler)
    {
        _handler->enableMetrics();
    }
    if (_channel)
    {
        _channel->enableMetrics();
    }
    _metricsEnabled = true;
    OD_LOG_OBJEXIT(); //####
} // Endpoint::enableMetrics

YarpString Endpoint::getName(void)
const
{
    YarpString result;
    
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
				OD_LOG("(_channel)"); //!!!!
#if defined(MpM_ReportContactDetails)
				DumpContactToLog("after registerContact", _contact); //####
#endif // defined(MpM_ReportContactDetails)
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
    
    if (_channel && _metricsEnabled)
    {
        _channel->updateSendCounters(numBytes);
    }
    OD_LOG_OBJEXIT(); //####
} // Endpoint::updateSendCounters

yarp::os::Contact Endpoint::where(void)
const
{
    OD_LOG_OBJENTER(); //####
    yarp::os::Contact result;
    
    if (_channel && isOpen())
    {
        result = _channel->where();
    }
    else
    {
        result = yarp::os::Contact::invalid();
    }
    OD_LOG_OBJEXIT(); //####
    return result;
} // Endpoint::where

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
