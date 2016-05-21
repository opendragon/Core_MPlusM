//--------------------------------------------------------------------------------------------------
//
//  File:       m+m/m+mEndpoint.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the connection endpoint for an m+m service.
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

#include "m+mEndpoint.hpp"

#include <m+m/m+mBaseInputHandlerCreator.hpp>
#include <m+m/m+mChannelStatusReporter.hpp>
#include <m+m/m+mException.hpp>
#include <m+m/m+mServiceChannel.hpp>
#include <m+m/m+mUtilities.hpp>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the connection endpoint for an m+m service. */
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
 @param[in,out] realPort The numeric value of 'portNumber'.
 @param[in] portNumber The port number as a string to be checked.
 @returns @c true if the port number string is numeric or empty. */
static bool
checkHostPort(int &              realPort,
              const YarpString & portNumber)
{
    ODL_ENTER(); //####
    ODL_P1("realPort = ", &realPort); //####
    ODL_S1s("portNumber = ", portNumber); //####
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
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_EXIT_B(result); //####
    return result;
} // checkHostPort

/*! @brief Set the IP address for the endpoint.
 @param[in,out] workingContact The connection information that is to be filled in.
 @param[in] endpointName The desired endpoint name.
 @param[in] portNumber The port number to be applied to the connection.
 @returns @c true if the connection information has been constructed and @c false otherwise. */
static bool
setEndpointIPAddress(yarp::os::Contact & workingContact,
                     const YarpString &  endpointName,
                     const int           portNumber)
{
    ODL_ENTER(); //####
    ODL_P1("workingContact = ", &workingContact); //####
    ODL_S1s("endpointName = ", endpointName); //####
    ODL_L1("portNumber = ", portNumber); //####
#if defined(MpM_ReportContactDetails)
    DumpContactToLog("enter setEndpointIPAddress", workingContact); //####
#endif // defined(MpM_ReportContactDetails)
    bool result = false;

    try
    {
        yarp::os::Contact aContact = yarp::os::Network::registerName(endpointName);
        YarpString        ipAddress = aContact.getHost();

        ODL_S1s("ipAddress = ", ipAddress); //####
        yarp::os::Network::unregisterName(endpointName);
        workingContact = workingContact.addSocket(CHANNEL_CARRIER_, ipAddress, portNumber);
#if defined(MpM_ReportContactDetails)
        DumpContactToLog("after addSocket", workingContact); //####
#endif // defined(MpM_ReportContactDetails)
        result = workingContact.isValid();
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_EXIT_B(result); //####
    return result;
} // setEndpointIPAddress

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

bool
Endpoint::CheckEndpointName(const YarpString & channelName)
{
    ODL_ENTER(); //####
    ODL_S1s("channelName = ", channelName); //####
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
            ODL_LOG("! (0 < nameLength)"); //####
            result = false;
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_EXIT_B(result); //####
    return result;
} // Endpoint::CheckEndpointName

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

Endpoint::Endpoint(const YarpString & endpointName,
                   const YarpString & portNumber) :
    _channel(NULL), _contact(), _handler(NULL), _handlerCreator(NULL),
    _metricsEnabled(false), _isOpen(false)
{
    ODL_ENTER(); //####
    ODL_S2s("endpointName = ", endpointName, "portNumber = ", portNumber); //####
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
                    ODL_EXIT_THROW_S("Could not create channel"); //####
                    throw new Exception("Could not create channel");
                }
            }
            else
            {
                ODL_EXIT_THROW_S("Bad connection information"); //####
                throw new Exception("Bad connection information");
            }
        }
        else
        {
            ODL_EXIT_THROW_S("Bad port number"); //####
            throw new Exception("Bad port number");
        }
    }
    else
    {
        ODL_EXIT_THROW_S("Bad endpoint name"); //####
        throw new Exception("Bad endpoint name");
    }
    ODL_EXIT_P(this); //####
} // Endpoint::Endpoint

Endpoint::~Endpoint(void)
{
    ODL_OBJENTER(); //####
    close();
    ODL_OBJEXIT(); //####
} // Endpoint::~Endpoint

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void
Endpoint::close(void)
{
    ODL_OBJENTER(); //####
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
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT(); //####
} // Endpoint::close

void
Endpoint::disableMetrics(void)
{
    ODL_OBJENTER(); //####
    _metricsEnabled = false;
    if (_handler)
    {
        _handler->disableMetrics();
    }
    if (_channel)
    {
        _channel->disableMetrics();
    }
    ODL_OBJEXIT(); //####
} // Endpoint::disableMetrics

void
Endpoint::enableMetrics(void)
{
    ODL_OBJENTER(); //####
    if (_handler)
    {
        _handler->enableMetrics();
    }
    if (_channel)
    {
        _channel->enableMetrics();
    }
    _metricsEnabled = true;
    ODL_OBJEXIT(); //####
} // Endpoint::enableMetrics

YarpString
Endpoint::getName(void)
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
        ODL_LOG("Exception caught"); //####
        throw;
    }
    return result;
} // Endpoint::getName

void
Endpoint::getSendReceiveCounters(SendReceiveCounters & counters)
{
    ODL_OBJENTER(); //####
    ODL_P1("counters = ", &counters); //####
    if (_channel)
    {
        _channel->getSendReceiveCounters(counters);
    }
    else
    {
        counters.clearCounters();
    }
    ODL_OBJEXIT(); //####
} // Endpoint::getSendReceiveCounters

bool
Endpoint::open(const double timeToWait)
{
    ODL_OBJENTER(); //####
    ODL_D1("timeToWait = ", timeToWait); //####
    bool result = false;

    try
    {
        if (! isOpen())
        {
            if (_channel)
            {
                ODL_LOG("(_channel)"); //####
#if defined(MpM_ReportContactDetails)
                DumpContactToLog("after registerContact", _contact); //####
#endif // defined(MpM_ReportContactDetails)
                if (0 < _contact.getHost().length())
                {
                    ODL_LOG("(0 < _contact.getHost().length())"); //####
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
                        ODL_LOG("Channel could not be opened"); //####
                    }
                }
                else if (_channel->openWithRetries(_contact.getName(), timeToWait))
                {
                    ODL_LOG("(_channel->openWithRetries(_contact.getName()))"); //####
                    _isOpen = true;
#if defined(MpM_ReportContactDetails)
                    DumpContactToLog("after open", _channel->where()); //####
#endif // defined(MpM_ReportContactDetails)
                }
                else
                {
                    ODL_LOG("Channel could not be opened"); //####
                }
                ODL_S1s("_channel->name = ", _channel->name()); //####
            }
            else
            {
                ODL_LOG("! (_channel)"); //####
            }
        }
        result = isOpen();
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT_B(result); //####
    return result;
} // Endpoint::open

bool
Endpoint::setInputHandler(BaseInputHandler & handler)
{
    ODL_OBJENTER(); //####
    ODL_P1("handler = ", &handler); //####
    bool result = false;

    try
    {
        if (_handlerCreator)
        {
            ODL_LOG("(_handlerCreator)"); //####
        }
        else if (_channel)
        {
            ODL_LOG("(_channel)"); //####
            if (isOpen())
            {
                ODL_LOG("(isOpen())"); //####
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
            ODL_LOG("! (_channel)"); //####
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT_B(result); //####
    return result;
} // Endpoint::setInputHandler

bool
Endpoint::setInputHandlerCreator(BaseInputHandlerCreator & handlerCreator)
{
    ODL_OBJENTER(); //####
    ODL_P1("handlerCreator = ", &handlerCreator); //####
    bool result = false;

    try
    {
        if (_handler)
        {
            ODL_LOG("(_handler)"); //####
        }
        else if (_channel)
        {
            if (isOpen())
            {
                ODL_LOG("(isOpen())"); //####
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
            ODL_LOG("! (_channel)"); //####
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT_B(result); //####
    return result;
} // Endpoint::setInputHandlerCreator

bool
Endpoint::setReporter(ChannelStatusReporter & reporter,
                      const bool              andReportNow)
{
    ODL_OBJENTER(); //####
    ODL_P1("reporter = ", &reporter); //####
    ODL_B1("andReportNow = ", andReportNow); //####
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
            ODL_LOG("! (_channel)"); //####
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT_B(result); //####
    return result;
} // Endpoint::setReporter

void
Endpoint::updateSendCounters(const size_t numBytes)
{
    ODL_OBJENTER(); //####
    ODL_LL1("numBytes = ", numBytes); //####

    if (_channel && _metricsEnabled)
    {
        _channel->updateSendCounters(numBytes);
    }
    ODL_OBJEXIT(); //####
} // Endpoint::updateSendCounters

yarp::os::Contact
Endpoint::where(void)
const
{
    ODL_OBJENTER(); //####
    yarp::os::Contact result;

    if (_channel && isOpen())
    {
        result = _channel->where();
    }
    else
    {
        result = yarp::os::Contact::invalid();
    }
    ODL_OBJEXIT(); //####
    return result;
} // Endpoint::where

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
