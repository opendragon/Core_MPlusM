//--------------------------------------------------------------------------------------------------
//
//  File:       mpm/M+MBaseChannel.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for common resources for channels.
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
//  Created:    2014-10-08
//
//--------------------------------------------------------------------------------------------------

#include <mpm/M+MBaseChannel.h>
#include <mpm/M+MBailOut.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for common resources for channels. */
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

/*! @brief Set the IP address for the endpoint.
 @param workingContact The connection information that is to be filled in.
 @param channelName The desired endpoint name.
 @returns @c true if the connection information has been constructed and @c false otherwise. */
static bool setChannelIPAddress(yarp::os::Contact &           workingContact,
                                const yarp::os::ConstString & channelName)
{
#if defined(MpM_ReportContactDetails)
    DumpContactToLog("enter setIPAddress", workingContact); //####
#endif // defined(MpM_ReportContactDetails)
    bool result = false;
    
    try
    {
        yarp::os::Contact     aContact = yarp::os::Network::registerName(channelName);
        yarp::os::ConstString ipAddress = aContact.getHost();
        
        OD_LOG_S1s("ipAddress = ", ipAddress); //####
        yarp::os::Network::unregisterName(channelName);
        workingContact = workingContact.addSocket(CHANNEL_CARRIER_, ipAddress, 0);
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
    return result;
} // setChannelIPAddress

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

void BaseChannel::RelinquishChannel(BaseChannel * theChannel)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P1("theChannel = ", theChannel); //####
    if (theChannel)
    {
#if (! defined(MpM_DontUseTimeouts))
        SetUpCatcher();
#endif // ! defined(MpM_DontUseTimeouts)
        try
        {
#if (! defined(MpM_DontUseTimeouts))
            BailOut bailer(*theChannel, STANDARD_WAIT_TIME);
#endif // ! defined(MpM_DontUseTimeouts)
            
            delete theChannel;
        }
        catch (...)
        {
            OD_LOG("Exception caught"); //####
            throw;
        }
#if (! defined(MpM_DontUseTimeouts))
        ShutDownCatcher();
#endif // ! defined(MpM_DontUseTimeouts)
    }
    OD_LOG_EXIT(); //####
} // BaseChannel::RelinquishChannel

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

BaseChannel::BaseChannel(void) :
    inherited(), _name(), _counters(), _metricsEnabled(false)
{
    OD_LOG_ENTER(); //####
    OD_LOG_EXIT_P(this); //####
} // BaseChannel::BaseChannel

BaseChannel::~BaseChannel(void)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_OBJEXIT(); //####
} // BaseChannel::~BaseChannel

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void BaseChannel::close(void)
{
    OD_LOG_OBJENTER(); //####
#if (! defined(MpM_DontUseTimeouts))
    SetUpCatcher();
#endif // ! defined(MpM_DontUseTimeouts)
    try
    {
#if (! defined(MpM_DontUseTimeouts))
        BailOut bailer(*this, STANDARD_WAIT_TIME);
#endif // ! defined(MpM_DontUseTimeouts)
        
        inherited::interrupt();
        OD_LOG("about to close"); //####
        inherited::close();
        OD_LOG("close completed."); //####
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
#if (! defined(MpM_DontUseTimeouts))
    ShutDownCatcher();
#endif // ! defined(MpM_DontUseTimeouts)
    OD_LOG_OBJEXIT(); //####
} // BaseChannel::close

void BaseChannel::disableMetrics(void)
{
    OD_LOG_OBJENTER(); //####
    _metricsEnabled = false;
    OD_LOG_OBJEXIT(); //####
} // BaseChannel::disableMetrics

void BaseChannel::enableMetrics(void)
{
    OD_LOG_OBJENTER(); //####
    _metricsEnabled = true;
    OD_LOG_OBJEXIT(); //####
} // BaseChannel::enableMetrics

void BaseChannel::getSendReceiveCounters(SendReceiveCounters & counters)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("counters = ", &counters); //####
    counters = _counters;
    OD_LOG_OBJEXIT(); //####
} // BaseChannel::getSendReceiveCounters

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
bool BaseChannel::openWithRetries(const yarp::os::ConstString & theChannelName,
                                  const double                  timeToWait)
{
#if ((! RETRY_LOOPS_USE_TIMEOUTS) && (! defined(OD_ENABLE_LOGGING)))
# if MAC_OR_LINUX_
#  pragma unused(timeToWait)
# endif // MAC_OR_LINUX_
#endif // (! RETRY_LOOPS_USE_TIMEOUTS) && (! defined(OD_ENABLE_LOGGING))
    OD_LOG_OBJENTER(); //####
    OD_LOG_S1s("theChannelName = ", theChannelName); //####
    OD_LOG_D1("timeToWait = ", timeToWait); //####
    bool              result = false;
    yarp::os::Contact aContact = yarp::os::Contact::byName(theChannelName);
    
#if defined(MpM_ReportContactDetails)
    DumpContactToLog("after byName", aContact); //####
#endif // defined(MpM_ReportContactDetails)
    if (setChannelIPAddress(aContact, theChannelName))
    {
        result = openWithRetries(aContact, timeToWait);
    }
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // BaseChannel::openWithRetries
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
bool BaseChannel::openWithRetries(yarp::os::Contact & theContactInfo,
                                  const double        timeToWait)
{
#if ((! RETRY_LOOPS_USE_TIMEOUTS) && (! defined(OD_ENABLE_LOGGING)))
# if MAC_OR_LINUX_
#  pragma unused(timeToWait)
# endif // MAC_OR_LINUX_
#endif // (! RETRY_LOOPS_USE_TIMEOUTS) && (! defined(OD_ENABLE_LOGGING))
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("theContactInfo = ", &theContactInfo); //####
    OD_LOG_D1("timeToWait = ", timeToWait); //####
#if defined(MpM_ReportContactDetails)
    DumpContactToLog("theContactInfo = ", theContactInfo); //####
#endif // defined(MpM_ReportContactDetails)
    bool   result = false;
    double retryTime = INITIAL_RETRY_INTERVAL;
    int    retriesLeft = MAX_RETRIES;
    
#if (defined(OD_ENABLE_LOGGING) && defined(MpM_LogIncludesYarpTrace))
    inherited::setVerbosity(1);
#else // ! (defined(OD_ENABLE_LOGGING) && defined(MpM_LogIncludesYarpTrace))
    inherited::setVerbosity(-1);
#endif // ! (defined(OD_ENABLE_LOGGING) && defined(MpM_LogIncludesYarpTrace))
#if RETRY_LOOPS_USE_TIMEOUTS
    SetUpCatcher();
#endif // RETRY_LOOPS_USE_TIMEOUTS
    try
    {
#if RETRY_LOOPS_USE_TIMEOUTS
        BailOut bailer(*this, timeToWait);
#endif // RETRY_LOOPS_USE_TIMEOUTS
        
        do
        {
            OD_LOG("about to open"); //####
            result = inherited::open(theContactInfo);
            if (! result)
            {
                if (0 < --retriesLeft)
                {
                    OD_LOG("%%retry%%"); //####
                    yarp::os::Time::delay(retryTime);
                    retryTime *= RETRY_MULTIPLIER;
                }
            }
            if (result)
            {
                _name = theContactInfo.getName();
                OD_LOG_S1s("_name <- ", _name); //####
            }
        }
        while ((! result) && (0 < retriesLeft));
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
#if RETRY_LOOPS_USE_TIMEOUTS
    ShutDownCatcher();
#endif // RETRY_LOOPS_USE_TIMEOUTS
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // BaseChannel::openWithRetries
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

void BaseChannel::updateReceiveCounters(const size_t numBytes)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_LL1("numBytes = ", numBytes); //####
    if (_metricsEnabled)
    {
        _counters.incrementInCounters(numBytes);
    }
    OD_LOG_OBJEXIT(); //####
} // BaseChannel::updateReceiveCounters

void BaseChannel::updateSendCounters(const size_t numBytes)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_LL1("numBytes = ", numBytes); //####
    if (_metricsEnabled)
    {
        _counters.incrementOutCounters(numBytes);
    }
    OD_LOG_OBJEXIT(); //####
} // BaseChannel::updateSendCounters

bool BaseChannel::write(yarp::os::Bottle & message)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_S1s("message = ", message.toString()); //####
    size_t messageSize = 0;
    
    if (_metricsEnabled)
    {
        message.toBinary(&messageSize);
    }
    bool result = inherited::write(message);

    if (result && _metricsEnabled)
    {
        updateSendCounters(messageSize);
    }
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // BaseChannel::write

bool BaseChannel::write(yarp::os::Bottle & message,
                        yarp::os::Bottle & reply)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_S1s("message = ", message.toString()); //####
    OD_LOG_P1("reply = ", &reply); //####
    size_t messageSize = 0;
    
    if (_metricsEnabled)
    {
        message.toBinary(&messageSize);
    }
    bool result = inherited::write(message, reply);
    
    if (result && _metricsEnabled)
    {
        size_t replySize = 0;

        reply.toBinary(&replySize);
        updateSendCounters(messageSize);
        updateReceiveCounters(replySize);
    }
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // BaseChannel::write

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
