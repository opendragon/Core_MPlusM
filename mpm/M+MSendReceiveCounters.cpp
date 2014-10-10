//--------------------------------------------------------------------------------------------------
//
//  File:       mpm/M+MSendReceiveCounters.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for the send / receive counters.
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
//  Created:    2014-10-09
//
//--------------------------------------------------------------------------------------------------

#include <mpm/M+MSendReceiveCounters.h>

#include <mpm/M+MUtilities.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The class definition for the send / receive counters. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

/*! @brief Add a large value to a dictionary.
 @param dictionary The dictionary to be updated.
 @param tag The tag to associate with the value.
 @param bigValue The value to be added. */
static void addLargeValueToDictionary(yarp::os::Property &          dictionary,
                                      const yarp::os::ConstString & tag,
                                      const int64_t                 bigValue)
{
    yarp::os::Value    stuff;
    yarp::os::Bottle * stuffAsList = stuff.asList();
    
    if (stuffAsList)
    {
        stuffAsList->addInt(static_cast<int>(bigValue >> 32));
        stuffAsList->addInt(static_cast<int>(bigValue));
        dictionary.put(tag, stuff);
    }
} // addLargeValueToDictionary

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

SendReceiveCounters::SendReceiveCounters(const int64_t initialInBytes,
                                         const size_t  initialInMessages,
                                         const int64_t initialOutBytes,
                                         const size_t  initialOutMessages) :
    _inBytes(initialInBytes), _outBytes(initialOutBytes), _inMessages(initialInMessages),
    _outMessages(initialOutMessages)
{
    OD_LOG_ENTER(); //####
    OD_LOG_EXIT_P(this); //####
} // SendReceiveCounters::SendReceiveCounters

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void SendReceiveCounters::addToList(yarp::os::Bottle &            counterList,
                                    const yarp::os::ConstString & channel)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("counterList = ", &counterList); //####
    OD_LOG_S1s("channel = ", channel); //####
    char                 buffer1[DATE_TIME_BUFFER_SIZE];
    char                 buffer2[DATE_TIME_BUFFER_SIZE];
    yarp::os::Property & props = counterList.addDict();

    Utilities::GetDateAndTime(buffer1, sizeof(buffer1), buffer2, sizeof(buffer2));
    props.put(MpM_SENDRECEIVE_CHANNEL_, channel);
    props.put(MpM_SENDRECEIVE_DATE_, buffer1);
    props.put(MpM_SENDRECEIVE_TIME_, buffer2);
    addLargeValueToDictionary(props, MpM_SENDRECEIVE_INBYTES_, _inBytes);
    addLargeValueToDictionary(props, MpM_SENDRECEIVE_INMESSAGES_, _inMessages);
    addLargeValueToDictionary(props, MpM_SENDRECEIVE_OUTBYTES_, _outBytes);
    addLargeValueToDictionary(props, MpM_SENDRECEIVE_OUTMESSAGES_, _outMessages);
    OD_LOG_OBJEXIT(); //####
} // SendReceiveCounters::addToList

void SendReceiveCounters::clearCounters(void)
{
    OD_LOG_OBJENTER(); //####
    _inBytes = _outBytes = 0;
    _inMessages = _outMessages = 0;
    OD_LOG_OBJEXIT(); //####
} // SendReceiveCounters::clearCounters

SendReceiveCounters & SendReceiveCounters::incrementInCounters(const int64_t moreInBytes)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_LL1("moreInBytes = ", moreInBytes); //####
    _inBytes += moreInBytes;
    ++_inMessages;
    OD_LOG_OBJEXIT_P(this); //####
    return *this;
} // SendReceiveCounters::incrementInCounters

SendReceiveCounters & SendReceiveCounters::incrementOutCounters(const int64_t moreOutBytes)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_LL1("moreOutBytes = ", moreOutBytes); //####
    _outBytes += moreOutBytes;
    ++_outMessages;
    OD_LOG_OBJEXIT_P(this); //####
    return *this;
} // SendReceiveCounters::incrementOutCounters

SendReceiveCounters & SendReceiveCounters::operator =(const SendReceiveCounters & other)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("other = ", &other); //####
    _inBytes = other._inBytes;
    _outBytes = other._outBytes;
    _inMessages = other._inMessages;
    _outMessages = other._outMessages;
    OD_LOG_OBJEXIT_P(this); //####
    return *this;
} // SendReceiveCounters::operator =

SendReceiveCounters & SendReceiveCounters::operator +=(const SendReceiveCounters & other)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("other = ", &other); //####
    _inBytes += other._inBytes;
    _outBytes += other._outBytes;
    _inMessages += other._inMessages;
    _outMessages += other._outMessages;
    OD_LOG_OBJEXIT_P(this); //####
    return *this;
} // SendReceiveCounters::operator +=

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
