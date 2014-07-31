//--------------------------------------------------------------------------------------------------
//
//  File:       mpm/M+MAdapterChannel.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for channels to and from adapters.
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
//  Created:    2014-03-18
//
//--------------------------------------------------------------------------------------------------

#include <mpm/M+MAdapterChannel.h>
#include <mpm/M+MBailOut.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The class definition for channels to and from adapters. */
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

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

void AdapterChannel::RelinquishChannel(AdapterChannel * theChannel)
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
            theChannel = NULL;
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
} // AdapterChannel::RelinquishChannel

#if defined(__APPLE__)
# pragma mark Constructors and destructors
#endif // defined(__APPLE__)

AdapterChannel::AdapterChannel(const bool isOutput) :
    inherited(), _name(), _isOutput(isOutput)
{
    OD_LOG_ENTER(); //####
    OD_LOG_EXIT_P(this); //####
} // AdapterChannel::AdapterChannel

AdapterChannel::~AdapterChannel(void)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_OBJEXIT(); //####
} // AdapterChannel::~AdapterChannel

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

void AdapterChannel::close(void)
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
} // AdapterChannel::close

bool AdapterChannel::openWithRetries(const yarp::os::ConstString & theChannelName,
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
            result = inherited::open(theChannelName);
            if (! result)
            {
                if (0 < --retriesLeft)
                {
                    OD_LOG("%%retry%%"); //####
                    yarp::os::Time::delay(retryTime);
                    retryTime *= RETRY_MULTIPLIER;
                }
            }
        }
        while ((! result) && (0 < retriesLeft));
        if (result)
        {
            _name = theChannelName;
            OD_LOG_S1s("_name <- ", _name); //####
        }
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
} // AdapterChannel::openWithRetries

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
