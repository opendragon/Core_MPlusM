//--------------------------------------------------------------------------------------
//
//  File:       MoMeCommon.cpp
//
//  Project:    MoAndMe
//
//  Contains:   The definitions for common functions.
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
//  Created:    2014-03-19
//
//--------------------------------------------------------------------------------------

//#include "MoMeCommon.h"
#include "MoMeBailOut.h"

//#include "ODEnableLogging.h"
#include "ODLogging.h"

#include <ace/Version.h>
#include <cmath>
#include <cstdlib>
#include <ctime>
#include <iostream>
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
#include <yarp/conf/version.h>
#include <yarp/os/Network.h>
#include <yarp/os/Os.h>
#include <yarp/os/Random.h>
#include <yarp/os/Time.h>
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The function and variable definitions for common entities for MoAndMe clients and services. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MoAndMe;
using namespace MoAndMe::Common;
using std::cerr;
using std::cout;
using std::endl;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief Report the version numbers when launching an executable. */
#define CHATTY_START_ /* */

/*! @brief Delay the start of execution to allow channels to clear. */
//#define DELAY_ON_START_ /* */

/*! @brief Report details of the contacts during operations that might change them. */
#define REPORT_CONTACT_DETAILS /* */

/*! @brief The basic time interval for retries. */
static const double kInitialRetryInterval = 0.1;
/*! @brief The retry interval multiplier. */
static const double kRetryMultiplier = 1.2;
#if defined(DELAY_ON_START_)
static const double kInitialDelay = 0.5;
#endif // defined(DELAY_ON_START_)
/*! @brief The maximum number of retries before declaring failure. */
static const int kMaxRetries = 5;

/*! @brief The maximum integer that we wish to use for generated random values. */
static const int kMaxRandom = 123456789;

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

/*! @brief Returns a printable string, even for null strings.
 @param aString The string to be checked.
 @returns The input string, if non-@c NULL, or a fixed string if it is @c NULL. */
static const char * nullOrString(const char * aString)
{
    const char * result;
    
    if (aString)
    {
        result = aString;
    }
    else
    {
        result = "<>";
    }
    return result;
} // nullOrString

#if (defined(__APPLE__) || defined(__linux__))
/*! @brief The signal handler to catch requests to stop the service.
 @param signal The signal being handled. */
static void localCatcher(int signal)
{
# if (! defined(OD_ENABLE_LOGGING))
#  pragma unused(signal)
# endif // ! defined(OD_ENABLE_LOGGING)
    OD_LOG_ENTER();//####
    OD_LOG_LL1("signal = ", signal);//####
    OD_LOG_EXIT_EXIT(1);//####
    yarp::os::exit(1);
} // localCatcher
#endif // defined(__APPLE__) || defined(__linux__)

/*! @brief Set up the signal-handling behaviour so that this thread will catch our signal. */
static void setUpCatcher(void)
{
#if (defined(__APPLE__) || defined(__linux__))
    sigset_t unblocking;
    
    sigemptyset(&unblocking);
    sigaddset(&unblocking, STANDARD_SIGNAL_TO_USE);
    pthread_sigmask(SIG_UNBLOCK, &unblocking, NULL);
    struct sigaction act;
    
    act.sa_handler = localCatcher;
    sigemptyset(&act.sa_mask);
    act.sa_flags = 0;
    sigaction(STANDARD_SIGNAL_TO_USE, &act, NULL);
#endif // defined(__APPLE__) || defined(__linux__)
} // setUpCatcher

/*! @brief Restore the normal signal-handling behaviour. */
static void shutDownCatcher(void)
{
#if (defined(__APPLE__) || defined(__linux__))
    sigset_t blocking;
    
    sigemptyset(&blocking);
    sigaddset(&blocking, STANDARD_SIGNAL_TO_USE);
    pthread_sigmask(SIG_BLOCK, &blocking, NULL);
    struct sigaction act;
    
    act.sa_handler = SIG_DFL;
    sigemptyset(&act.sa_mask);
    act.sa_flags = 0;
    sigaction(STANDARD_SIGNAL_TO_USE, &act, NULL);
#endif // defined(__APPLE__) || defined(__linux__)
} // shutDownCatcher

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

Channel * MoAndMe::AcquireChannel(void)
{
    OD_LOG_ENTER();//####
    Channel * result = new Channel;
    
    OD_LOG_EXIT_P(result);//####
    return result;
} // MoAndMe::AcquireChannel

bool MoAndMe::AddOutputToChannelWithRetries(Channel &                     theChannel,
                                            const yarp::os::ConstString & theChannelToBeAdded)
{
    OD_LOG_ENTER();//####
    OD_LOG_P1("theChannel = ", &theChannel);//####
    OD_LOG_S2("theChannelName = ", theChannel.getName().c_str(), "theChannelToBeAdded = ",//####
              theChannelToBeAdded.c_str());//####
    bool   result = false;
    double retryTime = kInitialRetryInterval;
    int    retriesLeft = kMaxRetries;
    
    setUpCatcher();
    try
    {
        do
        {
            BailOut bailer(&theChannel);
            
            OD_LOG("about to add an output");//####
            result = theChannel.addOutput(theChannelToBeAdded);
            if (! result)
            {
                if (0 < --retriesLeft)
                {
                    OD_LOG("%%retry%%");//####
                    yarp::os::Time::delay(retryTime);
                    retryTime *= kRetryMultiplier;
                }
            }
        }
        while ((! result) && (0 < retriesLeft));
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    shutDownCatcher();
    OD_LOG_EXIT_B(result);//####
    return result;
} // MoAndMe::AddOutputToChannelWithRetries

void MoAndMe::CloseChannel(Channel & theChannel)
{
    OD_LOG_ENTER();//####
    OD_LOG_P1("theChannel = ", &theChannel);//####
    setUpCatcher();
    try
    {
        BailOut bailer(&theChannel);
        
        theChannel.interrupt();
        OD_LOG_S1("about to close, channel = ", theChannel.getName().c_str());//####
        theChannel.close();
        OD_LOG("close completed.");//####
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    shutDownCatcher();
    OD_LOG_EXIT();//####
} // MoAndMe::CloseChannel

void MoAndMe::DumpContact(const char *              tag,
                          const yarp::os::Contact & aContact)
{
    OD_LOG_S4("tag = ", tag, "contact.name = ", aContact.getName().c_str(),//####
              "contact.host = ", aContact.getHost().c_str(), "contact.carrier = ", aContact.getCarrier().c_str());//####
    OD_LOG_LL1("contact.port = ", aContact.getPort());//####
    OD_LOG_S1("contact.toString = ", aContact.toString().c_str());//####
    OD_LOG_B1("contact.isValid = ", aContact.isValid());//####
    cout << "tag = '" << nullOrString(tag) << "', contact.name = '" << nullOrString(aContact.getName().c_str()) <<
            "'" << endl;
    cout << "contact.host = '" << nullOrString(aContact.getHost().c_str()) << "', contact.carrier = '" <<
            nullOrString(aContact.getCarrier().c_str()) << "'" << endl;
    cout << "contact.port = " << aContact.getPort() << endl;
    cout << "contact.toString = '" << nullOrString(aContact.toString().c_str()) << "'" << endl;
    cout << "contact.isValid = " << (aContact.isValid() ? "true" : "false") << endl;
    cout.flush();
} // DumpContact

yarp::os::ConstString MoAndMe::GetRandomChannelName(const char * channelRoot)
{
    OD_LOG_ENTER();//####
    OD_LOG_S1("channelRoot = ", channelRoot);//####
    yarp::os::ConstString result;

    try
    {
        bool         hasLeadingSlash;
        const char * stringToUse;
        size_t       buffLen;
        
        if (channelRoot)
        {
            hasLeadingSlash = ('/' == *channelRoot);
            stringToUse = channelRoot;
            buffLen = strlen(channelRoot);
        }
        else
        {
            hasLeadingSlash = false;
            stringToUse = "_";
            buffLen = 1;
        }
        buffLen += 32; // allow for a big number...
        char * buff = new char[buffLen];
        int    randNumb = yarp::os::Random::uniform(0, kMaxRandom);
        
        if (hasLeadingSlash)
        {
            snprintf(buff, buffLen, "%s%x", stringToUse, randNumb);            
        }
        else
        {
            snprintf(buff, buffLen, "/%s%x", stringToUse, randNumb);
        }
        result = buff;
        delete[] buff;
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_EXIT_S(result.c_str());//####
    return result;
} // Endpoint::GetRandomChannelName

void MoAndMe::Initialize(void)
{
    OD_LOG_ENTER();//####
    try
    {
#if (defined(OD_ENABLE_LOGGING) && defined(MAM_LOG_INCLUDES_YARP_TRACE))
        yarp::os::Network::setVerbosity(1);
#else // ! (defined(OD_ENABLE_LOGGING) && defined(MAM_LOG_INCLUDES_YARP_TRACE))
        yarp::os::Network::setVerbosity(-1);
#endif // ! (defined(OD_ENABLE_LOGGING) && defined(MAM_LOG_INCLUDES_YARP_TRACE))
        double intPart;
        double now = yarp::os::Time::now();
        double fraction = modf(now, &intPart);
        int    seed = static_cast<int>(ceil(fraction * kMaxRandom));
        
#if defined(CHATTY_START_)
        cerr << "Movement And Meaning Version " << MAM_VERSION << ", YARP Version " << YARP_VERSION_STRING <<
                ", ACE Version = " << ACE_VERSION << endl;
#endif // defined(CHATTY_START_)
        OD_LOG_D2("time = ", now, "fraction = ", fraction);//####
        OD_LOG_LL1("seed = ", seed);//####
        yarp::os::Random::seed(seed);
#if defined(DELAY_ON_START_)
        yarp::os::Time::delay(kInitialDelay);
#endif // defined(DELAY_ON_START_)
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_EXIT();//####
} // MoAndMe::Initialize

bool MoAndMe::NetworkConnectWithRetries(const yarp::os::ConstString & sourceName,
                                        const yarp::os::ConstString & destinationName)
{
    OD_LOG_ENTER();//####
    OD_LOG_S2("sourceName = ", sourceName.c_str(), "destinationName = ", destinationName.c_str());//####
    bool   result = false;
    double retryTime = kInitialRetryInterval;
    int    retriesLeft = kMaxRetries;
    
    setUpCatcher();
    try
    {
        do
        {
            BailOut bailer;
            
            OD_LOG("about to connect");//####
            result = yarp::os::Network::connect(sourceName, destinationName);
            if (! result)
            {
                if (0 < --retriesLeft)
                {
                    OD_LOG("%%retry%%");//####
                    yarp::os::Time::delay(retryTime);
                    retryTime *= kRetryMultiplier;
                }
            }
        }
        while ((! result) && (0 < retriesLeft));
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    shutDownCatcher();
    OD_LOG_EXIT_B(result);//####
    return result;
} // MoAndMe::NetworkConnectWithRetries

bool MoAndMe::NetworkDisconnectWithRetries(const yarp::os::ConstString & sourceName,
                                           const yarp::os::ConstString & destinationName)
{
    OD_LOG_ENTER();//####
    OD_LOG_S2("sourceName = ", sourceName.c_str(), "destinationName = ", destinationName.c_str());//####
    bool   result = false;
    double retryTime = kInitialRetryInterval;
    int    retriesLeft = kMaxRetries;
    
    setUpCatcher();
    try
    {
        do
        {
            BailOut bailer;
            
            OD_LOG("about to disconnect");//####
            result = yarp::os::Network::disconnect(sourceName, destinationName);
            if (! result)
            {
                if (0 < --retriesLeft)
                {
                    OD_LOG("%%retry%%");//####
                    yarp::os::Time::delay(retryTime);
                    retryTime *= kRetryMultiplier;
                }
            }
        }
        while ((! result) && (0 < retriesLeft));
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    shutDownCatcher();
    OD_LOG_EXIT_B(result);//####
    return result;
} // MoAndMe::NetworkDisconnectWithRetries

bool MoAndMe::OpenChannelWithRetries(Channel &                     theChannel,
                                     const yarp::os::ConstString & theChannelName)
{
    OD_LOG_ENTER();//####
    OD_LOG_P1("theChannel = ", &theChannel);//####
    OD_LOG_S1("theChannelName = ", theChannelName.c_str());//####
    bool   result = false;
    double retryTime = kInitialRetryInterval;
    int    retriesLeft = kMaxRetries;
    
#if (defined(OD_ENABLE_LOGGING) && defined(MAM_LOG_INCLUDES_YARP_TRACE))
    theChannel.setVerbosity(1);
#else // ! (defined(OD_ENABLE_LOGGING) && defined(MAM_LOG_INCLUDES_YARP_TRACE))
    theChannel.setVerbosity(-1);
#endif // ! (defined(OD_ENABLE_LOGGING) && defined(MAM_LOG_INCLUDES_YARP_TRACE))
    setUpCatcher();
    try
    {
        do
        {
            BailOut bailer(&theChannel);
            
            OD_LOG("about to open");//####
            result = theChannel.open(theChannelName);
            if (! result)
            {
                if (0 < --retriesLeft)
                {
                    OD_LOG("%%retry%%");//####
                    yarp::os::Time::delay(retryTime);
                    retryTime *= kRetryMultiplier;
                }
            }
        }
        while ((! result) && (0 < retriesLeft));
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    shutDownCatcher();
    OD_LOG_EXIT_B(result);//####
    return result;
} // MoAndMe::OpenChannelWithRetries

bool MoAndMe::OpenChannelWithRetries(Channel &           theChannel,
                                     yarp::os::Contact & theContactInfo)
{
    OD_LOG_ENTER();//####
    OD_LOG_P2("theChannel = ", &theChannel, "theContactInfo = ", &theContactInfo);//####
    bool   result = false;
    double retryTime = kInitialRetryInterval;
    int    retriesLeft = kMaxRetries;
    
#if defined(REPORT_CONTACT_DETAILS)
    DumpContact("before open", theContactInfo);//####
#endif // defined(REPORT_CONTACT_DETAILS)
#if (defined(OD_ENABLE_LOGGING) && defined(MAM_LOG_INCLUDES_YARP_TRACE))
    theChannel.setVerbosity(1);
#else // ! (defined(OD_ENABLE_LOGGING) && defined(MAM_LOG_INCLUDES_YARP_TRACE))
    theChannel.setVerbosity(-1);
#endif // ! (defined(OD_ENABLE_LOGGING) && defined(MAM_LOG_INCLUDES_YARP_TRACE))
    setUpCatcher();
    try
    {
        do
        {
            BailOut bailer(&theChannel);
            
            OD_LOG("about to open");//####
            result = theChannel.open(theContactInfo);
            if (! result)
            {
                if (0 < --retriesLeft)
                {
                    OD_LOG("%%retry%%");//####
                    yarp::os::Time::delay(retryTime);
                    retryTime *= kRetryMultiplier;
                }
            }
        }
        while ((! result) && (0 < retriesLeft));
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    shutDownCatcher();
    OD_LOG_EXIT_B(result);//####
    return result;
} // MoAndMe::OpenChannelWithRetries

void MoAndMe::RelinquishChannel(Channel * & theChannel)
{
    OD_LOG_ENTER();//####
    OD_LOG_P1("theChannel = ", theChannel);//####
    setUpCatcher();
    try
    {
        BailOut bailer(theChannel);
        
        delete theChannel;
        theChannel = NULL;
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    shutDownCatcher();
    OD_LOG_EXIT();//####
} // MoAndMe::RelinquishChannel

#if (defined(__APPLE__) || defined(__linux__))
void MoAndMe::SetSignalHandlers(SignalHandler theHandler)
{
    OD_LOG_ENTER();//####
    struct sigaction act;

    act.sa_handler = theHandler;
    sigemptyset(&act.sa_mask);
    act.sa_flags = 0;
# if (SIGHUP != STANDARD_SIGNAL_TO_USE)
    sigaction(SIGHUP, &act, NULL);
# endif // SIGHUP != STANDARD_SIGNAL_TO_USE
# if (SIGINT != STANDARD_SIGNAL_TO_USE)
    sigaction(SIGINT, &act, NULL);
# endif // SIGINT != STANDARD_SIGNAL_TO_USE
# if (SIGQUIT != STANDARD_SIGNAL_TO_USE)
    sigaction(SIGQUIT, &act, NULL);
# endif // SIGQUIT != STANDARD_SIGNAL_TO_USE
# if (SIGUSR1 != STANDARD_SIGNAL_TO_USE)
    sigaction(SIGUSR1, &act, NULL);
# endif // SIGUSR1 != STANDARD_SIGNAL_TO_USE
# if (SIGUSR2 != STANDARD_SIGNAL_TO_USE)
    sigaction(SIGUSR2, &act, NULL);
# endif // SIGUSR2 != STANDARD_SIGNAL_TO_USE
    sigset_t blocking;
    
    sigemptyset(&blocking);
    sigaddset(&blocking, STANDARD_SIGNAL_TO_USE);
    pthread_sigmask(SIG_BLOCK, &blocking, NULL);
    OD_LOG_EXIT();//####
} // MoAndMe::SetSignalHandlers
#endif // defined(__APPLE__) || defined(__linux__)