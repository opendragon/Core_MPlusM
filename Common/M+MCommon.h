//--------------------------------------------------------------------------------------
//
//  File:       M+MCommon.h
//
//  Project:    M+M
//
//  Contains:   The function and variable declarations for common entities for M+M
//              clients and services.
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
//  Created:    2014-02-18
//
//--------------------------------------------------------------------------------------

#if (! defined(MpMCommon_H_))
/*! @brief Header guard. */
# define MpMCommon_H_ /* */

# include "M+MConfig.h"

# include <iostream>
# include <vector>
# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wc++11-extensions"
#  pragma clang diagnostic ignored "-Wdocumentation"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#  pragma clang diagnostic ignored "-Wpadded"
#  pragma clang diagnostic ignored "-Wshadow"
#  pragma clang diagnostic ignored "-Wunused-parameter"
#  pragma clang diagnostic ignored "-Wweak-vtables"
# endif // defined(__APPLE__)
# include <yarp/os/Bottle.h>
# include <yarp/os/ConstString.h>
# include <yarp/os/Contact.h>
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 
 @brief The function and variable declarations for common entities for M+M clients and services. */

/*! @dir Common
 @brief The set of files that implement the M+M framework. */

/*! @namespace MplusM
 @brief The classes that implement the M+M framework. */

/*! @namespace MplusM::Common
 @brief The classes that support the basic functionality of the M+M framework. */

/*! @namespace MplusM::Example
 @brief A set of example classes using features from M+M. */

/*! @namespace MplusM::Parser
 @brief The classes that support parsing of search requests and the generation of SQL @c SELECT strings from the search
 requests. */

/*! @namespace MplusM::Registry
 @brief The classes that support registering and unregistering services. */

/*! @namespace MplusM::RequestCounter
 @brief The classes that support measuring the time for service requests. */

/*! @namespace MplusM::Test
 @brief The classes used for unit testing of the M+M classes. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

/*! @brief The base of the channel name to use for an adapter if not provided. */
# define ADAPTER_PORT_NAME_BASE    MpM_ADAPTER_BASE_NAME

/*! @brief The base of the channel name to use for a client if not provided. */
# define CLIENT_PORT_NAME_BASE     MpM_CLIENT_BASE_NAME

/*! @brief The default name for the root part of a channel name. */
# define DEFAULT_CHANNEL_ROOT      "channel_"

/*! @brief The base of the channel name to use for a service if not provided. */
# define DEFAULT_SERVICE_NAME_BASE MpM_SERVICE_BASE_NAME

/*! @brief The basic time interval for retries. */
# define INITIAL_RETRY_INTERVAL    0.1

/*! @brief The maximum number of retries before declaring failure. */
# define MAX_RETRIES               10

/*! @brief The delay value corresponding to one second of delay. */
# define ONE_SECOND_DELAY          1.0

/*! @brief The retry interval multiplier. */
# define RETRY_MULTIPLIER          1.2

/*! @brief All M+M services maintain contexts for each incoming channel connection. */
# define SERVICES_HAVE_CONTEXTS    /* */

/*! @brief The signal to use for internally-detected timeouts. */
# if (defined(__APPLE__) || defined(__linux__))
#  define STANDARD_SIGNAL_TO_USE   SIGUSR2
# else // (! defined(__APPLE__)) && (! defined(__linux__))
#  define STANDARD_SIGNAL_TO_USE   42
# endif // (! defined(__APPLE__)) && (! defined(__linux__))

/*! @brief The default timeout duration in seconds. */
# define STANDARD_WAIT_TIME        5.0

/*! @brief A simple macro to hold the pieces of a string together. */
# define T_(xx)                    xx

namespace MplusM
{
    namespace CommonX
    {
        /*! @brief The logical connection between a client and a service. */
        typedef yarp::os::Bottle         Package;
        
        /*! @brief A sequence of random numbers. */
        typedef std::vector<double>      DoubleVector;
        
        /*! @brief A sequence of strings. */
        typedef std::vector<std::string> StringVector;
        
        /*! @brief A signal handler. */
        typedef void (*SignalHandler) (int signal);
        
        /*! @brief Dump out a description of the provided connection information to the log.
         @param tag A unique string used to identify the call point for the output.
         @param aContact The connection information to be reported. */
        void DumpContact(const char *              tag,
                         const yarp::os::Contact & aContact);
        
        /*! @brief Generate a random channel name.
         @returns A randomly-generated channel name. */
        yarp::os::ConstString GetRandomChannelName(const char * channelRoot = DEFAULT_CHANNEL_ROOT);
        
        /*! @brief Perform initialization of internal resources.
         @param progName The name of the executing program.
         
         Should be called in the main() function of each application or service. */
        void Initialize(const char * progName);
        
        /*! @brief Connect two channels, using a backoff strategy with retries.
         @param sourceName The name of the source channel.
         @param destinationName The name of the destination channel.
         @returns @c true if the connection was established and @ false otherwise. */
        bool NetworkConnectWithRetries(const yarp::os::ConstString & sourceName,
                                       const yarp::os::ConstString & destinationName);
        
        /*! @brief Disconnect two channels, using a backoff strategy with retries.
         @param sourceName The name of the source channel.
         @param destinationName The name of the destination channel.
         @returns @c true if the connection was removed and @ false otherwise. */
        bool NetworkDisconnectWithRetries(const yarp::os::ConstString & sourceName,
                                          const yarp::os::ConstString & destinationName);
        
        /*! @brief Connect the standard signals to a handler.
         @param theHandler The new handler for the signals. */
        void SetSignalHandlers(SignalHandler theHandler);
        
        /*! @brief Set up the signal-handling behaviour so that this thread will catch our signal. */
        void SetUpCatcher(void);
        
        /*! @brief Restore the normal signal-handling behaviour. */
        void ShutDownCatcher(void);
        
        /*! @brief Perform a busy loop, using yarp::os::Time::yield(). */
#if (defined(__APPLE__) || defined(__linux__))
        void Stall(void) __attribute__((noreturn));
#else // (! defined(__APPLE__)) && (! defined(__linux__))
        void Stall(void);
#endif // (! defined(__APPLE__)) && (! defined(__linux__))
        
    } // Common

    /*! @brief Return the name of a signal.
     @param theSignal The signal of interest.
     @returns A string description of the signal. */
    const char * NameOfSignal(const int theSignal);
    
    /*! @brief Write out a (possibly multi-line) description.
     @param outStream The stream to write to.
     @param heading The text to appear on the first line before the beginning of the description.
     @param description The description, which may contain multiple newlines. */
    void OutputDescription(std::ostream &                outStream,
                           const char *                  heading,
                           const yarp::os::ConstString & description);

} // MplusM

#endif // ! defined(MpMCommon_H_)
