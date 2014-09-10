//--------------------------------------------------------------------------------------------------
//
//  File:       mpm/M+MCommon.h
//
//  Project:    M+M
//
//  Contains:   The function and variable declarations for common entities for M+M clients and
//              services.
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
//  Created:    2014-02-18
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMCommon_H_))
# define MpMCommon_H_ /* Header guard */

# include <mpm/M+MConfig.h>

# if (! defined(MAC_OR_LINUX_))
/*! @brief @c TRUE if non-Windows, @c FALSE if Windows. */
#  define MAC_OR_LINUX_ (defined(__APPLE__) || defined(__linux__))
# endif // ! defined(MAC_OR_LINUX_)

/*! @brief @c TRUE if NetworkBase::checkNetwork() can be trusted and @c FALSE otherwise. */
# define CheckNetworkWorks_ MAC_OR_LINUX_

# include <cctype>
# include <csignal>
# include <cstdlib>
# include <cstring>
# include <iostream>
# include <list>
# include <map>
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
#  pragma clang diagnostic ignored "-Wmissing-noreturn"
#  pragma clang diagnostic ignored "-Wsign-conversion"
#  pragma clang diagnostic ignored "-Wunused-private-field"
#  pragma clang diagnostic ignored "-Wunreachable-code"
#  pragma clang diagnostic ignored "-Wshorten-64-to-32"
# endif // defined(__APPLE__)
# include <yarp/os/all.h>
# if MAC_OR_LINUX_
#  include <yarp/os/impl/Logger.h>
# endif // MAC_OR_LINUX_
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 
 @brief The function and variable declarations for common entities for M+M clients and services. */

/*! @dir /mpm
 @brief The set of files that implement the M+M framework. */

/*! @dir /CommonTests
 @brief The set of files that provide test cases for the M+M framework. */

/*! @dir /ParserTest
 @brief The set of files that support testing the parsing of search requests. */

/*! @namespace MplusM
 @brief The classes that implement the M+M framework. */

/*! @namespace MplusM::Common
 @brief The classes that support the basic functionality of the M+M framework. */

/*! @namespace MplusM::Example
 @brief A set of example classes using features from M+M. */

/*! @namespace MplusM::Parser
 @brief The classes that support parsing of search requests and the generation of SQL @c SELECT
 strings from the search requests. */

/*! @namespace MplusM::Registry
 @brief The classes that support registering and unregistering services. */

/*! @namespace MplusM::RequestCounter
 @brief The classes that support measuring the time for service requests. */

/*! @namespace MplusM::Test
 @brief The classes used for unit testing of the M+M classes. */

/*! @namespace MplusM::Utilities
 @brief The classes that extend the basic functionality of the M+M framework. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

/*! @brief The base of the channel name to use for an adapter if not provided. */
# define ADAPTER_PORT_NAME_BASE    MpM_ADAPTER_BASE_NAME

/*! @brief The time between client announcements of associated channels. */
# define ANNOUNCE_INTERVAL         (17.1 * ONE_SECOND_DELAY)

/*! @brief The base of the channel name to use for a client if not provided. */
# define CLIENT_PORT_NAME_BASE     MpM_CLIENT_BASE_NAME

/*! @brief The size of the buffer used to display the date or the time. */
# define DATE_TIME_BUFFER_SIZE     20

/*! @brief The default name for the root part of a channel name. */
# define DEFAULT_CHANNEL_ROOT      "channel_"

/*! @brief The base of the channel name to use for a service if not provided. */
# define DEFAULT_SERVICE_NAME_BASE MpM_SERVICE_BASE_NAME

/*! @brief The prefix string to use for internal channels. */
# define HIDDEN_CHANNEL_PREFIX     "/$$$_"

/*! @brief The basic time interval for retries. */
# define INITIAL_RETRY_INTERVAL    (0.11 * ONE_SECOND_DELAY)

/*! @brief The base of the channel name to use for an input if not provided. */
# define INPUT_PORT_NAME_BASE      MpM_INPUT_BASE_NAME

/*! @brief The maximum number of retries before declaring failure, if not using timeouts. */
# define MAX_RETRIES               7

/*! @brief The delay value corresponding to one second of delay. */
# define ONE_SECOND_DELAY          1.0

/*! @brief The base of the channel name to use for an output if not provided. */
# define OUTPUT_PORT_NAME_BASE     MpM_OUTPUT_BASE_NAME

/*! @brief The time between checking for 'stale' registry entries. */
# define PING_CHECK_INTERVAL       (7.3 * ONE_SECOND_DELAY)

/*! @brief The number of missing pings before a service is declared 'dead'. */
# define PING_COUNT_MAX            5

/*! @brief The time between ping requests that a service should use. */
# define PING_INTERVAL             (9.7 * ONE_SECOND_DELAY)

/*! @brief The retry interval multiplier. */
# define RETRY_MULTIPLIER          1.21

/*! @brief The hostname to use to refer to the machine on which we are executing. */
# define STANDARD_HOST_NAME        "127.0.0.1"/*"localhost"*/

/*! @brief The standard command-line options. */
# define STANDARD_OPTIONS          "jt"

/*! @brief A TAB character. */
# define CHAR_TAB                  "\t"

/*! @brief A NEWLINE character. */
# define CHAR_NEWLINE              "\n"

/*! @brief A BACKSLASH character. */
# define CHAR_BACKSLASH            "\\"

/*! @brief A DOUBLEQUOTE character. */
# define CHAR_DOUBLEQUOTE          "\""

/*! @brief The signal to use for internally-detected timeouts. */
# if MAC_OR_LINUX_
#  define STANDARD_SIGNAL_TO_USE   SIGUSR2
# else // ! MAC_OR_LINUX_
#  define STANDARD_SIGNAL_TO_USE   42
# endif // ! MAC_OR_LINUX_

/*! @brief The default timeout duration in seconds. */
# define STANDARD_WAIT_TIME        (5.3 * ONE_SECOND_DELAY)

/*! @brief A simple macro to hold the pieces of a string together. */
# define T_(xx)                    xx

/*! @brief @c TRUE if retry loops use timeouts and @c FALSE otherwise. */
# if defined(MpM_UseTimeoutsInRetryLoops)
#  if defined(MpM_DontUseTimeouts)
#   define RETRY_LOOPS_USE_TIMEOUTS FALSE
#  else  // ! defined(MpM_DontUseTimeouts)
#   define RETRY_LOOPS_USE_TIMEOUTS TRUE
#  endif  // ! defined(MpM_DontUseTimeouts)
# else // ! defined(MpM_UseTimeoutsInRetryLoops)
#  define RETRY_LOOPS_USE_TIMEOUTS  FALSE
# endif // ! defined(MpM_UseTimeoutsInRetryLoops)

namespace MplusM
{
    namespace Common
    {
        /*! @brief The mode of a channel. */
        enum ChannelMode
        {
            /*! @brief The connection is a TCP connection. */
            kChannelModeTCP,
            
            /*! @brief The connection is a UDP connection. */
            kChannelModeUDP,
            
            /*! @brief The connection is neither a TCP nor a UDP connection. */
            kChannelModeOther,
            
            /*! @brief Force the enumeration to be 4 bytes. */
            kChannelModeUnknown = 0x80000000
            
        }; // ChannelMode
        
        /*! @brief A description of a channel. */
        struct ChannelDescription
        {
            /*! @brief The name of the port being connected to. */
            yarp::os::ConstString _portName;
            
            /*! @brief The protocol of the port. */
            yarp::os::ConstString _portProtocol;
            
            /*! @brief The mode of the connection. */
            ChannelMode _portMode;
            
# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunused-private-field"
# endif // defined(__APPLE__)
            /*! @brief Filler to pad to alignment boundary */
            char _filler[4];
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)
            
        }; // ChannelDescription
        
        /*! @brief The format for the output from command-line tools. */
        enum OutputFlavour
        {
            /*! @brief Normal output with no special processing. */
            kOutputFlavourNormal,
            
            /*! @brief Output in JSON format. Tabs and newlines are replaced with spaces. */
            kOutputFlavourJSON,
            
            /*! @brief Output in tab-delimited format. Tabs and newlines are replaced with
             spaces. */
            kOutputFlavourTabs,
            
            /*! @brief Force the enumeration to be 4 bytes. */
            kOutputFlavourUnknown = 0x80000000
            
        }; // OutputFlavour
        
        /*! @brief The behavioural model for the service. */
        enum ServiceKind
        {
            /*! @brief The service has no specical characteristics. */
            kServiceKindNormal,
            
            /*! @brief The service provides a proxy for an input source. */
            kServiceKindInput,
            
            /*! @brief The service provides a proxy for an output destination. */
            kServiceKindOutput,
            
            /*! @brief The service provides a proxy for a transformative process. */
            kServiceKindFilter,
            
            /*! @brief The service is the Registry, which is a specialized 'normal' service. */
            kServiceKindRegistry,
            
            /*! @brief Force the enumeration to be 4 bytes. */
            kServiceKindUnknown = 0x80000000
            
        }; // ServiceKind
        
        /*! @brief A sequence of connections. */
        typedef std::vector<ChannelDescription>    ChannelVector;
        
        /*! @brief A sequence of random numbers. */
        typedef std::vector<double>                DoubleVector;
        
        /*! @brief A sequence of strings. */
        typedef std::vector<yarp::os::ConstString> StringVector;
        
        /*! @brief A function that checks for early exit from loops.
         @param stuff Private data for the function.
         @returns @c true if the caller should exit any loops and @c false otherwise. */
        typedef bool (*CheckFunction)
            (void * stuff);
        
        /*! @brief Dump out a description of the provided connection information to the log.
         @param tag A unique string used to identify the call point for the output.
         @param aContact The connection information to be reported. */
        void DumpContactToLog(const char *              tag,
                              const yarp::os::Contact & aContact);
        
# if MAC_OR_LINUX_
        /*! @brief Return the YARP logging object.
         @returns The YARP logging object. */
        yarp::os::impl::Logger & GetLogger(void);
# endif // MAC_OR_LINUX_

        /*! @brief Generate a random channel name.
         @returns A randomly-generated channel name. */
        yarp::os::ConstString GetRandomChannelName(const char * channelRoot = DEFAULT_CHANNEL_ROOT);
        
        /*! @brief Generate a random channel name.
         @returns A randomly-generated channel name. */
        yarp::os::ConstString GetRandomChannelName(const yarp::os::ConstString & channelRoot);
        
        /*! @brief Perform initialization of internal resources.
         @param progName The name of the executing program.
         
         Should be called in the main() function of each application or service. */
        void Initialize(const char * progName);
        
        /*! @brief Connect two channels, using a backoff strategy with retries.
         @param sourceName The name of the source channel.
         @param destinationName The name of the destination channel.
         @param timeToWait The number of seconds allowed before a failure is considered.
         @param isUDP @c true if the connection is to be UDP and @c false otherwise.
         @param checker A function that provides for early exit from loops.
         @param checkStuff The private data for the early exit function.
         @returns @c true if the connection was established and @ false otherwise. */
        bool NetworkConnectWithRetries(const yarp::os::ConstString & sourceName,
                                       const yarp::os::ConstString & destinationName,
                                       const double                  timeToWait,
                                       const bool                    isUDP,
                                       CheckFunction                 checker,
                                       void *                        checkStuff);
        
        /*! @brief Disconnect two channels, using a backoff strategy with retries.
         @param sourceName The name of the source channel.
         @param destinationName The name of the destination channel.
         @param timeToWait The number of seconds allowed before a failure is considered.
         @param checker A function that provides for early exit from loops.
         @param checkStuff The private data for the early exit function.
         @returns @c true if the connection was removed and @ false otherwise. */
        bool NetworkDisconnectWithRetries(const yarp::os::ConstString & sourceName,
                                          const yarp::os::ConstString & destinationName,
                                          const double                  timeToWait,
                                          CheckFunction                 checker,
                                          void *                        checkStuff);
        
        /*! @brief Connect the standard signals to a handler.
         @param theHandler The new handler for the signals. */
        void SetSignalHandlers(yarp::os::YarpSignalHandler theHandler);
        
        /*! @brief Set up the signal-handling behaviour so that this thread will catch our 
         signal. */
        void SetUpCatcher(void);
        
# if MAC_OR_LINUX_
        /*! @brief Set up the error logger.
         @param progName The name of the executing program.
         
         Should be called in the main() function of each application or service before anything
         else. */
        void SetUpLogger(const char * progName);
# endif // MAC_OR_LINUX_

        /*! @brief Restore the normal signal-handling behaviour. */
        void ShutDownCatcher(void);
        
        /*! @brief Perform a busy loop, using yarp::os::Time::yield(). */
# if MAC_OR_LINUX_
        void Stall(void) __attribute__((noreturn));
# else // ! MAC_OR_LINUX_
        void Stall(void);
# endif // ! MAC_OR_LINUX_
        
    } // Common
    
    /*! @brief Return @c true if standard input can be used and @c false otherwise.
     @returns @c true if standard input can be used and @c false otherwise. */
    bool CanReadFromStandardInput(void);
    
    /*! @brief Returns @c true if the executable can continue running and @c false otherwise.
     @returns @c true if the executable can continue running and @c false otherwise. */
    bool IsRunning(void);
    
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
    
    /*! @brief Return a string with special characters escaped.
     @param inString The string to be processed.
     @param allowDoubleQuotes @c true if double quotes aren't escaped and @c false otherwise.
     @returns A string with special characters escaped. */
    yarp::os::ConstString SanitizeString(const yarp::os::ConstString & inString,
                                         const bool                    allowDoubleQuotes = false);
    
    /*! @brief The signal handler to catch requests to stop the service.
     @param signal The signal being handled. */
    void SignalRunningStop(int signal);
    
    /*! @brief Mark the executable as running or ready-to-run. */
    void StartRunning(void);
    
    /*! @brief Indicate that the executable should stop running. */
    void StopRunning(void);
    
} // MplusM

#endif // ! defined(MpMCommon_H_)
