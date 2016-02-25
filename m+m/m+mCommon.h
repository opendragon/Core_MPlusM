//--------------------------------------------------------------------------------------------------
//
//  File:       m+m/m+mCommon.h
//
//  Project:    m+m
//
//  Contains:   The function and variable declarations for common entities for m+m clients and
//              services.
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
//  Created:    2014-02-18
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMCommon_H_))
# define MpMCommon_H_ /* Header guard */

# include <m+m/m+mConfig.h>

# if (! defined(LINUX_))
/*! @brief @c TRUE if Linux, @c FALSE otherwise. */
#  define LINUX_ defined(__linux__)
# endif // ! defined(LINUX_)
# if (! defined(MAC_OR_LINUX_))
/*! @brief @c TRUE if non-Windows, @c FALSE if Windows. */
#  define MAC_OR_LINUX_ (defined(__APPLE__) || defined(__linux__))
# endif // ! defined(MAC_OR_LINUX_)

# include <algorithm>
# include <cctype>
# include <csignal>
# include <cstdlib>
# include <cstring>
# include <iostream>
# include <list>
# include <map>
# include <stdint.h>
# include <sstream>
# include <time.h>
# include <vector>
# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
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
# if (! MAC_OR_LINUX_)
#  pragma warning(push)
#  pragma warning(disable: 4100)
#  pragma warning(disable: 4267)
#  pragma warning(disable: 4458)
#  pragma warning(disable: 4996)
# endif // ! MAC_OR_LINUX_
# include <yarp/os/all.h>
# include <yarp/conf/version.h>
# if (! MAC_OR_LINUX_)
#  pragma warning(pop)
# endif // ! MAC_OR_LINUX_
# if MAC_OR_LINUX_
#  include <yarp/os/impl/Logger.h>
# endif // MAC_OR_LINUX_
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

# if (! defined(TRUE))
#  define TRUE 1
# endif // ! defined(TRUE)
# if (! defined(FALSE))
#  define FALSE 0
# endif // ! defined(FALSE)

# if MAC_OR_LINUX_
#  include <sys/socket.h>
#  define SOCKET         int /* Standard socket type in *nix. */
#  define INVALID_SOCKET -1
# else // ! MAC_OR_LINUX_
#  pragma warning(push)
#  pragma warning(disable: 4996)
#  include <WinSock2.h>
#  include <Ws2tcpip.h>
#  pragma warning(pop)
# endif // ! MAC_OR_LINUX_

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The function and variable declarations for common entities for m+m clients and services. */

/*! @dir /m+m
 @brief The set of files that implement the m+m framework. */

/*! @dir /CommonTests
 @brief The set of files that provide test cases for the m+m framework. */

/*! @dir /ParserTest
 @brief The set of files that support testing the parsing of search requests. */

/*! @namespace MplusM
 @brief The classes that implement the m+m framework. */

/*! @namespace MplusM::Common
 @brief The classes that support the basic functionality of the m+m framework. */

/*! @namespace MplusM::Example
 @brief A set of example classes using features from m+m. */

/*! @namespace MplusM::Parser
 @brief The classes that support parsing of search requests and the generation of SQL @c SELECT
 strings from the search requests. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

/*! @brief The time between client announcements of associated channels. */
# define ANNOUNCE_INTERVAL_         (17.1 * ONE_SECOND_DELAY_)

/*! @brief The character separating argument descriptors. */
# define ARGUMENT_SEPARATOR_        "\v"

/*! @brief Construct a proper channel name from two parts. */
# define BUILD_NAME_(aa_, bb_)      T_(aa_ MpM_NAME_SEPARATOR_ bb_)

/*! @brief The carrier type to be used for m+m connections. */
# define CHANNEL_CARRIER_           "tcp"

/*! @brief A TAB character. */
# define CHAR_TAB_                  "\t"

/*! @brief A NEWLINE character. */
# define CHAR_NEWLINE_              "\n"

/*! @brief A DOUBLEQUOTE character. */
# define CHAR_DOUBLEQUOTE_          "\""

/*! @brief The size of the buffer used to display the date or the time. */
# define DATE_TIME_BUFFER_SIZE_     20

/*! @brief The default name for the root part of a channel name. */
# define DEFAULT_CHANNEL_ROOT_      "channel_"

/*! @brief The line length for command-line help output. */
# define HELP_LINE_LENGTH_          250

/*! @brief The prefix string to use for internal channels. */
# define HIDDEN_CHANNEL_PREFIX_     T_(MpM_BASE_NAME_PREFIX_ "$$$_")

/*! @brief The basic time interval for retries. */
# define INITIAL_RETRY_INTERVAL_    (0.11 * ONE_SECOND_DELAY_)

/*! @brief The Windows line end sequence. */
# define LINE_END_                  "\r\n"

/*! @brief The maximum number of retries before declaring failure, if not using timeouts. */
# define MAX_RETRIES_               7

/*! @brief The largest IP port that is acceptable. */
# define MAXIMUM_PORT_ALLOWED_      65535

/*! @brief The smallest IP port that is acceptable. */
# define MINIMUM_PORT_ALLOWED_      1024

/*! @brief The time allowed for 'checkNetwork()'. */
# define NETWORK_CHECK_TIMEOUT_     (3.1 * ONE_SECOND_DELAY_)

/*! @brief The delay value corresponding to one second of delay. */
# define ONE_SECOND_DELAY_          1.0

/*! @brief The time between checking for 'stale' registry entries. */
# define PING_CHECK_INTERVAL_       (7.3 * ONE_SECOND_DELAY_)

/*! @brief The number of missing pings before a service is declared 'dead'. */
# define PING_COUNT_MAX_            5

/*! @brief The time between ping requests that a service should use. */
# define PING_INTERVAL_             (9.7 * ONE_SECOND_DELAY_)

/*! @brief The retry interval multiplier. */
# define RETRY_MULTIPLIER_          1.21

/*! @brief The IP address for the loopback address for the machine that is running the
executable. */
# define SELF_ADDRESS_IPADDR_       "127.0.0.1"

/*! @brief The IP name for the loopback address for the machine that is running the
executable. */
# define SELF_ADDRESS_NAME_         "localhost"

/*! @brief The standard copyright holder name to use for m+m-created executables. */
# define STANDARD_COPYRIGHT_NAME_   "H Plus Technologies Ltd. and Simon Fraser University"

/*! @brief The signal to use for internally-detected timeouts. */
# if MAC_OR_LINUX_
#  define STANDARD_SIGNAL_TO_USE_   SIGUSR2
# else // ! MAC_OR_LINUX_
#  define STANDARD_SIGNAL_TO_USE_   42
# endif // ! MAC_OR_LINUX_

/*! @brief The default timeout duration in seconds. */
# define STANDARD_WAIT_TIME_        (5.3 * ONE_SECOND_DELAY_)

/*! @brief A simple macro to hold the pieces of a string together. */
# define T_(xx_)                    xx_

/*! @brief The standard copy constructor and assignment operator declarations. */
# define COPY_AND_ASSIGNMENT_(xx_) \
    xx_(const xx_ & other_);\
    xx_ &\
    operator =(const xx_ & other_)

/*! @brief @c TRUE if retry loops use timeouts and @c FALSE otherwise. */
# if defined(MpM_UseTimeoutsInRetryLoops)
#  if defined(MpM_DontUseTimeouts)
#   define RETRY_LOOPS_USE_TIMEOUTS_ FALSE
#  else // ! defined(MpM_DontUseTimeouts)
#   define RETRY_LOOPS_USE_TIMEOUTS_ TRUE
#  endif // ! defined(MpM_DontUseTimeouts)
# else // ! defined(MpM_UseTimeoutsInRetryLoops)
#  define RETRY_LOOPS_USE_TIMEOUTS_  FALSE
# endif // ! defined(MpM_UseTimeoutsInRetryLoops)

/*! @brief @c TRUE if the version of YARP uses 'fatal' instead of 'fail'. */
# undef USE_YARP_FATAL_NOT_FAIL_
# undef YARP_SYSTEM_INFO_MOVED_
# if (2 < YARP_VERSION_MAJOR)
#  define USE_YARP_FATAL_NOT_FAIL_ TRUE
#  define YARP_SYSTEM_INFO_MOVED_ TRUE
# elif (3 < YARP_VERSION_MINOR)
#  define USE_YARP_FATAL_NOT_FAIL_ TRUE
#  define YARP_SYSTEM_INFO_MOVED_ TRUE
# elif (62 < YARP_VERSION_PATCH)
#  define USE_YARP_FATAL_NOT_FAIL_ TRUE
#  define YARP_SYSTEM_INFO_MOVED_ TRUE
# else // 62 >= YARP_VERSION_PATCH
#  define USE_YARP_FATAL_NOT_FAIL_ FALSE
#  define YARP_SYSTEM_INFO_MOVED_ FALSE
# endif // 62 >= YARP_VERSION_PATCH

# if MAC_OR_LINUX_
#  define MpM_ERROR_(xx_) GetLogger().error(xx_)
# else // ! MAC_OR_LINUX_
#  define MpM_ERROR_(xx_) cerr << "Error: " << xx_ << endl
# endif // ! MAC_OR_LINUX_

# if MAC_OR_LINUX_
#  if USE_YARP_FATAL_NOT_FAIL_
#   define MpM_FAIL_(xx_) GetLogger().fatal(xx_)
#  else // ! USE_YARP_FATAL_NOT_FAIL_
#   define MpM_FAIL_(xx_) GetLogger().fail(xx_)
#  endif // ! USE_YARP_FATAL_NOT_FAIL_
# else // ! MAC_OR_LINUX_
#  define MpM_FAIL_(xx_) cerr << "Fail: " << xx_ << endl
# endif // ! MAC_OR_LINUX_

/*! @brief A standard error message. */
# define MSG_COULD_NOT_CONNECT_TO_SERVICE "Could not connect to the required service."

/*! @brief A standard error message. */
# define MSG_COULD_NOT_DISCONNECT_FROM_SERVICE "Problem disconnect ing from the service."

/*! @brief A standard error message. */
# define MSG_COULD_NOT_FIND_SERVICE "Could not find the required service."

/*! @brief A standard error message. */
# define MSG_REGISTRY_NOT_RUNNING "Registry Service not running."

/*! @brief A standard error message. */
# define MSG_SERVICE_NOT_REGISTERED "Service could not be registered."

/*! @brief A standard error message. */
# define MSG_SERVICE_NOT_STARTED "Service could not be started."

/*! @brief A standard error message. */
# define MSG_YARP_NOT_RUNNING "YARP network not running."

/*! @brief An alias for a YARP-type string. */
typedef yarp::os::ConstString YarpString;

/*! @brief A sequence of strings. */
typedef std::vector<YarpString> YarpStringVector;

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
            
            /*! @brief Force the size to be 4 bytes. */
            kChannelModeUnknown = 0x7FFFFFFF
            
        }; // ChannelMode
        
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
            
            /*! @brief Force the size to be 4 bytes. */
            kOutputFlavourUnknown = 0x7FFFFFFF
            
        }; // OutputFlavour
        
        /*! @brief The behavioural model for the service. */
        enum ServiceKind
        {
            /*! @brief The service provides a proxy for a transformative process that utilizes
             another service. */
            kServiceKindAdapter,
            
            /*! @brief The service provides a proxy for a transformative process. */
            kServiceKindFilter,
            
            /*! @brief The service provides a proxy for an input source. */
            kServiceKindInput,
            
            /*! @brief The service has no specical characteristics. */
            kServiceKindNormal,
            
            /*! @brief The service provides a proxy for an output destination. */
            kServiceKindOutput,
            
            /*! @brief The service is the %Registry, which is a specialized 'normal' service. */
            kServiceKindRegistry,
            
            /*! @brief Force the size to be 4 bytes. */
            kServiceKindUnknown = 0x7FFFFFFF
            
        }; // ServiceKind
        
        /*! @brief A description of a channel. */
        struct ChannelDescription
        {
            /*! @brief The name of the port being connected to. */
            YarpString _portName;
            
            /*! @brief The protocol of the port. */
            YarpString _portProtocol;
            
            /*! @brief The protocol description. */
            YarpString _protocolDescription;
            
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
        
        /*! @brief An IPv4 address. */
        struct NetworkAddress
        {
            /*! @brief The bytes of the address, in the order MSB .. LSB. */
            int _ipBytes[4];
            
            /*! @brief The port number of the address. */
            int _ipPort;
            
        }; // NetworkAddress
        
        /*! @brief A sequence of connections. */
        typedef std::vector<ChannelDescription> ChannelVector;
        
        /*! @brief A sequence of random numbers. */
        typedef std::vector<double> DoubleVector;
        
        /*! @brief A function that checks for early exit from loops.
         @param stuff Private data for the function.
         @returns @c true if the caller should exit any loops and @c false otherwise. */
        typedef bool
        (*CheckFunction)
            (void * stuff);
        
        /*! @brief Dump out a description of the provided connection information to the log.
         @param tag A unique string used to identify the call point for the output.
         @param aContact The connection information to be reported. */
        void
        DumpContactToLog(const char *              tag,
                         const yarp::os::Contact & aContact);
        
# if MAC_OR_LINUX_
        /*! @brief Return the YARP logging object.
         @returns The YARP logging object. */
        yarp::os::impl::Logger &
        GetLogger(void);
# endif // MAC_OR_LINUX_

        /*! @brief Generate a random channel name.
         @returns A randomly-generated channel name. */
        YarpString
        GetRandomChannelName(const char * channelRoot = DEFAULT_CHANNEL_ROOT_);
        
        /*! @brief Generate a random channel name.
         @returns A randomly-generated channel name. */
        YarpString
        GetRandomChannelName(const YarpString & channelRoot);
        
        /*! @brief Perform initialization of internal resources.
         @param progName The name of the executing program.
         
         Should be called in the main() function of each application or service. */
        void
        Initialize(const YarpString & progName);
        
        /*! @brief Connect the standard signals to a handler.
         @param theHandler The new handler for the signals. */
        void
        SetSignalHandlers(yarp::os::YarpSignalHandler theHandler);
        
        /*! @brief Set up the signal-handling behaviour so that this thread will catch our 
         signal. */
        void
        SetUpCatcher(void);
        
# if MAC_OR_LINUX_
        /*! @brief Set up the error logger.
         @param progName The name of the executing program.
         
         Should be called in the main() function of each application or service before anything
         else. */
        void
        SetUpLogger(const YarpString & progName);
# endif // MAC_OR_LINUX_

        /*! @brief Restore the normal signal-handling behaviour. */
        void
        ShutDownCatcher(void);
        
        /*! @brief Perform a busy loop, using yarp::os::Time::yield(). */
# if MAC_OR_LINUX_
        void
        Stall(void) __attribute__((noreturn));
# else // ! MAC_OR_LINUX_
        void
        Stall(void);
# endif // ! MAC_OR_LINUX_
        
    } // Common
    
    /*! @brief Return @c true if standard input can be used and @c false otherwise.
     @returns @c true if standard input can be used and @c false otherwise. */
    bool
    CanReadFromStandardInput(void);
    
    /*! @brief Introduce some delay in processing.
     @param factor The fraction of a second to delay. */
    void
    ConsumeSomeTime(const double factor = 200.0);
    
    /*! @brief Sit in an idle loop until a request to stop occurs. */
    void
    IdleUntilNotRunning(void);

    /*! @brief Returns @c true if the executable can continue running and @c false otherwise.
     @returns @c true if the executable can continue running and @c false otherwise. */
    bool
    IsRunning(void);
    
    /*! @brief Check if a list is actually a dictionary, as they have the same textual
     representation.
     
     Note that the output dictionary will be modified, regardless of whether or not the list is, in
     fact, a dictionary.
     @param aList The list of interest.
     @param aDictionary The corresponding dictionary.
     @returns @c true if the list can be converted into a dictionary and @c false otherwise. */
    bool
    ListIsReallyDictionary(const yarp::os::Bottle & aList,
                           yarp::os::Property &     aDictionary);
    
    /*! @brief Return the name of a signal.
     @param theSignal The signal of interest.
     @returns A string description of the signal. */
    const char *
    NameOfSignal(const int theSignal);
    
    /*! @brief Write out a (possibly multi-line) description.
     @param outStream The stream to write to.
     @param heading The text to appear on the first line before the beginning of the description.
     @param description The description, which may contain multiple newlines. */
    void
    OutputDescription(std::ostream &             outStream,
                      const char *               heading,
                      const YarpString & description);
    
    /*! @brief Return a string with special characters escaped.
     @param inString The string to be processed.
     @param allowDoubleQuotes @c true if double quotes aren't escaped and @c false otherwise.
     @returns A string with special characters escaped. */
    YarpString
    SanitizeString(const YarpString & inString,
                   const bool         allowDoubleQuotes = false);
    
    /*! @brief The signal handler to catch requests to stop the service.
     @param signal The signal being handled. */
    void
    SignalRunningStop(const int signal);
    
    /*! @brief Mark the executable as running or ready-to-run. */
    void
    StartRunning(void);
    
    /*! @brief Indicate that the executable should stop running. */
    void
    StopRunning(void);
    
    /*! @brief The escapre character. */
    extern const char kEscapeChar;
    
    /*! @brief The directory separator string; */
    extern const YarpString kDirectorySeparator;

} // MplusM

#endif // ! defined(MpMCommon_H_)
