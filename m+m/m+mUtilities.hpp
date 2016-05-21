//--------------------------------------------------------------------------------------------------
//
//  File:       m+m/m+mUtilities.hpp
//
//  Project:    m+m
//
//  Contains:   The function and variable declarations for utilities for m+m clients and services.
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
//  Created:    2014-05-16
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMUtilities_HPP_))
# define MpMUtilities_HPP_ /* Header guard */

# include <m+m/m+mBaseArgumentDescriptor.hpp>
# include <m+m/m+mChannelStatusReporter.hpp>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The function and variable declarations for utilities for m+m clients and services. */

/*! @namespace MplusM::Utilities
 @brief The classes that extend the basic functionality of the m+m framework. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace Common
    {
        class BaseAdapterArguments;
        class StringBuffer;
    } // Common

    namespace Utilities
    {
        /*! @brief Which combination of input and output to use. */
        enum InputOutputFlag
        {
            /*! @brief Neither input nor output. */
            kInputAndOutputNone   = 0x0,

            /*! @brief Input. */
            kInputAndOutputInput  = 0x1,

            /*! @brief Output. */
            kInputAndOutputOutput = 0x2,

            /*! @brief Both input and output. */
            kInputAndOutputBoth   = 0x3,

            /*! @brief Force the size to be 4 bytes. */
            kInputAndOutputUnknown = 0x7FFFFFFF

        }; // InputOutputFlag

        /*! @brief The kinds of ports. */
        enum PortKind
        {
            /*! @brief The port is an adapter port. */
            kPortKindAdapter,

            /*! @brief The port is a client port. */
            kPortKindClient,

            /*! @brief The port is the %Registry Service port. */
            kPortKindRegistryService,

            /*! @brief The port is a service port. */
            kPortKindService,

            /*! @brief The port is s standard port. */
            kPortKindStandard,

            /*! @brief Force the size to be 4 bytes. */
            kPortKindUnknown = 0x7FFFFFFF

        }; // PortKind

        /*! @brief The attributes of a port. */
        struct PortDescriptor
        {
            /*! @brief The registered name of the port. */
            YarpString _portName;

            /*! @brief The IP address for the port. */
            YarpString _portIpAddress;

            /*! @brief The IP port number for the port. */
            YarpString _portPortNumber;

        }; // PortDescriptor

        /*! @brief The attributes of a service or adapter. */
        struct ServiceDescriptor
        {
            /*! @brief The standard name for the service or adapter. */
            YarpString _serviceName;

            /*! @brief The standard channel for the service or adapter. */
            YarpString _channelName;

            /*! @brief The description of the service or adapter. */
            YarpString _description;

            /*! @brief The extra information for the service or adapter. */
            YarpString _extraInfo;

            /*! @brief The argument descriptions for the service. */
            DescriptorVector _argumentList;

            /*! @brief The set of secondary client channels for the service or adapter. */
            Common::ChannelVector _clientChannels;

            /*! @brief The set of secondary input channels for the service or adapter. */
            Common::ChannelVector _inputChannels;

            /*! @brief The set of secondary output channels for the service or adapter. */
            Common::ChannelVector _outputChannels;

            /*! @brief The description of the behavioural model for the service or adapter. */
            YarpString _kind;

            /*! @brief The name of the input channel for the service or adapter. */
            YarpString _path;

            /*! @brief The description of the requests for the service or adapter. */
            YarpString _requestsDescription;

            /*! @brief The modifier tag for the service or adapter. */
            YarpString _tag;

        }; // ServiceDescriptor

        /*! @brief A set of port descriptions. */
        typedef std::vector<PortDescriptor> PortVector;

        /*! @brief Add a connection between two ports.
         @param[in] fromPortName The name of the source port.
         @param[in] toPortName The name of the destination port.
         @param[in] timeToWait The number of seconds allowed before a failure is considered.
         @param[in] isUDP @c true if the connection is to be UDP and @c false otherwise.
         @param[in] checker A function that provides for early exit from loops.
         @param[in] checkStuff The private data for the early exit function.
         @returns @c true if successful and @c false otherwise. */
        bool
        AddConnection(const YarpString &    fromPortName,
                      const YarpString &    toPortName,
                      const double          timeToWait,
                      const bool            isUDP = false,
                      Common::CheckFunction checker = NULL,
                      void *                checkStuff = NULL);

        /*! @brief Check if a connection exists between two ports.
         @param[in] fromPortName The name of the source port.
         @param[in] toPortName The name of the destination port.
         @returns @c true if a connection exists and @c false otherwise. */
        bool
        CheckConnection(const YarpString & fromPortName,
                        const YarpString & toPortName);

        /*! @brief Check if a channel is in use.
         @param[in] channelName The name of the channel to check.
         @returns @c true if the channel is known to YARP and @c false otherwise. */
        bool
        CheckForChannel(const YarpString & channelName);

        /*! @brief Check for the NameServerReporter mDNS entry and update the YARP information if
         found. */
        void
        CheckForNameServerReporter(void);

        /*! @brief Check if the %Registry Service is active.
         @returns @c true if the %Registry Service port is present and @c false otherwise. */
        bool
        CheckForRegistryService(void);

        /*! @brief Check if the YARP network is available.
         @param[in] quiet @c true if nothing should be reported on failure and @c false otherwise.
         @returns @c true if the YARP network is available and @c false otherwise. */
        bool
        CheckForValidNetwork(const bool quiet = false);

        /*! @brief Check if the %Registry Service is present in a list.
         @param[in] ports The set of detected ports.
         @returns @c true if the %Registry Service port is present and @c false otherwise. */
        bool
        CheckListForRegistryService(const PortVector & ports);

        /*! @brief Convert a YARP message into a JSON record.
         @param[in,out] outBuffer The buffer to be written to.
         @param[in] input The message to be processed. */
# if defined(MpM_UseCustomStringBuffer)
        void
        ConvertMessageToJSON(Common::StringBuffer &   outBuffer,
                             const yarp::os::Bottle & input);
# else // ! defined(MpM_UseCustomStringBuffer)
        void
        ConvertMessageToJSON(std::stringstream &      outBuffer,
                             const yarp::os::Bottle & input);
# endif // ! defined(MpM_UseCustomStringBuffer)

        /*! @brief Convert the service metrics into a string.
         @param[in] metrics The metrics to convert.
         @param[in] flavour The output format to be used.
         @returns A string representation of the service metrics. */
        YarpString
        ConvertMetricsToString(const yarp::os::Bottle &    metrics,
                               const Common::OutputFlavour flavour = Common::kOutputFlavourNormal);

        /*! @brief Collect the input and output connections for a port.
         @param[in] portName The port to be inspected.
         @param[out] inputs The collected inputs for the port.
         @param[out] outputs The collected outputs for the port.
         @param[in] which A flag to specify what is to be gathered.
         @param[in] quiet @c true if status output is to be suppressed and @c false otherwise.
         @param[in] checker A function that provides for early exit from loops.
         @param[in] checkStuff The private data for the early exit function. */
        void
        GatherPortConnections(const YarpString &      portName,
                              Common::ChannelVector & inputs,
                              Common::ChannelVector & outputs,
                              const InputOutputFlag   which,
                              const bool              quiet = false,
                              Common::CheckFunction   checker = NULL,
                              void *                  checkStuff = NULL);

        /*! @brief Retrieve the configuration values for a service.
         @param[in] serviceChannelName The channel for the service.
         @param[out] values The configuration values for a service.
         @param[in] timeToWait The number of seconds allowed before a failure is considered.
         @param[in] checker A function that provides for early exit from loops.
         @param[in] checkStuff The private data for the early exit function.
         @returns @c true if the service returned the desired information and @c false otherwise. */
        bool
        GetConfigurationForService(const YarpString &    serviceChannelName,
                                   YarpStringVector &    values,
                                   const double          timeToWait,
                                   Common::CheckFunction checker = NULL,
                                   void *                checkStuff = NULL);

        /*! @brief Return the time since 1 January, 1970, as milliseconds.
         @returns The time elapsed since 1 January, 1970. */
        int64_t
        GetCurrentTimeInMilliseconds(void);

        /*! @brief Get the configured server address and port.
         @param[out] serverAddress The configured server address.
         @param[out] serverPort The configured server port.
         @returns @c true if the returned values are valid and @c false otherwise. */
        bool
        GetCurrentYarpConfiguration(struct in_addr & serverAddress,
                                    int &            serverPort);

        /*! @brief Fill buffers with the current date and time as strings.
         @param[in] dateBuffer The buffer to fill with the date.
         @param[in] dateBufferSize The size of the buffer for the date.
         @param[in] timeBuffer The buffer to fill with the time.
         @param[in] timeBufferSize The size of the buffer for the time. */
        void
        GetDateAndTime(char *       dateBuffer,
                       const size_t dateBufferSize,
                       char *       timeBuffer,
                       const size_t timeBufferSize);

        /*! @brief Get the set of detected ports.
         @param[out] ports The set of detected ports.
         @param[in] includeHiddenPorts @c true if all ports are returned and @c false is 'hidden'
         ports are ignored.
         @returns @c true if the list of ports is valid and @c false otherwise. */
        bool
        GetDetectedPortList(PortVector & ports,
                            const bool   includeHiddenPorts = false);

        /*! @brief Retrieve the extra information string for a service.
         @param[in] serviceChannelName The channel for the service.
         @param[out] extraInfo The extra information for a service.
         @param[in] timeToWait The number of seconds allowed before a failure is considered.
         @param[in] checker A function that provides for early exit from loops.
         @param[in] checkStuff The private data for the early exit function.
         @returns @c true if the service returned the desired information and @c false otherwise. */
        bool
        GetExtraInformationForService(const YarpString &    serviceChannelName,
                                      YarpString &          extraInfo,
                                      const double          timeToWait,
                                      Common::CheckFunction checker = NULL,
                                      void *                checkStuff = NULL);

        /*! @brief Return the base name of a file name.
         @param[in] inFileName The file name to be processed.
         @returns The base name of a file name. */
        YarpString
        GetFileNameBase(const YarpString & inFileName);

        /*! @brief Return the file name part of a path.
         @param[in] inFileName The file path to be processed.
         @returns The file name part of a path. */
        YarpString
        GetFileNamePart(const YarpString & inFileName);

        /*! @brief Return the global status reporter.
         @returns The global status reporter. */
        Common::ChannelStatusReporter *
        GetGlobalStatusReporter(void);

        /*! @brief Retrieve the list of available IP addresses for the machine.
         @param[out] result The list of available IP addresses. */
        void
        GetMachineIPs(YarpStringVector & result);

        /*! @brief Retrieve the channel metrics for a service.
         @param[in] serviceChannelName The channel for the service.
         @param[out] metrics The metrics for the channels of a service.
         @param[in] timeToWait The number of seconds allowed before a failure is considered.
         @param[in] checker A function that provides for early exit from loops.
         @param[in] checkStuff The private data for the early exit function.
         @returns @c true if the service returned the desired information and @c false otherwise. */
        bool
        GetMetricsForService(const YarpString &    serviceChannelName,
                             yarp::os::Bottle &    metrics,
                             const double          timeToWait,
                             Common::CheckFunction checker = NULL,
                             void *                checkStuff = NULL);

        /*! @brief Retrieve the state of the channel metrics for a service.
         @param[in] serviceChannelName The channel for the service.
         @param[out] metricsState The state of metrics collection for the channels of a service.
         @param[in] timeToWait The number of seconds allowed before a failure is considered.
         @param[in] checker A function that provides for early exit from loops.
         @param[in] checkStuff The private data for the early exit function.
         @returns @c true if the service returned the desired information and @c false otherwise. */
        bool
        GetMetricsStateForService(const YarpString &    serviceChannelName,
                                  bool &                metricsState,
                                  const double          timeToWait,
                                  Common::CheckFunction checker = NULL,
                                  void *                checkStuff = NULL);

        /*! @brief Retrieve the details for a service.
         @param[in] serviceChannelName The channel for the service.
         @param[out] descriptor The details for a service.
         @param[in] timeToWait The number of seconds allowed before a failure is considered.
         @param[in] checker A function that provides for early exit from loops.
         @param[in] checkStuff The private data for the early exit function.
         @returns @c true if the service returned the desired information and @c false otherwise. */
        bool
        GetNameAndDescriptionForService(const YarpString &    serviceChannelName,
                                        ServiceDescriptor &   descriptor,
                                        const double          timeToWait,
                                        Common::CheckFunction checker = NULL,
                                        void *                checkStuff = NULL);

        /*! @brief Map a port name to the port kind.
         @param[in] portName The name of the port.
         @returns The kind of the port. */
        PortKind
        GetPortKind(const YarpString & portName);

        /*! @brief Return the IP address and port number for a port.
         @param[in] portName The port to be located.
         @returns The IP address and port number of the port. */
        YarpString
        GetPortLocation(const YarpString & portName);

        /*! @brief Return a random string of hexadecimal digits.
         @returns A random string of hexadecimal digits. */
        YarpString
        GetRandomHexString(void);

        /*! @brief Retrieve the set of known services.
         @param[out] services The set of registered services.
         @param[in] quiet @c true if status output is to be suppressed and @c false otherwise.
         @param[in] checker A function that provides for early exit from loops.
         @param[in] checkStuff The private data for the early exit function.
         @returns @c true if the set of known services was updated successfully and @c false
         otherwise.*/
        bool
        GetServiceNames(YarpStringVector &    services,
                        const bool            quiet = false,
                        Common::CheckFunction checker = NULL,
                        void *                checkStuff = NULL);

        /*! @brief Retrieve the set of known services.
         @param[in] criteria The matching criteria to be used.
         @param[out] services The set of registered services.
         @param[in] quiet @c true if status output is to be suppressed and @c false otherwise.
         @param[in] checker A function that provides for early exit from loops.
         @param[in] checkStuff The private data for the early exit function.
         @returns @c true if the set of known services was updated successfully and @c false
         otherwise.*/
        bool
        GetServiceNamesFromCriteria(const YarpString &    criteria,
                                    YarpStringVector &    services,
                                    const bool            quiet = false,
                                    Common::CheckFunction checker = NULL,
                                    void *                checkStuff = NULL);

        /*! @brief Put the active thread to sleep for a number of milliseconds.
         @param[in] milliseconds The number of milliseconds to sleep. */
        void
        GoToSleep(const int milliseconds);

        /*! @brief Return a string representation of a service kind.
         @param[in] kind The value to be converted.
         @returns A string representation of the service kind. */
        const char *
        MapServiceKindToString(const Common::ServiceKind kind);

        /*! @brief Return the service kind corresponding to a string.
         @param[in] kindString The value to be converted.
         @returns The service kind corresponding to a string. */
        Common::ServiceKind
        MapStringToServiceKind(const YarpString & kindString);

        /*! @brief Connect two channels, using a backoff strategy with retries.
         @param[in] sourceName The name of the source channel.
         @param[in] destinationName The name of the destination channel.
         @param[in] timeToWait The number of seconds allowed before a failure is considered.
         @param[in] isUDP @c true if the connection is to be UDP and @c false otherwise.
         @param[in] checker A function that provides for early exit from loops.
         @param[in] checkStuff The private data for the early exit function.
         @returns @c true if the connection was established and @ false otherwise. */
        bool
        NetworkConnectWithRetries(const YarpString &    sourceName,
                                  const YarpString &    destinationName,
                                  const double          timeToWait,
                                  const bool            isUDP = false,
                                  Common::CheckFunction checker = NULL,
                                  void *                checkStuff = NULL);

        /*! @brief Disconnect two channels, using a backoff strategy with retries.
         @param[in] sourceName The name of the source channel.
         @param[in] destinationName The name of the destination channel.
         @param[in] timeToWait The number of seconds allowed before a failure is considered.
         @param[in] checker A function that provides for early exit from loops.
         @param[in] checkStuff The private data for the early exit function.
         @returns @c true if the connection was removed and @ false otherwise. */
        bool
        NetworkDisconnectWithRetries(const YarpString &    sourceName,
                                     const YarpString &    destinationName,
                                     const double          timeToWait,
                                     Common::CheckFunction checker = NULL,
                                     void *                checkStuff = NULL);

        /*! @brief Return @c true if the port name is for the %Registry Service.
         @param[in] portName the name of the port.
         @returns @c true if the port name is for the %Registry Service main port. */
        inline bool
        PortIsRegistryService(const YarpString & portName)
        {
            return (kPortKindRegistryService == GetPortKind(portName));
        } // PortIsRegistryService

        /*! @brief Process the standard options for client executables.
         The option '-h' / '--help' displays the list of optional parameters and arguments and
         returns @c false.
         The option '-i' / '--info' displays the type of the executable and the description of the
         executable and returns @c false.
         The option '-j' / '--json' specifies that output is to be in JSON format.
         The option '-t' / '--tabs' specifies that output is to be in tab-delimited format.
         The option '-v' / '--vers'displays the version and copyright information and returns
         @c false.
         @param[in] argc The number of arguments in 'argv'.
         @param[in] argv The arguments to be used with the utility.
         @param[in,out] argumentDescriptions Descriptions of the arguments to the adapter.
         @param[in] clientDescription A description of the client application.
         @param[in] year The copyright year for the calling application.
         @param[in] copyrightHolder The name of the entity holding the copyright to the utility.
         @param[out] flavour Set if the -j or -t options are seen.
         @param[in] ignoreFlavours @c true if the flavour options are ignored and @c false
         otherwise.
         @param[in] arguments If non-@c NULL, returns the arguments for the utility.
         @returns @c true if the program should continue and @c false if it should leave. */
        bool
        ProcessStandardClientOptions(const int               argc,
                                     char * *                argv,
                                     DescriptorVector &      argumentDescriptions,
                                     const YarpString &      clientDescription,
                                     const int               year,
                                     const char *            copyrightHolder,
                                     Common::OutputFlavour & flavour,
                                     const bool              ignoreFlavours = false,
                                     YarpStringVector *      arguments = NULL);

        /*! @brief Process the standard options for utility executables.
         The option '-h' / '--help' displays the list of optional parameters and arguments and
         returns @c false.
         The option '-i' / '--info' displays the type of the executable and the description of the
         executable and returns @c false.
         The option '-j' / '--json' specifies that output is to be in JSON format.
         The option '-t' / '--tabs' specifies that output is to be in tab-delimited format.
         The option '-v' / '--vers'displays the version and copyright information and returns
         @c false.
         @param[in] argc The number of arguments in 'argv'.
         @param[in] argv The arguments to be used with the utility.
         @param[in,out] argumentDescriptions Descriptions of the arguments to the adapter.
         @param[in] utilityDescription A description of the utility.
         @param[in] year The copyright year for the calling application.
         @param[in] copyrightHolder The name of the entity holding the copyright to the utility.
         @param[out] flavour Set if the -j or -t options are seen.
         @param[in] ignoreFlavours @c true if the flavour options are ignored and @c false
         otherwise.
         @param[in] arguments If non-@c NULL, returns the arguments for the utility.
         @returns @c true if the program should continue and @c false if it should leave. */
        bool
        ProcessStandardUtilitiesOptions(const int               argc,
                                        char * *                argv,
                                        DescriptorVector &      argumentDescriptions,
                                        const YarpString &      utilityDescription,
                                        const int               year,
                                        const char *            copyrightHolder,
                                        Common::OutputFlavour & flavour,
                                        const bool              ignoreFlavours = false,
                                        YarpStringVector *      arguments = NULL);

        /*! @brief Remove a connection between two ports.
         @param[in] fromPortName The name of the source port.
         @param[in] toPortName The name of the destination port.
         @param[in] checker A function that provides for early exit from loops.
         @param[in] checkStuff The private data for the early exit function.
         @returns @c true if the connection was removed and @c false otherwise. */
        bool
        RemoveConnection(const YarpString &    fromPortName,
                         const YarpString &    toPortName,
                         Common::CheckFunction checker = NULL,
                         void *                checkStuff = NULL);

        /*! @brief Remove any ports that YARP considers to be stale.
         @param[in] timeout The number of seconds to allow YARP to check a port. */
        void
        RemoveStalePorts(const float timeout = 5);

        /*! @brief Restart a service.
         @param[in] serviceChannelName The channel for the service.
         @param[in] timeToWait The number of seconds allowed before a failure is considered.
         @param[in] checker A function that provides for early exit from loops.
         @param[in] checkStuff The private data for the early exit function.
         @returns @c true if the service was restarted and @c false otherwise. */
        bool
        RestartAService(const YarpString &    serviceChannelName,
                        const double          timeToWait,
                        Common::CheckFunction checker = NULL,
                        void *                checkStuff = NULL);

        /*! @brief Send new configuration values to a service.
         @param[in] serviceChannelName The channel for the service.
         @param[in] newValues The new configuration values for a service.
         @param[in] timeToWait The number of seconds allowed before a failure is considered.
         @param[in] checker A function that provides for early exit from loops.
         @param[in] checkStuff The private data for the early exit function.
         @returns @c true if the service returned the desired information and @c false otherwise. */
        bool
        SetConfigurationForService(const YarpString &       serviceChannelName,
                                   const yarp::os::Bottle & newValues,
                                   const double             timeToWait,
                                   Common::CheckFunction    checker = NULL,
                                   void *                   checkStuff = NULL);

        /*! @brief Set the state of the channel metrics for a service.
         @param[in] serviceChannelName The channel for the service.
         @param[in] newMetricsState The desired state of metrics collection for the channels of a
         service.
         @param[in] timeToWait The number of seconds allowed before a failure is considered.
         @param[in] checker A function that provides for early exit from loops.
         @param[in] checkStuff The private data for the early exit function.
         @returns @c true if the service returned the desired information and @c false otherwise. */
        bool
        SetMetricsStateForService(const YarpString &    serviceChannelName,
                                  const bool            newMetricsState,
                                  const double          timeToWait,
                                  Common::CheckFunction checker = NULL,
                                  void *                checkStuff = NULL);

        /*! @brief Set up the global status reporter. */
        void
        SetUpGlobalStatusReporter(void);

        /*! @brief Shut down the global status reporter. */
        void
        ShutDownGlobalStatusReporter(void);

        /*! @brief Shut down a service.
         @param[in] serviceChannelName The channel for the service.
         @param[in] timeToWait The number of seconds allowed before a failure is considered.
         @param[in] checker A function that provides for early exit from loops.
         @param[in] checkStuff The private data for the early exit function.
         @returns @c true if the service was stopped and @c false otherwise. */
        bool
        StopAService(const YarpString &    serviceChannelName,
                     const double          timeToWait,
                     Common::CheckFunction checker = NULL,
                     void *                checkStuff = NULL);

        /*! @brief Checks a network port number for validity.
         @param[in] aPort The port number to be checked.
         @param[in] systemAllowed @c true if system port numbers are valid and @c false otherwise.
         @returns @c true if the port number is valid and @c false otherwise. */
        inline bool
        ValidPortNumber(const int  aPort,
                        const bool systemAllowed = false)
        {
            return (((systemAllowed ? 0 : MINIMUM_PORT_ALLOWED_) <= aPort) &&
                    (MAXIMUM_PORT_ALLOWED_ >= aPort));
        } // ValidPortNumber

    } // Utilities

} // MplusM

#endif // ! defined(MpMUtilities_HPP_)
