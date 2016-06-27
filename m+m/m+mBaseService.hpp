//--------------------------------------------------------------------------------------------------
//
//  File:       m+m/m+mBaseService.hpp
//
//  Project:    m+m
//
//  Contains:   The class declaration for the minimal functionality required for an m+m service.
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
//  Created:    2014-02-06
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMBaseService_HPP_))
# define MpMBaseService_HPP_ /* Header guard */

# include <m+m/m+mBaseArgumentDescriptor.hpp>
# include <m+m/m+mRequestMap.hpp>
# include <m+m/m+mSendReceiveCounters.hpp>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for the minimal functionality required for an m+m service. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

/*! @brief The option character for the 'args' option. */
# define ARGS_OPTION_STRING_ "a"

/*! @brief The option character for the 'channel' option. */
# define CHANNEL_OPTION_STRING_ "c"

/*! @brief The option character for the 'endpoint' option. */
# define ENDPOINT_OPTION_STRING_ "e"

/*! @brief The option character for the 'go' option. */
# define GO_OPTION_STRING_ "g"

/*! @brief The option character for the 'help' option. */
# define HELP_OPTION_STRING_ "h"

/*! @brief The option character for the 'info' option. */
# define INFO_OPTION_STRING_ "i"

/*! @brief The option character for the 'mod' option. */
# define MOD_OPTION_STRING_ "m"

/*! @brief The option character for the 'port' option. */
# define PORT_OPTION_STRING_ "p"

/*! @brief The option character for the 'report' option. */
# define REPORT_OPTION_STRING_ "r"

/*! @brief The option character for the 'tag' option. */
# define TAG_OPTION_STRING_ "t"

/*! @brief The option character for the 'version' option. */
# define VERSION_OPTION_STRING_ "v"

/*! @brief The full set of options, except for 'help' and 'version', which are always present. */
# define ALL_OPTIONS_STRING_ T_(ARGS_OPTION_STRING_ CHANNEL_OPTION_STRING_ ENDPOINT_OPTION_STRING_ \
                                GO_OPTION_STRING_ INFO_OPTION_STRING_ MOD_OPTION_STRING_ \
                                PORT_OPTION_STRING_ REPORT_OPTION_STRING_ TAG_OPTION_STRING_)

namespace MplusM
{
    namespace Common
    {
        class ArgumentsRequestHandler;
        class BaseContext;
        class BaseRequestHandler;
        class ChannelsRequestHandler;
        class ClientsRequestHandler;
        class DetachRequestHandler;
        class Endpoint;
        class ExtraInfoRequestHandler;
        class InfoRequestHandler;
        class ListRequestHandler;
        class MetricsRequestHandler;
        class MetricsStateRequestHandler;
        class NameRequestHandler;
        class PingThread;
        class ServiceInputHandler;
        class ServiceInputHandlerCreator;
        class SetMetricsStateRequestHandler;
        class StopRequestHandler;

        /*! @brief The modification values to be used with the service channel tag. */
        enum AddressTagModifier
        {
            /*! @brief Apply no modification. */
            kModificationNone,

            /*! @brief Use the LSB of the IP address as a modifier. */
            kModificationBottomByte,

            /*! @brief Use the low byte pair of the IP address as a modifier. */
            kModificationBottomTwoBytes,

            /*! @brief Use all but the MSB of the IP address as a modifier. */
            kModificationBottomThreeBytes,

            /*! @brief Use all the bytes of the IP address as a modifier. */
            kModificationAllBytes

        }; // AddressTagModifier

        /*! @brief The command-line options to skip.
         Note that the 'help' and 'version' options are always present. */
        enum OptionsMask
        {
            /*! @brief Skip no options. */
            kSkipNone           = 0x0000,

            /*! @brief Skip the 'args' option. */
            kSkipArgsOption     = 0x0001,

            /*! @brief Skip the 'channel' option. */
            kSkipChannelOption  = 0x0002,

            /*! @brief Skip the 'endpoint' option. */
            kSkipEndpointOption = 0x0004,

            /*! @brief Skip the 'go' / 'autostart' option. */
            kSkipGoOption       = 0x0008,

            /*! @brief Skip the 'info' option. */
            kSkipInfoOption     = 0x0010,

            /*! @brief Skip the 'mod' option. */
            kSkipModOption      = 0x0020,

            /*! @brief Skip the 'port' option. */
            kSkipPortOption     = 0x0040,

            /*! @brief Skip the 'report' option. */
            kSkipReportOption   = 0x0080,

            /*! @brief Skip the 'tag' option. */
            kSkipTagOption      = 0x0100,

            /*! @brief Skip all the options. */
            kSkipAllOptions     = 0xFFFF
        }; // OptionsMask

        /*! @brief The minimal functionality required for an m+m service. */
        class BaseService
        {
        public :

        protected :

        private :

            /*! @brief A mapping from strings to contexts. */
            typedef std::map<YarpString, BaseContext *> ContextMap;

            /*! @brief The entry-type for the mapping. */
            typedef ContextMap::value_type ContextMapValue;

        public :

            /*! @brief The constructor.
             @param[in] theKind The behavioural model for the service.
             @param[in] launchPath The command-line name used to launch the service.
             @param[in] argc The number of arguments in 'argv'.
             @param[in] argv The arguments passed to the executable used to launch the service.
             @param[in] tag The modifier for the service name and port names.
             @param[in] useMultipleHandlers @c true if simultaneous handlers are allowed, @c false
             if one handler is used.
             @param[in] canonicalName The channel-independent name of the service.
             @param[in] description The description of the service.
             @param[in] requestsDescription The description of the requests for the service.
             @param[in] serviceEndpointName The YARP name to be assigned to the new service.
             @param[in] servicePortNumber The channel being used by the service. */
            BaseService(const ServiceKind  theKind,
                        const YarpString & launchPath,
                        const int          argc,
                        char * *           argv,
                        const YarpString & tag,
                        const bool         useMultipleHandlers,
                        const YarpString & canonicalName,
                        const YarpString & description,
                        const YarpString & requestsDescription,
                        const YarpString & serviceEndpointName,
                        const YarpString & servicePortNumber = "");

            /*! @brief The constructor.

             Note that this is a special constructor for the test code, which does not support the
             service name modifier.
             @param[in] theKind The behavioural model for the service.
             @param[in] launchPath The command-line name used to launch the service.
             @param[in] argc The number of arguments in 'argv'.
             @param[in] argv The arguments passed to the executable used to launch the service.
             @param[in] useMultipleHandlers @c true if simultaneous handlers are allowed, @c false
             if one handler is used.
             @param[in] canonicalName The channel-independent name of the service.
             @param[in] description The description of the service.
             @param[in] requestsDescription The description of the requests for the service.
             @exception Exception Thrown if the endpoint isn't specified in the arguments. */
            BaseService(const ServiceKind  theKind,
                        const YarpString & launchPath,
                        const int          argc,
                        char * *           argv,
                        const bool         useMultipleHandlers,
                        const YarpString & canonicalName,
                        const YarpString & description,
                        const YarpString & requestsDescription);

            /*! @brief The destructor. */
            virtual
            ~BaseService(void);

            /*! @brief Return the description of the service.
             @returns The description of the service. */
            inline const YarpString &
            description(void)
            const
            {
                return _description;
            } // description

            /*! @brief Forget the specified client.
             @param[in] key The client-provided key. */
            void
            detachClient(const YarpString & key);

            /*! @brief Turn off the send / receive metrics collecting. */
            virtual void
            disableMetrics(void);

            /*! @brief Turn on the send / receive metrics collecting. */
            virtual void
            enableMetrics(void);

            /*! @brief Return the extra information for the service.
             @returns The extra information for the service. */
            inline const YarpString &
            extraInformation(void)
            const
            {
                return _extraInfo;
            } // extraInformation

            /*! @brief Fill in a list of clients for the service.
             @param[in,out] clients The list to be filled in. */
            void
            fillInClientList(YarpStringVector & clients);

            /*! @brief Fill in a list of secondary client channels for the service.
             @param[in,out] channels The list of channels to be filled in. */
            virtual void
            fillInSecondaryClientChannelsList(ChannelVector & channels);

            /*! @brief Fill in a list of secondary input channels for the service.
             @param[in,out] channels The list of channels to be filled in. */
            virtual void
            fillInSecondaryInputChannelsList(ChannelVector & channels);

            /*! @brief Fill in a list of secondary output channels for the service.
             @param[in,out] channels The list of channels to be filled in. */
            virtual void
            fillInSecondaryOutputChannelsList(ChannelVector & channels);

            /*! @brief Fill in the metrics for the service.
             @param[in,out] metrics The gathered metrics. */
            virtual void
            gatherMetrics(yarp::os::Bottle & metrics);

            /*! @brief Return the list of arguments given to the service.
             @returns The list of arguments given to the service. */
            inline const YarpStringVector &
            getArguments(void)
            const
            {
                return _originalArguments;
            } // getArguments

            /*! @brief Return the associated endpoint.
             @returns The associated endpoint. */
            inline Endpoint &
            getEndpoint(void)
            const
            {
                return *_endpoint;
            } // getEndpoint

            /*! @brief Update the auxiliary send / receive counters.
             @param[in] additionalCounters The counters to add. */
            void
            incrementAuxiliaryCounters(const SendReceiveCounters & additionalCounters);

            /*! @brief Return the state of the service.
             @returns @c true if the service has been started and @c false otherwise. */
            inline bool
            isStarted(void)
            const
            {
                return _started;
            } // isStarted

            /*! @brief Return the behavioural model for the service.
             @returns The behavioural model for the service. */
            inline ServiceKind
            kind(void)
            const
            {
                return _kind;
            } // kind

            /*! @brief Return the command-line name used to launch the service.
             @returns The command-line name used to launch the service. */
            inline const YarpString &
            launchPath(void)
            const
            {
                return _launchPath;
            } // launchPath

            /*! @brief Return the state of the  send / receive metrics.
             @returns @c true if the send / receive metrics are being gathered and @c false
             otherwise. */
            inline bool
            metricsAreEnabled(void)
            const
            {
                return _metricsEnabled;
            } // metricsAreEnabled

            /*! @brief Process partially-structured input data.
             @param[in] request The requested operation.
             @param[in] restOfInput The arguments for the operation.
             @param[in] senderChannel The name of the channel used to send the input data.
             @param[in] replyMechanism @c NULL if no reply is expected and non-@c NULL otherwise.
             @returns @c true if the input was correctly structured and successfully processed. */
            bool
            processRequest(const YarpString &           request,
                           const yarp::os::Bottle &     restOfInput,
                           const YarpString &           senderChannel,
                           yarp::os::ConnectionWriter * replyMechanism);

            /*! @brief Return the description of the requests for the service.
             @returns The description of the requests for the service. */
            inline const YarpString &
            requestsDescription(void)
            const
            {
                return _requestsDescription;
            } // requestsDescription

            /*! @brief Send a 'ping' on behalf of a service.
             @param[in] channelName The service channel to report with the ping.
             @param[in] checker A function that provides for early exit from loops.
             @param[in] checkStuff The private data for the early exit function. */
            bool
            sendPingForChannel(const YarpString & channelName,
                               CheckFunction      checker = NULL,
                               void *             checkStuff = NULL);

            /*! @brief Return the working name of the service.
             @returns The working name of the service. */
            inline const YarpString &
            serviceName(void)
            const
            {
                return _serviceName;
            } // serviceName

            /*! @brief Set the extra information for the service.
             @param[in] extraInfo The extra information for the service. */
            void
            setExtraInformation(const YarpString & extraInfo);

            /*! @brief Start the background 'pinging' thread. */
            void
            startPinger(void);

            /*! @brief Start processing requests.
             @returns @c true if the service was started and @c false if it was not. */
            virtual bool
            startService(void);

            /*! @brief Stop processing requests.
             @returns @c true if the service was stopped and @c false it if was not. */
            virtual bool
            stopService(void);

            /*! @brief Return the modifier tag of the service.
             @returns The modifier tag of the service. */
            inline const YarpString &
            tag(void)
            const
            {
                return _tag;
            } // tag

            /*! @brief Update the response counters for the service port.
             @param[in] numBytes The number of bytes sent. */
            void
            updateResponseCounters(const size_t numBytes);

        protected :

            /*! @brief Add a context for a persistent connection.
             @param[in] key The name for the context.
             @param[in] context The context to be remembered. */
            void
            addContext(const YarpString & key,
                       BaseContext *      context);

            /*! @brief Remove all contexts. */
            void
            clearContexts(void);

            /*! @brief Locate the context corresponding to a name.
             @param[in] key The name of the context.
             @returns @c NULL if the named context could not be found or a pointer to the context if
             found. */
            BaseContext *
            findContext(const YarpString & key);

            /*! @brief Remember the function to be used to handle a particular request.
             @param[in] handler The function to be called for the request. */
            void
            registerRequestHandler(BaseRequestHandler * handler);

            /*! @brief Remove a context.
             @param[in] key The name of the context. */
            void
            removeContext(const YarpString & key);

            /*! @brief Remember the function to be used to handle unrecognized requests.
             @param[in] handler The function to be called by default. */
            void
            setDefaultRequestHandler(BaseRequestHandler * handler);

            /*! @brief Forget the function to be used to handle a particular request.
             @param[in] handler The function that was called for the request. */
            void
            unregisterRequestHandler(BaseRequestHandler * handler);

        private :

            /*! @brief The copy constructor.
             @param[in] other The object to be copied. */
            BaseService(const BaseService & other);

            /*! @brief Enable the standard request handlers. */
            void
            attachRequestHandlers(void);

            /*! @brief Lock the data unless the lock would block.
             @returns @c true if the data was locked and @c false otherwise. */
            inline bool
            conditionallyLockContexts(void)
            {
                return _contextsLock.tryLock();
            } // conditionallyLockContexts

            /*! @brief Disable the standard request handlers. */
            void
            detachRequestHandlers(void);

            /*! @brief Lock the data. */
            inline void
            lockContexts(void)
            {
                _contextsLock.lock();
            } // lockContexts

            /*! @brief The assignment operator.
             @param[in] other The object to be copied.
             @returns The updated object. */
            BaseService &
            operator =(const BaseService & other);

            /*! @brief Unlock the data. */
            inline void
            unlockContexts(void)
            {
                _contextsLock.unlock();
            } // unlockContexts

        public :

        protected :

        private :

            /*! @brief The command-line name used to launch the service. */
            YarpString _launchPath;

            /*! @brief The contention lock used to avoid inconsistencies. */
            yarp::os::Mutex _contextsLock;

            /*! @brief The map between requests and request handlers. */
            RequestMap _requestHandlers;

            /*! @brief The map between requests and request handlers. */
            ContextMap _contexts;

            /*! @brief The description of the service. */
            YarpString _description;

            /*! @brief Extra information on the service. */
            YarpString _extraInfo;

            /*! @brief The description of the requests for the service. */
            YarpString _requestsDescription;

            /*! @brief The channel-independent name of the service. */
            YarpString _serviceName;

            /*! @brief The modifier tag for the service. */
            YarpString _tag;

            /*! @brief The arguments that were used to launch the executable that created this
             service. */
            YarpStringVector _originalArguments;

            /*! @brief The auxiliary send / receive counters. */
            SendReceiveCounters _auxCounters;

            /*! @brief The request handler for the 'arguments' request. */
            ArgumentsRequestHandler * _argumentsHandler;

            /*! @brief The request handler for the 'channels' request. */
            ChannelsRequestHandler * _channelsHandler;

            /*! @brief The request handler for the 'clients' request. */
            ClientsRequestHandler * _clientsHandler;

            /*! @brief The request handler for the 'detach' request. */
            DetachRequestHandler * _detachHandler;

            /*! @brief The request handler for the 'extraInfo' request. */
            ExtraInfoRequestHandler * _extraInfoHandler;

            /*! @brief The request handler for the 'info' request. */
            InfoRequestHandler * _infoHandler;

            /*! @brief The request handler for the 'list' request. */
            ListRequestHandler * _listHandler;

            /*! @brief The request handler for the 'metrics' request. */
            MetricsRequestHandler * _metricsHandler;

            /*! @brief The request handler for the 'metricsState' request. */
            MetricsStateRequestHandler * _metricsStateHandler;

            /*! @brief The request handler for the 'name' request. */
            NameRequestHandler * _nameHandler;

            /*! @brief The request handler for the 'setMetricsState' request. */
            SetMetricsStateRequestHandler * _setMetricsStateHandler;

            /*! @brief The request handler for the 'stop' request. */
            StopRequestHandler * _stopHandler;

            /*! @brief The connection point for the service. */
            Endpoint * _endpoint;

            /*! @brief The input handler for the service. */
            ServiceInputHandler * _handler;

            /*! @brief The input handler creator for the service. */
            ServiceInputHandlerCreator * _handlerCreator;

            /*! @brief The object used to generate 'pings' for the service. */
            PingThread * _pinger;

            /*! @brief The kind of service. */
            ServiceKind _kind;

            /*! @brief @c true if metrics are enabled and @c false otherwise. */
            bool _metricsEnabled;

            /*! @brief The current state of the service - @c true if active and @c false
             otherwise. */
            bool _started;

            /*! @brief Whether to use a handler creator or a handler - @c true for a creator and
             @c false otherwise. */
            bool _useMultipleHandlers;

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunused-private-field"
# endif // defined(__APPLE__)
            /*! @brief Filler to pad to alignment boundary */
            char _filler[1];
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

        }; // BaseService

        /*! @brief Update the endpoint name based on the provided arguments to the service.
         @param[in] defaultEndpointNameRoot The default endpoint root name.
         @param[in] modFlag The address-based modifier to apply to the tag value.
         @param[in,out] tag Set to the argument of the last -t option seen.
         @param[in,out] serviceEndpointName Set to the endpoint name to be used, based on the last
         -e and -t options.
         @param[in] tagModifier The string to be applied to the tag and endpoint for customization.
         @returns @c true if the endpoint name was set in the arguments and @c false if it was
         not. */
        bool
        AdjustEndpointName(const YarpString &       defaultEndpointNameRoot,
                           const AddressTagModifier modFlag,
                           YarpString &             tag,
                           YarpString &             serviceEndpointName,
                           const YarpString &       tagModifier = "");

        /*! @brief Determine the address that the Registry Service will use to connect to us.
         @param[in,out] ourAddress The IP address that we are using.
         @returns @c true if the address was determined and @c false otherwise. */
        bool
        GetOurEffectiveAddress(NetworkAddress & ourAddress);

        /*! @brief Process the standard options for service executables.
         The option '-c' / '--channel' displays the endpoint name after applying all other
         options and retunrs @c false.
         The option '-e' / '--endpoint' specifies the endpoint name to be used.
         The option '-g' / '--go' indicates that the service is to be started immediately.
         The option '-h' / '--help' displays the list of optional parameters and arguments and
         returns @c false.
         The option '-i' / '--info' displays the type of the executable, the available options and
         the description of the executable and returns @c false.
         The option '-m' / '--mod' specifies that the IP address will be used to modify the tag,
         if present, or to replace the tag. The argument is the number of bytes of the IP address to
         use, starting from the LSB.
         The option '-p' / '--port' specifie the port number to be used.
         The option '-r' / '--report' indicates that the service metrics are to be reported on exit.
         The option '-t' / '--tag' specifies the tag modifier, which is applied to the name of the
         channel, if the name was not specified. It is also applied to the service name as a suffix.
         The option '-v' / '--vers'displays the version and copyright information and returns
         @c false.
         @param[in] argc The number of arguments in 'argv'.
         @param[in] argv The arguments to be used with the service.
         @param[in] argumentDescriptions Descriptions of the arguments to the service.
         @param[in] serviceDescription A description of the service.
         @param[in] matchingCriteria The criteria used to locate the service that the service
         requires to be running.
         @param[in] year The copyright year for the calling application.
         @param[in] copyrightHolder The name of the entity holding the copyright to the utility.
         @param[out] goWasSet Set to @c true if the service is to be started immediately.
         @param[out] reportEndpoint Set to @c true if the service endpoint is to be reported.
         @param[out] reportOnExit Set to @c true if the -r option is seen.
         @param[out] tag Set to the argument of the last -t option seen.
         @param[out] serviceEndpointName Set to the endpoint name to be used, based on the last -e
         and -t options.
         @param[out] servicePortNumber Set to the argument of the last -p option seen.
         @param[out] modFlag The address-based modifier to apply to the tag value.
         @param[in] skipOptions The command-line options to be skipped.
         @param[in] arguments If non-@c NULL, returns the arguments for the service.
         @returns @c true if the service should continue and @c false if it should leave. */
        bool
        ProcessStandardServiceOptions(const int                     argc,
                                      char * *                      argv,
                                      Utilities::DescriptorVector & argumentDescriptions,
                                      const YarpString &            serviceDescription,
                                      const YarpString &            matchingCriteria,
                                      const int                     year,
                                      const char *                  copyrightHolder,
                                      bool &                        goWasSet,
                                      bool &                        reportEndpoint,
                                      bool &                        reportOnExit,
                                      YarpString &                  tag,
                                      YarpString &                  serviceEndpointName,
                                      YarpString &                  servicePortNumber,
                                      AddressTagModifier &          modFlag,
                                      const OptionsMask             skipOptions = kSkipNone,
                                      YarpStringVector *            arguments = NULL);

        /*! @brief Register a local service with a running %Registry Service.
         @param[in] channelName The channel provided by the service.
         @param[in] service The actual service being registered.
         @param[in] checker A function that provides for early exit from loops.
         @param[in] checkStuff The private data for the early exit function.
         @returns @c true if the service was successfully registered and @c false otherwise. */
        bool
        RegisterLocalService(const YarpString & channelName,
                             BaseService &      service,
                             CheckFunction      checker = NULL,
                             void *             checkStuff = NULL);

        /*! @brief Unregister a local service with a running %Registry Service.
         @param[in] channelName The channel provided by the service.
         @param[in] service The actual service being unregistered.
         @param[in] checker A function that provides for early exit from loops.
         @param[in] checkStuff The private data for the early exit function.
         @returns @c true if the service was successfully unregistered and @c false otherwise. */
        bool
        UnregisterLocalService(const YarpString & channelName,
                               BaseService &      service,
                               CheckFunction      checker = NULL,
                               void *             checkStuff = NULL);

    } // Common

} // MplusM

#endif // ! defined(MpMBaseService_HPP_)
