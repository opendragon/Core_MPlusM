//--------------------------------------------------------------------------------------------------
//
//  File:       mpm/M+MBaseService.h
//
//  Project:    M+M
//
//  Contains:   The class declaration for the minimal functionality required for an M+M service.
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
//  Created:    2014-02-06
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMBaseService_H_))
# define MpMBaseService_H_ /* Header guard */

# include <mpm/M+MRequestMap.h>

# include <stdint.h>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 
 @brief The class declaration for the minimal functionality required for an M+M service. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace Common
    {
        class BaseContext;
        class BaseRequestHandler;
        class BaseServiceInputHandler;
        class BaseServiceInputHandlerCreator;
        class ChannelsRequestHandler;
        class ClientsRequestHandler;
        class CountRequestHandler;
        class DetachRequestHandler;
        class Endpoint;
        class InfoRequestHandler;
        class ListRequestHandler;
        class NameRequestHandler;
        class PingThread;
        
        /*! @brief The minimal functionality required for an M+M service. */
        class BaseService
        {
        public:
            
            /*! @brief The constructor.
             @param theKind The behavioural model for the service.
             @param launchPath The command-line name used to launch the service.
             @param tag The modifier for the service name.
             @param useMultipleHandlers @c true if simultaneous handlers are allowed, @c false if
             one handler is used.
             @param canonicalName The channel-independent name of the service.
             @param description The description of the service.
             @param requestsDescription The description of the requests for the service.
             @param serviceEndpointName The YARP name to be assigned to the new service.
             @param servicePortNumber The channel being used by the service. */
            BaseService(const ServiceKind             theKind,
                        const yarp::os::ConstString & launchPath,
                        const yarp::os::ConstString & tag,
                        const bool                    useMultipleHandlers,
                        const yarp::os::ConstString & canonicalName,
                        const yarp::os::ConstString & description,
                        const yarp::os::ConstString & requestsDescription,
                        const yarp::os::ConstString & serviceEndpointName,
                        const yarp::os::ConstString & servicePortNumber = "");
            
            /*! @brief The constructor.
             @param theKind The behavioural model for the service.
             @param launchPath The command-line name used to launch the service.
             @param tag The modifier for the service name.
             @param useMultipleHandlers @c true if simultaneous handlers are allowed, @c false if
             one handler is used.
             @param canonicalName The channel-independent name of the service.
             @param description The description of the service.
             @param requestsDescription The description of the requests for the service.
             @param argc The number of arguments in 'argv'.
             @param argv The arguments to be used to specify the new service. */
            BaseService(const ServiceKind             theKind,
                        const yarp::os::ConstString & launchPath,
                        const yarp::os::ConstString & tag,
                        const bool                    useMultipleHandlers,
                        const yarp::os::ConstString & canonicalName,
                        const yarp::os::ConstString & description,
                        const yarp::os::ConstString & requestsDescription,
                        const int                     argc,
                        char **                       argv);
            
            /*! @brief The destructor. */
            virtual ~BaseService(void);
            
            /*! @brief Return the description of the service.
             @returns The description of the service. */
            inline yarp::os::ConstString description(void)
            const
            {
                return _description;
            } // description
            
            /*! @brief Forget the specified client.
             @param key The client-provided key. */
            void detachClient(const yarp::os::ConstString & key);
            
            /*! @brief Fill in a list of clients for the service.
             @param clients The list to be filled in. */
            void fillInClientList(StringVector & clients);
            
            /*! @brief Fill in a list of secondary input channels for the service.
             @param channels The list of channels to be filled in. */
            virtual void fillInSecondaryInputChannelsList(ChannelVector & channels);
            
            /*! @brief Fill in a list of secondary output channels for the service.
             @param channels The list of channels to be filled in. */
            virtual void fillInSecondaryOutputChannelsList(ChannelVector & channels);
            
            /*! @brief Return the associated endpoint.
             @returns The associated endpoint. */
            inline Endpoint & getEndpoint(void)
            const
            {
                return *_endpoint;
            } // getEndpoint
            
            /*! @brief Return the request statistics.
             @param count The number of requests since the service started.
             @param currentTime The current service time. */
            void getStatistics(int64_t & count,
                               double &  currentTime);
            
            /*! @brief Return the state of the service.
             @returns @c true if the service has been started and @c false otherwise. */
            inline bool isStarted(void)
            const
            {
                return _started;
            } // isStarted
            
            /*! @brief Return the behavioural model for the service.
             @returns The behavioural model for the service. */
            inline ServiceKind kind(void)
            const
            {
                return _kind;
            } // kind
            
            /*! @brief Return the command-line name used to launch the service.
             @returns The command-line name used to launch the service. */
            inline yarp::os::ConstString launchPath(void)
            const
            {
                return _launchPath;
            } // launchPath
            
            /*! @brief Process partially-structured input data.
             @param request The requested operation.
             @param restOfInput The arguments for the operation.
             @param senderChannel The name of the channel used to send the input data.
             @param replyMechanism @c nullptr if no reply is expected and non-@c nullptr otherwise.
             @returns @c true if the input was correctly structured and successfully processed. */
            bool processRequest(const yarp::os::ConstString & request,
                                const yarp::os::Bottle &      restOfInput,
                                const yarp::os::ConstString & senderChannel,
                                yarp::os::ConnectionWriter *  replyMechanism);
            
            /*! @brief Return the description of the requests for the service.
             @returns The description of the requests for the service. */
            inline yarp::os::ConstString requestsDescription(void)
            const
            {
                return _requestsDescription;
            } // requestsDescription
            
            /*! @brief Send a 'ping' on behalf of a service.
             @param channelName The service channel to report with the ping.
             @param checker A function that provides for early exit from loops.
             @param checkStuff The private data for the early exit function. */
            static bool SendPingForChannel(const yarp::os::ConstString & channelName,
                                           CheckFunction                 checker,
                                           void *                        checkStuff);
            
            /*! @brief Return the working name of the service.
             @returns The working name of the service. */
            inline yarp::os::ConstString serviceName(void)
            const
            {
                return _serviceName;
            } // serviceName
            
            /*! @brief Start processing requests.
             @returns @c true if the service was started and @c false if it was not. */
            virtual bool start(void);
            
            /*! @brief Start the background 'pinging' thread. */
            void startPinger(void);
            
            /*! @brief Stop processing requests.
             @returns @c true if the service was stopped and @c false it if was not. */
            virtual bool stop(void);
            
        protected:
            
            /*! @brief Add a context for a persistent connection.
             @param key The name for the context.
             @param context The context to be remembered. */
            void addContext(const yarp::os::ConstString & key,
                            BaseContext *                 context);
            
            /*! @brief Remove all contexts. */
            void clearContexts(void);
            
            /*! @brief Locate the context corresponding to a name.
             @param key The name of the context.
             @returns @c nullptr if the named context could not be found or a pointer to the context
             if found. */
            BaseContext * findContext(const yarp::os::ConstString & key);
            
            /*! @brief Remember the function to be used to handle a particular request.
             @param handler The function to be called for the request. */
            void registerRequestHandler(BaseRequestHandler * handler);
            
            /*! @brief Remove a context.
             @param key The name of the context. */
            void removeContext(const yarp::os::ConstString & key);
            
            /*! @brief Remember the function to be used to handle unrecognized requests.
             @param handler The function to be called by default. */
            void setDefaultRequestHandler(BaseRequestHandler * handler);
            
            /*! @brief Forget the function to be used to handle a particular request.
             @param handler The function that was called for the request. */
            void unregisterRequestHandler(BaseRequestHandler * handler);
            
        private:
            
            /*! @brief A mapping from strings to contexts. */
            typedef std::map<yarp::os::ConstString, BaseContext *> ContextMap;
            
            /*! @brief The entry-type for the mapping. */
            typedef ContextMap::value_type ContextMapValue;
            
            /*! @brief Copy constructor.
             
             Note - not implemented and private, to prevent unexpected copying.
             @param other Another object to construct from. */
            BaseService(const BaseService & other);
            
            /*! @brief Assignment operator.
             
             Note - not implemented and private, to prevent unexpected copying.
             @param other Another object to construct from. */
            BaseService & operator =(const BaseService & other);
            
            /*! @brief Enable the standard request handlers. */
            void attachRequestHandlers(void);
            
            /*! @brief Lock the data unless the lock would block.
             @returns @c true if the data was locked and @c false otherwise. */
            inline bool conditionallyLockContexts(void)
            {
                return _contextsLock.tryLock();
            } // conditionallyLockContexts
            
            /*! @brief Disable the standard request handlers. */
            void detachRequestHandlers(void);
            
            /*! @brief Lock the data. */
            inline void lockContexts(void)
            {
                _contextsLock.lock();
            } // lockContexts
            
            /*! @brief Unlock the data. */
            inline void unlockContexts(void)
            {
                _contextsLock.unlock();
            } // unlockContexts
            
            /*! @brief The command-line name used to launch the service. */
            yarp::os::ConstString _launchPath;
            
            /*! @brief The contention lock used to avoid inconsistencies. */
            yarp::os::Mutex _contextsLock;
            
            /*! @brief The map between requests and request handlers. */
            RequestMap _requestHandlers;
            
            /*! @brief The map between requests and request handlers. */
            ContextMap _contexts;
            
            /*! @brief The description of the service. */
            yarp::os::ConstString _description;
            
            /*! @brief The description of the requests for the service. */
            yarp::os::ConstString _requestsDescription;
            
            /*! @brief The channel-independent name of the service. */
            yarp::os::ConstString _serviceName;
            
            /*! @brief The number of requests seen. */
            int64_t _requestCount;
            
            /*! @brief The request handler for the 'channels' request. */
            ChannelsRequestHandler * _channelsHandler;
            
            /*! @brief The request handler for the 'clients' request. */
            ClientsRequestHandler * _clientsHandler;
            
            /*! @brief The request handler for the 'count' request. */
            CountRequestHandler * _countHandler;
            
            /*! @brief The request handler for the 'detach' request. */
            DetachRequestHandler * _detachHandler;
            
            /*! @brief The request handler for the 'info' request. */
            InfoRequestHandler * _infoHandler;
            
            /*! @brief The request handler for the 'list' request. */
            ListRequestHandler * _listHandler;
            
            /*! @brief The request handler for the 'name' request. */
            NameRequestHandler * _nameHandler;
            
            /*! @brief The connection point for the service. */
            Endpoint * _endpoint;
            
            /*! @brief The input handler for the service. */
            BaseServiceInputHandler * _handler;
            
            /*! @brief The input handler creator for the service. */
            BaseServiceInputHandlerCreator * _handlerCreator;
            
            /*! @brief The object used to generate 'pings' for the service. */
            PingThread * _pinger;
            
            /*! @brief The kind of service. */
            ServiceKind _kind;
            
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
            char _filler[2];
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)
            
        }; // BaseService
        
        /*! @brief Register a local service with a running Service Registry service.
         @param channelName The channel provided by the service.
         @param checker A function that provides for early exit from loops.
         @param checkStuff The private data for the early exit function.
         @returns @c true if the service was successfully registered and @c false otherwise. */
        bool RegisterLocalService(const yarp::os::ConstString & channelName,
                                  CheckFunction                 checker,
                                  void *                        checkStuff);
        
        /*! @brief Unregister a local service with a running Service Registry service.
         @param channelName The channel provided by the service.
         @param checker A function that provides for early exit from loops.
         @param checkStuff The private data for the early exit function.
         @returns @c true if the service was successfully unregistered and @c false otherwise. */
        bool UnregisterLocalService(const yarp::os::ConstString & channelName,
                                    CheckFunction                 checker,
                                    void *                        checkStuff);
        
    } // Common
    
} // MplusM

#endif // ! defined(MpMBaseService_H_)
