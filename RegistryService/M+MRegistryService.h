//--------------------------------------------------------------------------------------------------
//
//  File:       M+MRegistryService.h
//
//  Project:    M+M
//
//  Contains:   The class declaration for the Service Registry M+M service.
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

#if (! defined(MpMRegistryService_H_))
# define MpMRegistryService_H_  /* Header guard */

# include "M+MBaseService.h"

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 
 @brief The class declaration for the Service Registry M+M service. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

struct sqlite3;

namespace MplusM
{
    namespace Common
    {
        class GeneralChannel;
    } // Common
    
    namespace Parser
    {
        class MatchExpression;
    } // Parser
    
    namespace Registry
    {
        class AssociateRequestHandler;
        class ColumnNameValidator;
        class DisassociateRequestHandler;
        class GetAssociatesRequestHandler;
        class MatchRequestHandler;
        class PingRequestHandler;
        class RegisterRequestHandler;
        class RegistryCheckThread;
        class UnregisterRequestHandler;
        
        /*! @brief The characteristics of a request. */
        struct RequestDescription
        {
            /*! @brief The details of the request. */
            yarp::os::ConstString _details;
            
            /*! @brief The inputs descriptor for the request. */
            yarp::os::ConstString _inputs;
            
            /*! @brief The outputs descriptor for the request. */
            yarp::os::ConstString _outputs;
            
            /*! @brief The service channel for the request. */
            yarp::os::ConstString _channel;
            
            /*! @brief The name of the request. */
            yarp::os::ConstString _request;
            
            /*! @brief The version of the request. */
            yarp::os::ConstString _version;
            
        }; // RequestDescription
        
        /*! @brief The M+M Service Registry service. */
        class RegistryService : public Common::BaseService
        {
        public:
            
            /*! @brief The constructor.
             @param launchPath The command-line name used to launch the service.
             @param useInMemoryDb @c true if the database is in-memory and @c false if a temporary
             disk file is to be used.
             @param servicePortNumber The port being used by the service. */
            RegistryService(const yarp::os::ConstString & launchPath,
                            const bool                    useInMemoryDb = false,
                            const yarp::os::ConstString & servicePortNumber = "");
            
            /*! @brief The destructor. */
            virtual ~RegistryService(void);
            
            /*! @brief Add an association between channels.
             @param primaryChannelName The name of the primary channel.
             @param isOutput @c true if the secondary channel is an output and @c false otherwise.
             @param secondaryChannelName The name of the associated channel.
             @returns @c true if the association can be added and @c false otherwise.*/
            bool addAssociation(const yarp::os::ConstString & primaryChannelName,
                                const bool                    isOutput,
                                const yarp::os::ConstString & secondaryChannelName);
            
            /*! @brief Add a request to the registry.
             @param keywordList The list of keywords associated with the request.
             @param description The attributes of the request.
             @returns @c true if the request was successfully added and @c false otherwise. */
            bool addRequestRecord(const yarp::os::Bottle &   keywordList,
                                  const RequestDescription & description);
            
            /*! @brief Add a service to the registry.
             @param channelName The service channel for the service.
             @param name The canonical name for the service.
             @param description The description of the service.
             @param executable The path to the executable for the service.
             @param requestsDescription The description of the requests for the service.
             @returns @c true if the request was successfully added and @c false otherwise. */
            bool addServiceRecord(const yarp::os::ConstString & channelName,
                                  const yarp::os::ConstString & name,
                                  const yarp::os::ConstString & description,
                                  const yarp::os::ConstString & executable,
                                  const yarp::os::ConstString & requestsDescription);
            
            /*! @brief Check if a service is already in the registry.
             @param channelName The service channel for the service.
             @returns @c true if the service is present and @c false otherwise. */
            bool checkForExistingService(const yarp::os::ConstString & channelName);
            
            /*! @brief Check for expired services. */
            void checkServiceTimes(void);
            
            /*! @brief Fill in the list of associated input channels and output channesls for a
             channel.
             @param channelName The channel to be checked.
             @param isPrimary @c true if the channel is a primary and @c false if it is an
             associated channel
             @param inputs The associated list of input channels to be filled in.
             @param outputs The associated list of output channels to be filled in.
             @returns @c true if the lists were successfully filled and @c false otherwise. */
            bool fillInAssociates(const yarp::os::ConstString & channelName,
                                  bool &                        isPrimary,
                                  Common::StringVector &        inputs,
                                  Common::StringVector &        outputs);
            
            /*! @brief Fill in a list of secondary output channels for the service.
             @param channels The list of channels to be filled in. */
            virtual void fillInSecondaryOutputChannelsList(Common::ChannelVector & channels);
            
            /*! @brief Return @c true if the service is active.
             @returns @c true if the service is active and @c false otherwise. */
            inline bool isActive(void)
            const
            {
                return _isActive;
            } // isActive
            
            /*! @brief Convert a match expression into SQL and process it.
             @param matcher The match expression to be processed.
             @param getNames @c true if service names are to be returned and @c false if service
             ports are to be returned.
             @param reply The result from performing a SELECT with the converted match expression.
             @returns @c true if the match request was successfully performed and @c false
             otherwise. */
            bool processMatchRequest(Parser::MatchExpression * matcher,
                                     const bool                getNames,
                                     yarp::os::Bottle &        reply);
            
            /*! @brief Remove all associations for a channel.
             @param primaryChannelName The name of the primary channel.
             @returns @c true if the associations were removed and @c false otherwise.*/
            bool removeAllAssociations(const yarp::os::ConstString & primaryChannelName);
            
            /*! @brief Remove the last checked time for a service channel.
             @param serviceChannelName The service channel that is being removed. */
            void removeCheckedTimeForChannel(const yarp::os::ConstString & serviceChannelName);
            
            /*! @brief Remove a service entry from the registry.
             @param serviceChannelName The service channel that is being removed.
             @returns @c true if the service was successfully removed and @c false otherwise. */
            bool removeServiceRecord(const yarp::os::ConstString & serviceChannelName);
            
            /*! @brief Start processing requests.
             @returns @c true if the service was started and @c false if it was not. */
            virtual bool start(void);
            
            /*! @brief Start the background 'checking' thread. */
            void startChecker(void);
            
            /*! @brief Stop processing requests.
             @returns @c true if the service was stopped and @c false it if was not. */
            virtual bool stop(void);
            
            /*! @brief Update the last checked time for a service channel.
             @param serviceChannelName The service channel that is being updated. */
            void updateCheckedTimeForChannel(const yarp::os::ConstString & serviceChannelName);
            
        protected:
            
        private:
            
            /*! @brief The class that this class is derived from. */
            typedef BaseService inherited;
            
            /*! @brief The current state of the service. */
            enum ServiceStatus
            {
                /*! @brief The registry has just started. */
                kRegistryStarted,
                
                /*! @brief The registry is stopping. */
                kRegistryStopped,
                
                /*! @brief A service is being added to the registry. */
                kRegistryAddService,
                
                /*! @brief A service is being removed from the registry. */
                kRegistryRemoveService
                
            }; // ServiceStatus
            
            /*! @brief A mapping from strings to time values. */
            typedef std::map<yarp::os::ConstString, double> TimeMap;
            
            /*! @brief The constructor.
             @param argc The number of arguments in 'argv'.
             @param argv The arguments to be used to specify the new service. */
            RegistryService(const int argc,
                            char **   argv);
            
            /*! @brief Copy constructor.
             
             Note - not implemented and private, to prevent unexpected copying.
             @param other Another object to construct from. */
            RegistryService(const RegistryService & other);
            
            /*! @brief Assignment operator.
             
             Note - not implemented and private, to prevent unexpected copying.
             @param other Another object to construct from. */
            RegistryService & operator =(const RegistryService & other);
            
            /*! @brief Enable the standard request handlers. */
            void attachRequestHandlers(void);
            
            /*! @brief Disable the standard request handlers. */
            void detachRequestHandlers(void);
            
            /*! @brief Report a change to a service.
             @param channelName The service channel for the service.
             @param newStatus The updated state of the service. */
            void reportStatusChange(const yarp::os::ConstString & channelName,
                                    const ServiceStatus           newStatus);
            
            /*! @brief Set up the service registry database.
             @returns @c true if the database was set up and @c false otherwise. */
            bool setUpDatabase(void);
            
            /*! @brief Set up the status reporting channel.
             @returns @c true if the channel was set up and @c false otherwise. */
            bool setUpStatusChannel(void);
            
            /*! @brief The last time that a channel 'checked-in'. */
            TimeMap _lastCheckedTime;
            
            /*! @brief The contention lock used to avoid inconsistencies. */
            yarp::os::Mutex _checkedTimeLock;
            
            /*! @brief The service registry database. */
            sqlite3 * _db;
            
            /*! @brief The validator function object that the Service Registry will use. */
            ColumnNameValidator * _validator;
            
            /*! @brief The request handler for the 'associate' request. */
            AssociateRequestHandler * _associateHandler;
            
            /*! @brief The request handler for the 'disassociate' request. */
            DisassociateRequestHandler * _disassociateHandler;
            
            /*! @brief The request handler for the 'disassociate' request. */
            GetAssociatesRequestHandler * _getAssociatesHandler;
            
            /*! @brief The request handler for the 'match' request. */
            MatchRequestHandler * _matchHandler;
            
            /*! @brief The request handler for the 'ping' request. */
            PingRequestHandler * _pingHandler;
            
            /*! @brief The channel to send status change messages to. */
            Common::GeneralChannel * _statusChannel;
            
            /*! @brief The request handler for the 'register' request. */
            RegisterRequestHandler * _registerHandler;
            
            /*! @brief The request handler for the 'unregister' request. */
            UnregisterRequestHandler * _unregisterHandler;
            
            /*! @brief The object used to generate 'checks' for the service. */
            RegistryCheckThread * _checker;
            
            /*! @brief @c true if the database is in-memory and @c false if it is disk-based. */
            bool _inMemory;
            
            /*! @brief @c true if the registry service is fully operational and @c false if it could
             not be set up. */
            bool _isActive;
            
# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunused-private-field"
# endif // defined(__APPLE__)
            /*! @brief Filler to pad to alignment boundary */
            char _filler[6];
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)
            
        }; // RegistryService
        
    } // Registry
    
} // MplusM

#endif // ! defined(MpMRegistryService_H_)
