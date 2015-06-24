//--------------------------------------------------------------------------------------------------
//
//  File:       M+MRegistryService.h
//
//  Project:    M+M
//
//  Contains:   The class declaration for the M+M Registry Service.
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

#if (! defined(MpMRegistryService_H_))
# define MpMRegistryService_H_ /* Header guard */

# include <mpm/M+MBaseService.h>
# include <mpm/M+MGeneralChannel.h>
# include <mpm/M+MMatchExpression.h>
# include <mpm/M+MServiceResponse.h>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for the M+M %Registry Service. */

/*! @namespace MplusM::Registry
 @brief The classes that support registering and unregistering services. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

/*! @brief The description of the service. */
# define REGISTRY_SERVICE_DESCRIPTION_ T_("Registry Service")

struct sqlite3;

namespace MplusM
{
    namespace Registry
    {
        class ColumnNameValidator;
        class MatchRequestHandler;
        class PingRequestHandler;
        class RegisterRequestHandler;
        class RegistryCheckThread;
        class UnregisterRequestHandler;
        
        /*! @brief The characteristics of a request. */
        struct RequestDescription
        {
            /*! @brief The details of the request. */
            YarpString _details;
            
            /*! @brief The inputs descriptor for the request. */
            YarpString _inputs;
            
            /*! @brief The outputs descriptor for the request. */
            YarpString _outputs;
            
            /*! @brief The service channel for the request. */
            YarpString _channel;
            
            /*! @brief The name of the request. */
            YarpString _request;
            
            /*! @brief The version of the request. */
            YarpString _version;
            
        }; // RequestDescription
        
        /*! @brief The M+M %Registry Service. */
        class RegistryService : public Common::BaseService
        {
        public :
            
            /*! @brief The current state of the service. */
            enum ServiceStatus
            {
                /*! @brief A service is being added to the registry. */
                kRegistryAddService,
                
                /*! @brief An association was not recognized. */
                kRegistryNotAnExistingAssociation,
                
                /*! @brief A service was not recognized. */
                kRegistryNotAnExistingService,
                
                /*! @brief A service has pinged the registry. */
                kRegistryPingFromService,
                
                /*! @brief A request could not be added to the registry. */
                kRegistryProblemAddingRequest,
                
                /*! @brief A service could not be added to the registry. */
                kRegistryProblemAddingService,
                
                /*! @brief A service is being registered in the registry. */
                kRegistryRegisterService,
                
                /*! @brief A service is being removed from the registry. */
                kRegistryRemoveService,
                
                /*! @brief A service has not pinged the registry recently. */
                kRegistryStaleService,
                
                /*! @brief The registry has just started. */
                kRegistryStarted,
                
                /*! @brief The registry is stopping. */
                kRegistryStopped,
                
                /*! @brief A service is being unregistered from the registry. */
                kRegistryUnregisterService,
                
                /*! @brief Force the enumeration to be 4 bytes. */
                kRegistryUnknown = 0x7FFFFFFF
                
            }; // ServiceStatus
            
            /*! @brief The constructor.
             @param launchPath The command-line name used to launch the service.
             @param argc The number of arguments in 'argv'.
             @param argv The arguments passed to the executable used to launch the service.
             @param useInMemoryDb @c true if the database is in-memory and @c false if a temporary
             disk file is to be used.
             @param servicePortNumber The port being used by the service. */
            RegistryService(const YarpString & launchPath,
                            const int          argc,
                            char * *           argv,
                            const bool         useInMemoryDb = false,
                            const YarpString & servicePortNumber = "");
            
            /*! @brief The destructor. */
            virtual ~RegistryService(void);
            
            /*! @brief Check if a service is already in the registry.
             @param channelName The service channel for the service.
             @returns @c true if the service is present and @c false otherwise. */
            bool checkForExistingService(const YarpString & channelName);
            
            /*! @brief Check for expired services. */
            void checkServiceTimes(void);
            
            /*! @brief Turn off the send / receive metrics collecting. */
            virtual void disableMetrics(void);
            
            /*! @brief Turn on the send / receive metrics collecting. */
            virtual void enableMetrics(void);
            
            /*! @brief Fill in a list of secondary output channels for the service.
             @param channels The list of channels to be filled in. */
            virtual void fillInSecondaryOutputChannelsList(Common::ChannelVector & channels);
            
            /*! @brief Fill in the metrics for the service.
             @param metrics The gathered metrics. */
            virtual void gatherMetrics(yarp::os::Bottle & metrics);
            
            /*! @brief Return @c true if the service is active.
             @returns @c true if the service is active and @c false otherwise. */
            inline bool isActive(void)
            const
            {
                return _isActive;
            } // isActive
            
            /*! @brief Check the response from the 'list' request.
             @param channelName The channel that sent the response.
             @param response The response to be analyzed.
             @returns @c true if the expected values are all present and @c false if they are not or
             if unexpected values appear. */
            bool processListResponse(const YarpString &              channelName,
                                     const Common::ServiceResponse & response);
            
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
            
            /*! @brief Check the response from the 'name' request.
             @param channelName The channel that sent the response.
             @param response The response to be analyzed.
             @returns @c true if the expected values are all present and @c false if they are not or
             if unexpected values appear. */
            bool processNameResponse(const YarpString &              channelName,
                                     const Common::ServiceResponse & response);
            
            /*! @brief Remove the last checked time for a service channel.
             @param serviceChannelName The service channel that is being removed. */
            void removeCheckedTimeForChannel(const YarpString & serviceChannelName);
            
            /*! @brief Remove a service entry from the registry.
             @param serviceChannelName The service channel that is being removed.
             @returns @c true if the service was successfully removed and @c false otherwise. */
            bool removeServiceRecord(const YarpString & serviceChannelName);
            
            /*! @brief Report a change to a service.
             @param channelName The service channel for the service.
             @param newStatus The updated state of the service.
             @param details Details on the change. */
            void reportStatusChange(const YarpString &  channelName,
                                    const ServiceStatus newStatus,
                                    const YarpString &  details = "");
            
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
            void updateCheckedTimeForChannel(const YarpString & serviceChannelName);
            
        protected :
            
        private :
            
            /*! @brief The constructor.
             @param argc The number of arguments in 'argv'.
             @param argv The arguments to be used to specify the new service. */
            RegistryService(const int argc,
                            char * *  argv);
            
            COPY_AND_ASSIGNMENT_(RegistryService);
            
            /*! @brief Add a request to the registry.
             @param keywordList The list of keywords associated with the request.
             @param description The attributes of the request.
             @returns @c true if the request was successfully added and @c false otherwise. */
            bool addRequestRecord(const yarp::os::Bottle &   keywordList,
                                  const RequestDescription & description);
            
            /*! @brief Add a service to the registry.
             @param channelName The service channel for the service.
             @param name The canonical name for the service.
             @param tag The modifier tag for the service.
             @param description The description of the service.
             @param extraInfo The extra information for the service.
             @param executable The path to the executable for the service.
             @param requestsDescription The description of the requests for the service.
             @returns @c true if the request was successfully added and @c false otherwise. */
            bool addServiceRecord(const YarpString & channelName,
                                  const YarpString & name,
                                  const YarpString & tag,
                                  const YarpString & description,
                                  const YarpString & extraInfo,
                                  const YarpString & executable,
                                  const YarpString & requestsDescription);
            
            /*! @brief Enable the standard request handlers. */
            void attachRequestHandlers(void);
            
            /*! @brief Disable the standard request handlers. */
            void detachRequestHandlers(void);
            
            /*! @brief Check the dictionary entry from the 'list' response.
             @param asDict The dictionary to be checked.
             @param channelName The channel that sent the response.
             @returns @c false if an unexpected value appears and @c true otherwise. */
            bool processDictionaryEntry(yarp::os::Property & asDict,
                                        const YarpString &   channelName);
            
            /*! @brief Set up the %Registry Service database.
             @returns @c true if the database was set up and @c false otherwise. */
            bool setUpDatabase(void);
            
            /*! @brief Set up the status reporting channel.
             @returns @c true if the channel was set up and @c false otherwise. */
            bool setUpStatusChannel(void);
            
        public :
        
        protected :
        
        private :
            
            /*! @brief The class that this class is derived from. */
            typedef BaseService inherited;
            
            /*! @brief A mapping from strings to time values. */
            typedef std::map<YarpString, double> TimeMap;
            
            /*! @brief The last time that a channel 'checked-in'. */
            TimeMap _lastCheckedTime;
            
            /*! @brief The contention lock used to avoid inconsistencies. */
            yarp::os::Mutex _checkedTimeLock;
            
            /*! @brief The %Registry Service database. */
            sqlite3 * _db;
            
            /*! @brief The validator function object that the %Registry Service will use. */
            ColumnNameValidator * _validator;
            
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
            
            /*! @brief @c true if the %Registry Service is fully operational and @c false if it
             could not be set up. */
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
