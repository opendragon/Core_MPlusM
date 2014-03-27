//--------------------------------------------------------------------------------------
//
//  File:       YPPRegistryService.h
//
//  Project:    YarpPlusPlus
//
//  Contains:   The class declaration for the Service Registry Yarp++ service.
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
//  Created:    2014-02-06
//
//--------------------------------------------------------------------------------------

#if (! defined(YPPREGISTRYSERVICE_H_))
/*! @brief Header guard. */
# define YPPREGISTRYSERVICE_H_ /* */

# include "YPPBaseService.h"
# include "YPPMatchExpression.h"

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 
 @brief The class declaration for the Service Registry Yarp++ service. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

struct sqlite3;

namespace YarpPlusPlus
{
    class ColumnNameValidator;
    class MatchRequestHandler;
    class RegisterRequestHandler;
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
        /*! @brief The service port for the request. */
        yarp::os::ConstString _port;
        /*! @brief The name of the request. */
        yarp::os::ConstString _request;
        /*! @brief The version of the request. */
        yarp::os::ConstString _version;
    }; // RequestDescription
    
    /*! @brief The Yarp++ Service Registry service. */
    class RegistryService : public BaseService
    {
    public:
        
        /*! @brief The constructor.
         @param useInMemoryDb @c true if the database is in-memory and @c false if a temporary disk file is to be used.
         @param serviceHostName The name or IP address of the machine running the service.
         @param servicePortNumber The port being used by the service. */
        RegistryService(const bool                    useInMemoryDb = false,
                        const yarp::os::ConstString & serviceHostName = "",
                        const yarp::os::ConstString & servicePortNumber = "");
        
        /*! @brief The destructor. */
        virtual ~RegistryService(void);
        
        /*! @brief Add a request to the registry.
         @param keywordList The list of keywords associated with the request.
         @param description The attributes of the request.
         @returns @c true if the request was successfully added and @c false otherwise. */
        bool addRequestRecord(const yarp::os::Bottle &   keywordList,
                              const RequestDescription & description);
        
        /*! @brief Add a service to the registry.
         @param portName The service port for the service.
         @param name The canonical name for the service.
         @param description The description of the service.
         @returns @c true if the request was successfully added and @c false otherwise. */
        bool addServiceRecord(const yarp::os::ConstString & portName,
                              const yarp::os::ConstString & name,
                              const yarp::os::ConstString & description);
        
        /*! @brief Return @c true if the service is active.
         @returns @c true if the service is active and @c false otherwise. */
        inline bool isActive(void)
        const
        {
            return _isActive;
        } // isActive
        
        /*! @brief Convert a match expression into SQL and process it.
         @param matcher The match expression to be processed.
         @param reply The result from performing a SELECT with the converted match expression.
         @returns @c true if the match request was successfully performed and @c false otherwise. */
        bool processMatchRequest(YarpPlusPlusParser::MatchExpression * matcher,
                                 yarp::os::Bottle &                    reply);
        
        /*! @brief Remove a service entry from the registry.
         @param servicePortName The service port that is being removed.
         @returns @c true if the service was successfully removed and @c false otherwise. */
        bool removeServiceRecord(const yarp::os::ConstString & servicePortName);
        
        /*! @brief Start processing requests.
         @returns @c true if the service was started and @c false if it was not. */
        virtual bool start(void);
        
        /*! @brief Stop processing requests.
         @returns @c true if the service was stopped and @c false it if was not. */
        virtual bool stop(void);

    protected:
        
    private:
        
        /*! @brief The class that this class is derived from. */
        typedef BaseService inherited;

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
        RegistryService & operator=(const RegistryService & other);
        
        /*! @brief Enable the standard request handlers. */
        void attachRequestHandlers(void);
        
        /*! @brief Disable the standard request handlers. */
        void detachRequestHandlers(void);
        
        /*! @brief Set up the service registry database.
         @returns @c true if the database was set up and @c false otherwise. */
        bool setUpDatabase(void);
        
        /*! @brief The service registry database. */
        sqlite3 *                  _db;
        
        /*! @brief The validator function object that the Service Registry will use. */
        ColumnNameValidator *      _validator;
        
        /*! @brief The request handler for the 'match' request. */
        MatchRequestHandler *      _matchHandler;
        
        /*! @brief The request handler for the 'register' request. */
        RegisterRequestHandler *   _registerHandler;
        
        /*! @brief The request handler for the 'unregister' request. */
        UnregisterRequestHandler * _unregisterHandler;
        
        /*! @brief @c true if the database is in-memory and @c false if it is disk-based. */
        bool                       _inMemory;
        
        /*! @brief @c true if the registry service is fully operational and @c false if it could not be set up. */
        bool                       _isActive;

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunused-private-field"
# endif // defined(__APPLE__)
        /*! @brief Filler to pad to alignment boundary */
        char      _filler[6];
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)
        
    }; // RegistryService

} // YarpPlusPlus

#endif // ! defined(YPPREGISTRYSERVICE_H_)
