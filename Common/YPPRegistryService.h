//
//  YPPRegistryService.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-06.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPREGISTRYSERVICE_H_))
# define YPPREGISTRYSERVICE_H_ /* */

# include "YPPBaseService.h"
# include "YPPMatchExpression.h"

struct sqlite3;

namespace YarpPlusPlus
{
    /*! @brief The characteristics of a request. */
    struct RequestDescription
    {
        /*! @brief The description of the request. */
        yarp::os::ConstString _description;
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
        
        /*! @brief Set up the service registry database.
         @returns @c true if the database was set up and @c false otherwise. */
        bool setUpDatabase(void);
        
        /*! @brief Set up the standard request handlers. */
        void setUpRequestHandlers(void);
        
        /*! @brief The service registry database. */
        sqlite3 * _db;
        
        /*! @brief @c true if the database is in-memory and @c false if it is disk-based. */
        bool _inMemory;
        
        /*! @brief @c true if the registry service is fully operational and @c false if it could not be set up. */
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

} // YarpPlusPlus

#endif // ! defined(YPPREGISTRYSERVICE_H_)
