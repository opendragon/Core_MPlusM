//
//  YPPBaseService.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-06.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPBASESERVICE_H_))
# define YPPBASESERVICE_H_ /* */

# include "YPPConfig.h"
# include "YPPEndpoint.h"
# include <yarp/os/ConstString.h>

namespace YarpPlusPlus
{
    /*! @brief The minimal functionality required for a Yarp++ service. */
    class BaseService
    {
    public:
        
        /*! @brief The constructor.
         @param serviceEndpointName The YARP name to be assigned to the new service.
         @param serviceHostName The name or IP address of the machine running the service.
         @param servicePortNumber The port being used by the service. */
        BaseService(const yarp::os::ConstString & serviceEndpointName,
                    const yarp::os::ConstString & serviceHostName = "",
                    const yarp::os::ConstString & servicePortNumber = "");
        
        /*! @brief The constructor.
         @param argc The number of arguments in 'argv'.
         @param argv The arguments to be used to specify the new service. */
        BaseService(const int argc,
                    char **   argv);
        
        /*! @brief The destructor. */
        virtual ~BaseService(void);
        
        /*! @brief Return the associated endpoint.
         @returns The associated endpoint. */
        inline Endpoint & getEndpoint(void)
        const
        {
            return *_endpoint;
        } // getEndpoint
        
        /*! @brief Process partially-structured input data.
         @param input The partially-structured input data.
         @param replyMechanism @c NULL if no reply is expected and non-@c NULL otherwise.
         @returns @c true if the input was correctly structured and successfully processed. */
        virtual bool processRequest(yarp::os::Bottle &           input,
                                    yarp::os::ConnectionWriter * replyMechanism) = 0;
        
        /*! @brief Start processing requests.
         @returns @c true if the service was started and @c false if it was not. */
        virtual bool start(void);
        
        /*! @brief Stop processing requests.
         @returns @c true if the service was stopped and @c false it if was not. */
        virtual bool stop(void);
        
    protected:
        
        /*! @brief The current state of the service - @c true if active and @c false otherwise. */
        bool _started;
        
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunused-private-field"
        /*! @brief Filler to pad to alignment boundary */
        char _filler[7];
# pragma clang diagnostic pop

    private:
        
        /*! @brief Copy constructor.

         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        BaseService(const BaseService & other);
        
        /*! @brief Assignment operator.

         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        BaseService & operator=(const BaseService & other);
        
        /*! @brief The connection point for the service. */
        Endpoint * _endpoint;
        
    }; // BaseService
    
} // YarpPlusPlus

#endif // ! defined(YPPBASESERVICE_H_)
