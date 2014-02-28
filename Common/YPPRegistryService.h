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

namespace YarpPlusPlus
{
    /*! @brief The Yarp++ Service Registry service. */
    class RegistryService : BaseService
    {
    public:
        
        /*! @brief The constructor.
         @param serviceEndpointName The YARP name to be assigned to the new service.
         @param serviceHostName The name or IP address of the machine running the service.
         @param servicePortNumber The port being used by the service. */
        RegistryService(const yarp::os::ConstString & serviceEndpointName,
                        const yarp::os::ConstString & serviceHostName = "",
                        const yarp::os::ConstString & servicePortNumber = "");
        
        /*! @brief The constructor.
         @param argc The number of arguments in 'argv'.
         @param argv The arguments to be used to specify the new service. */
        RegistryService(const int argc,
                        char **   argv);
        
        /*! @brief The destructor. */
        virtual ~RegistryService(void);
        
        /*! @brief Start processing requests.
         @returns @c true if the service was started and @c false if it was not. */
        virtual bool start(void);
        
        /*! @brief Stop processing requests.
         @returns @c true if the service was stopped and @c false it if was not. */
        virtual bool stop(void);

    protected:
        
    private:
        
        typedef BaseService inherited;

        /*! @brief Copy constructor.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        RegistryService(const RegistryService & other);
        
        /*! @brief Assignment operator.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        RegistryService & operator=(const RegistryService & other);
        
    }; // RegistryService

} // YarpPlusPlus

#endif // ! defined(YPPREGISTRYSERVICE_H_)
