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
         @param serviceHostName The name or IP address of the machine running the service.
         @param servicePortNumber The port being used by the service. */
        RegistryService(const yarp::os::ConstString & serviceHostName = "",
                        const yarp::os::ConstString & servicePortNumber = "");
        
        /*! @brief The destructor. */
        virtual ~RegistryService(void);
        
        inline bool isActive(void)
        const
        {
            return _isActive;
        } // isActive
        
        /*! @brief Start processing requests.
         @returns @c true if the service was started and @c false if it was not. */
        virtual bool start(void);
        
        /*! @brief Stop processing requests.
         @returns @c true if the service was stopped and @c false it if was not. */
        virtual bool stop(void);

        /*! @brief Register a local service with a running Service Registry service.
         @param portName The port provided by the service.
         @returns @c true if the service was successfully registered and @c false otherwise. */
        static bool registerLocalService(const yarp::os::ConstString & portName);
        
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
        
        /*! @brief Set up the standard request handlers. */
        void setUpRequestHandlers(void);
        
        /*! @brief @c true if the registry service is fully operational and @c false if it could not be set up. */
        bool _isActive;

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunused-private-field"
# endif // defined(__APPLE__)
        /*! @brief Filler to pad to alignment boundary */
        char _filler[7];
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)
        
    }; // RegistryService

} // YarpPlusPlus

#endif // ! defined(YPPREGISTRYSERVICE_H_)
