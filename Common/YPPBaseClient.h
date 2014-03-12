//
//  YPPBaseClient.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-06.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPBASECLIENT_H_))
# define YPPBASECLIENT_H_ /* */

# include "YPPConfig.h"
# include <yarp/os/Bottle.h>

namespace YarpPlusPlus
{
    class ServiceResponse;
    
    /*! @brief The minimal functionality required for a Yarp++ client. */
    class BaseClient
    {
    public:
        
        /*! @brief The number of elements expected in the output of FindMatchingServices. */
        static const int kExpectedResponseSize;
        
        /*! @brief The constructor. */
        BaseClient(void);
        
        /*! @brief The destructor. */
        virtual ~BaseClient(void);
        
        bool connect(const char * criteria,
                     const bool   allowOnlyOneMatch = false);
        
    protected:
        
        bool send(const yarp::os::Bottle & request,
                  ServiceResponse *        response);

    private:
        
        /*! @brief Copy constructor.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        BaseClient(const BaseClient & other);
        
        /*! @brief Assignment operator.

         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        BaseClient & operator=(const BaseClient & other);
        
        /*! @brief The name of the service port being used. */
        yarp::os::ConstString _servicePort;
        
    }; // BaseClient
    
    /*! @brief Find one or more matching services that are registered with a running Service Registry service.
     @param criteria The matching conditions.
     @returns A (possibly empty) list of matching services. */
    yarp::os::Bottle FindMatchingServices(const char * criteria);
    
} // YarpPlusPlus

#endif // ! defined(YPPBASECLIENT_H_)
