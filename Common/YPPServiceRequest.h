//
//  YPPServiceRequest.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-06.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPSERVICEREQUEST_H_))
# define YPPSERVICEREQUEST_H_ /* */

namespace YarpPlusPlus
{
    /*! @brief The data constituting a service request. */
    class ServiceRequest
    {
    public:
        
        /*! @brief The constructor. */
        ServiceRequest(void);
        
        /*! @brief The destructor. */
        virtual ~ServiceRequest(void);
        
    protected:
        
    private:
        
    }; // ServiceRequest
    
} // YarpPlusPlus

#endif // ! defined(YPPSERVICEREQUEST_H_)
