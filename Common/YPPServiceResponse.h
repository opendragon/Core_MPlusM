//
//  YPPServiceResponse.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-06.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPSERVICERESPONSE_H_))
# define YPPSERVICERESPONSE_H_ /* */

# include "YPPConfig.h"

namespace YarpPlusPlus
{
    /*! @brief The data returned from a service request. */
    class ServiceResponse
    {
    public:
        
        /*! @brief The constructor. */
        ServiceResponse(void);
        
        /*! @brief The destructor. */
        virtual ~ServiceResponse(void);
        
    protected:
        
    private:
        
    }; // ServiceResponse
    
} // YarpPlusPlus

#endif // ! defined(YPPSERVICERESPONSE_H_)
