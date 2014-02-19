//
//  YPPServiceResponse.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-06.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPSERVICERESPONSE_H_))
# define YPPSERVICERESPONSE_H_ /* */

# include "YPPCommon.h"
# include <yarp/os/Bottle.h>

namespace YarpPlusPlus
{
    /*! @brief The data returned from a service request. */
    class ServiceResponse
    {
    public:
        
        /*! @brief The constructor.
         @param values The (optional) values for the response. */
        ServiceResponse(const ParameterType * values = NULL);
        
        /*! @brief The constructor.
         @param values The (optional) values for the response. */
        ServiceResponse(const yarp::os::Bottle & values);

        /*! @brief The destructor. */
        virtual ~ServiceResponse(void);
        
        /*! @brief The assignment operator.
         @param values The (optional) values for the response. */
        ServiceResponse & operator=(const yarp::os::Bottle & values);
        
        /*! @brief The number of values in the response.
         @returns The number of values in the response. */
        size_t count(void)
        const;
        
        /*! @brief Fetch an element from the values.
         @param index The @c 0-based index of the desired element.
         @returns The element corresponding to the provided index. */
        yarp::os::ConstString element(const size_t index)
        const;
        
    protected:
        
    private:
        
        /*! @brief The response values. */
        ParameterType _values;

    }; // ServiceResponse
    
} // YarpPlusPlus

#endif // ! defined(YPPSERVICERESPONSE_H_)
