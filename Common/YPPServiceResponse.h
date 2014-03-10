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
        ServiceResponse(const yarp::os::Bottle & values = "");

        /*! @brief The destructor. */
        virtual ~ServiceResponse(void);
        
        /*! @brief The assignment operator.
         @param values The (optional) values for the response. */
        ServiceResponse & operator=(const yarp::os::Bottle & values);
        
        /*! @brief Return a printable version of the response.
         @returns A printable version of the response. */
        yarp::os::ConstString asString(void)
        const;
        
        /*! @brief The number of values in the response.
         @returns The number of values in the response. */
        inline int count(void)
        const
        {
            return _values.size();
        } // count
        
        /*! @brief Fetch an element from the values.
         @param index The @c 0-based index of the desired element.
         @returns The element corresponding to the provided index. */
        yarp::os::Value element(const int index)
        const;
        
        /*! @brief Return the full set of values.
         @returns All the values in the response. */
        yarp::os::Bottle values(void)
        const
        {
            return _values;
        } // values
        
    protected:
        
    private:
        
        /*! @brief Copy constructor.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        ServiceResponse(const ServiceResponse & other);
        
        /*! @brief Assignment operator.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        ServiceResponse & operator=(const ServiceResponse & other);
        
        /*! @brief The response values. */
        yarp::os::Bottle _values;

    }; // ServiceResponse
    
} // YarpPlusPlus

#endif // ! defined(YPPSERVICERESPONSE_H_)
