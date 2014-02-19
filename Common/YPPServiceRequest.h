//
//  YPPServiceRequest.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-06.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPSERVICEREQUEST_H_))
# define YPPSERVICEREQUEST_H_ /* */

# include "YPPServiceResponse.h"

namespace YarpPlusPlus
{
    class Endpoint;
    
    /*! @brief The data constituting a service request. */
    class ServiceRequest
    {
    public:
        
        /*! @brief The constructor.
         @param requestName The request to be processed.
         @param parameters The (optional) parameters for the request. */
        ServiceRequest(const yarp::os::ConstString & requestName,
                       const ParameterType *         parameters = NULL);
        
        /*! @brief The destructor. */
        virtual ~ServiceRequest(void);
        
        /*! @brief Send the request to an endpoint for processing.
         @param destination The endpoint that is to receive the request.
         @param response The response from the request, @c NULL if none is expected.
         @returns @c true if the request was successfully transmitted. */
        bool send(Endpoint &        destination,
                  ServiceResponse * response = NULL);
        
    protected:
        
    private:
        
        /*! @brief The request name. */
        yarp::os::ConstString _name;
        /*! @brief The request parameters. */
        ParameterType         _parameters;
        
    }; // ServiceRequest
    
} // YarpPlusPlus

#endif // ! defined(YPPSERVICEREQUEST_H_)
