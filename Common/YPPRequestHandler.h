//
//  YPPRequestHandler.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-026.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPREQUESTHANDLER_H_))
# define YPPREQUESTHANDLER_H_ /* */

//# include "YPPConfig.h"
# include <yarp/os/ConstString.h>
# include <yarp/os/Bottle.h>
# include <yarp/os/ConnectionWriter.h>
# include <yarp/os/Property.h>

namespace YarpPlusPlus
{
    class BaseService;

    /*! @brief A convenience class to provide function objects for requests. */
    class RequestHandler
    {
    public:
        
        /*! @brief The constructor.
         @param request The name of the request.
         @param service The service that responds to this request. */
        RequestHandler(const yarp::os::ConstString & request,
                       BaseService &                 service);
        
        /*! @brief The destructor. */
        virtual ~RequestHandler(void);
        
        /*! @brief Fill in a description dictionary for the request.
         @param info The dictionary to be filled in. */
        virtual void fillInDescription(yarp::os::Property & info) = 0;
        
        inline const yarp::os::ConstString & name(void)
        const
        {
            return _name;
        } // name
        
        
        /*! @brief Process a request.
         @param restOfInput The arguments to the operation.
         @param replyMechanism non-@c NULL if a reply is expected and @c NULL otherwise. */
        virtual bool operator() (const yarp::os::Bottle &     restOfInput,
                                 yarp::os::ConnectionWriter * replyMechanism) = 0;
        
    protected:
        
        /*! @brief The service that 'owns' this handler. */
        BaseService & _service;
        
    private:
        
        /*! @brief Copy constructor.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        RequestHandler(const RequestHandler & other);
        
        /*! @brief Assignment operator.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        RequestHandler & operator=(const RequestHandler & other);
        
        /*! @brief The name of the request. */
        yarp::os::ConstString _name;
        
    }; // RequestHandler
    
} // YarpPlusPlus

#endif // ! defined(YPPREQUESTHANDLER_H_)
