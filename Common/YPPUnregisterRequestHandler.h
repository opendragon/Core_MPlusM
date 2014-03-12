//
//  YPPUnregisterRequestHandler.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-03-03.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPUNREGISTERREQUESTHANDLER_H_))
# define YPPUNREGISTERREQUESTHANDLER_H_ /* */

# include "YPPRequestHandler.h"

namespace YarpPlusPlus
{
    class RegistryService;

    /*! @brief The standard 'unregister' request handler.
     
     The input is the name of a service port and the output is either 'OK', which indicates success, or 'FAILED'
     followed with a description of the reason for failure. */
    class UnregisterRequestHandler : public RequestHandler
    {
    public:
        
        /*! @brief The constructor.
         @param service The service that has registered this request. */
        UnregisterRequestHandler(RegistryService & service);
        
        /*! @brief The destructor. */
        virtual ~UnregisterRequestHandler(void);
        
        /*! @brief Fill in a description dictionary for the request.
         @param info The dictionary to be filled in. */
        virtual void fillInDescription(yarp::os::Property & info);
        
        /*! @brief Process a request.
         @param restOfInput The arguments to the operation.
         @param replyMechanism non-@c NULL if a reply is expected and @c NULL otherwise. */
        virtual bool operator() (const yarp::os::Bottle &     restOfInput,
                                 yarp::os::ConnectionWriter * replyMechanism);
        
    protected:
        
    private:
        
        /*! @brief The class that this class is derived from. */
        typedef RequestHandler inherited;
        
        /*! @brief Copy constructor.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        UnregisterRequestHandler(const UnregisterRequestHandler & other);
        
        /*! @brief Assignment operator.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        UnregisterRequestHandler & operator=(const UnregisterRequestHandler & other);
        
        /*! @brief The service that will handle the unregistration operation. */
        RegistryService & _service;
        
    }; // RegisterRequestHandler
    
} // YarpPlusPlus

#endif // ! defined(YPPUNREGISTERREQUESTHANDLER_H_)
