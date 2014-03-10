//
//  YPPMatchRequestHandler.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-03-10.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPMATCHREQUESTHANDLER_H_))
# define YPPMATCHREQUESTHANDLER_H_ /* */

# include "YPPRequestHandler.h"
# include "YPPServiceResponse.h"

namespace YarpPlusPlus
{
    class RegistryService;
    
    /*! @brief The 'register' request handler. */
    class MatchRequestHandler : public RequestHandler
    {
    public:
        
        /*! @brief The constructor.
         @param service The service that has registered this request.
         @param validator The field validator to use. */
        MatchRequestHandler(RegistryService &  service,
                            FieldNameValidator validator = NULL);
        
        /*! @brief The destructor. */
        virtual ~MatchRequestHandler(void);
        
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
        
        typedef RequestHandler inherited;
        
        /*! @brief Copy constructor.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        MatchRequestHandler(const MatchRequestHandler & other);
        
        /*! @brief Assignment operator.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        MatchRequestHandler & operator=(const MatchRequestHandler & other);
        
        /*! @brief The service that will handle the registration operation. */
        RegistryService &  _service;
        
        /*! @brief The field name validator to be used. */
        FieldNameValidator _validator;
        
    }; // MatchRequestHandler
    
} // YarpPlusPlus

#endif // ! defined(YPPMATCHREQUESTHANDLER_H_)
