//
//  YPPRegisterRequestHandler.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-03-03.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPREGISTERREQUESTHANDLER_H_))
# define YPPREGISTERREQUESTHANDLER_H_ /* */

# include "YPPRequestHandler.h"
# include "YPPServiceResponse.h"

namespace YarpPlusPlus
{
    class RegistryService;
    
    /*! @brief The standard 'register' request handler.
     
     The input is the name of a service port and the output is either 'OK', which indicates success, or 'FAILED'
     followed with a description of the reason for failure. */
    class RegisterRequestHandler : public RequestHandler
    {
    public:
        
        /*! @brief The constructor.
         @param service The service that has registered this request. */
        RegisterRequestHandler(RegistryService & service);
        
        /*! @brief The destructor. */
        virtual ~RegisterRequestHandler(void);
        
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
        RegisterRequestHandler(const RegisterRequestHandler & other);
        
        /*! @brief Assignment operator.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        RegisterRequestHandler & operator=(const RegisterRequestHandler & other);
        
        /*! @brief Check the response from the 'list' request.
         @param portName The port that sent the response.
         @param response The response to be analyzed.
         @returns @c true if the expected values are all present and @c false if they are not or if unexpected values
         appear. */
        bool processListResponse(const yarp::os::ConstString & portName,
                                 const ServiceResponse &       response);

        /*! @brief The service that will handle the registration operation. */
        RegistryService & _service;
        
    }; // RegisterRequestHandler
    
} // YarpPlusPlus

#endif // ! defined(YPPREGISTERREQUESTHANDLER_H_)
