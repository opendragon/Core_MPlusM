//
//  YPPTTest09DefaultRequestHandler.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-28.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPTTEST09DEFAULTREQUESTHANDLER_H_))
# define YPPTTEST09DEFAULTREQUESTHANDLER_H_ /* */

# include "../YPPRequestHandler.h"

namespace YarpPlusPlusTest
{
    /*! @brief A test request handler. */
    class Test09DefaultRequestHandler : public YarpPlusPlus::RequestHandler
    {
    public:
        
        /*! @brief The constructor.
         @param service The service that responds to this request. */
        Test09DefaultRequestHandler(YarpPlusPlus::BaseService & service);
        
        /*! @brief The destructor. */
        virtual ~Test09DefaultRequestHandler(void);
        
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
        
    }; // Test09DefaultRequestHandler
    
} // YarpPlusPlusTest

#endif // ! defined(YPPTTEST09DEFAULTREQUESTHANDLER_H_)
