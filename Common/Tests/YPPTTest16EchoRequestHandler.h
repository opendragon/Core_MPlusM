//
//  YPPTTest16EchoRequestHandler.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-03-06.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPTTEST16ECHOREQUESTHANDLER_H_))
# define YPPTTEST16ECHOREQUESTHANDLER_H_ /* */

# include "../YPPRequestHandler.h"

namespace YarpPlusPlusTest
{
    /*! @brief A test request handler. */
    class Test16EchoRequestHandler : public YarpPlusPlus::RequestHandler
    {
    public:
        
        /*! @brief The constructor. */
        Test16EchoRequestHandler(void);
        
        /*! @brief The destructor. */
        virtual ~Test16EchoRequestHandler(void);
        
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
        
    }; // Test16EchoRequestHandler
    
} // YarpPlusPlusTest

#endif // ! defined(YPPTTEST16ECHOREQUESTHANDLER_H_)
