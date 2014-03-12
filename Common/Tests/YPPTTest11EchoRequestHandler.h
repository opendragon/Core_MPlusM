//
//  YPPTTest11EchoRequestHandler.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-28.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPTTEST11ECHOREQUESTHANDLER_H_))
# define YPPTTEST11ECHOREQUESTHANDLER_H_ /* */

# include "../YPPRequestHandler.h"

namespace YarpPlusPlusTest
{
    /*! @brief A test request handler. */
    class Test11EchoRequestHandler : public YarpPlusPlus::RequestHandler
    {
    public:
        
        /*! @brief The constructor. */
        Test11EchoRequestHandler(void);
        
        /*! @brief The destructor. */
        virtual ~Test11EchoRequestHandler(void);
        
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
        
    }; // Test11EchoRequestHandler
    
} // YarpPlusPlusTest

#endif // ! defined(YPPTTEST11ECHOREQUESTHANDLER_H_)
