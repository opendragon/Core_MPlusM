//
//  YPPRandomRequestHandler.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-03-06.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPTRANDOMREQUESTHANDLER_H_))
# define YPPTRANDOMREQUESTHANDLER_H_ /* */

# include "YPPRequestHandler.h"

/*! @brief The name for the 'random' request. */
# define YPP_RANDOM_REQUEST "random"

namespace YarpPlusPlusExample
{
    class RandomRequestHandler : public YarpPlusPlus::RequestHandler
    {
    public:
        
        /*! @brief The constructor. */
        RandomRequestHandler(void);
        
        /*! @brief The destructor. */
        virtual ~RandomRequestHandler(void);
        
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
        
    }; // RandomRequestHandler
    
} // YarpPlusPlusExample

#endif // ! defined(YPPTRANDOMREQUESTHANDLER_H_)
