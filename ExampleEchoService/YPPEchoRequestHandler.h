//
//  YPPEchoRequestHandler.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-28.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPTECHOREQUESTHANDLER_H_))
# define YPPTECHOREQUESTHANDLER_H_ /* */

# include "YPPRequestHandler.h"

namespace YarpPlusPlusExample
{
    /*! @brief The example 'echo' request handler.
     
    The input is a list of strings and the output is the same list of strings. */
    class EchoRequestHandler : public YarpPlusPlus::RequestHandler
    {
    public:
        
        /*! @brief The constructor. */
        EchoRequestHandler(void);
        
        /*! @brief The destructor. */
        virtual ~EchoRequestHandler(void);
        
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
        
    }; // EchoRequestHandler
    
} // YarpPlusPlusExample

#endif // ! defined(YPPTECHOREQUESTHANDLER_H_)
