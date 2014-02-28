//
//  YPPInfoRequestHandler.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-27.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPINFOREQUESTHANDLER_H_))
# define YPPINFOREQUESTHANDLER_H_ /* */

# include "YPPRequestHandler.h"

namespace YarpPlusPlus
{
    /*! @brief The 'list' request handler. */
    class InfoRequestHandler : public RequestHandler
    {
    public:
        
        /*! @brief The constructor. */
        InfoRequestHandler(void);
        
        /*! @brief The destructor. */
        virtual ~InfoRequestHandler(void);
        
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
        InfoRequestHandler(const InfoRequestHandler & other);
        
        /*! @brief Assignment operator.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        InfoRequestHandler & operator=(const InfoRequestHandler & other);
        
    }; // InfoRequestHandler
    
} // YarpPlusPlus

#endif // ! defined(YPPINFOREQUESTHANDLER_H_)
