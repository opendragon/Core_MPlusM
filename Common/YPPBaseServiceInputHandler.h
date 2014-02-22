//
//  YPPBaseServiceInputHandler.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-21.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPBASESERVICEINPUTHANDLER_H_))
# define YPPBASESERVICEINPUTHANDLER_H_ /* */

# include "YPPInputHandler.h"

namespace YarpPlusPlus
{
    class BaseService;
    
    /*! @brief The minimal functionality required for a Yarp++ service. */
    class BaseServiceInputHandler : public InputHandler
    {
    public:
        
        /*! @brief The constructor. */
        BaseServiceInputHandler(BaseService & service);
        
        /*! @brief The destructor. */
        virtual ~BaseServiceInputHandler(void);
        
        /*! @brief Process partially-structured input data.
         @param input The partially-structured input data.
         @param replyMechanism @c NULL if no reply is expected and non-@c NULL otherwise.
         @returns @c true if the input was correctly structured and successfully processed. */
        virtual bool handleInput(yarp::os::Bottle &           input,
                                 yarp::os::ConnectionWriter * replyMechanism);
        
    protected:
        
    private:
        
        /*! @brief Copy constructor.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        BaseServiceInputHandler(const BaseServiceInputHandler & other);
        
        /*! @brief Assignment operator.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        BaseServiceInputHandler & operator=(const BaseServiceInputHandler & other);

        /*! @brief The service that 'owns' this handler. */
        BaseService & _service;

    }; // BaseServiceInputHandler
    
} // YarpPlusPlus

#endif // ! defined(YPPBASESERVICEINPUTHANDLER_H_)
