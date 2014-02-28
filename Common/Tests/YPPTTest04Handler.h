//
//  YPPTTest04Handler.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-28.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPTTEST04HANDLER_H_))
# define YPPTTEST04HANDLER_H_ /* */

# include "../YPPInputHandler.h"

namespace YarpPlusPlusTest
{
    /*! @brief A test input handler. */
    class Test04Handler : public YarpPlusPlus::InputHandler
    {
    public:
        
        /*! @brief The constructor. */
        Test04Handler(void);
        
        /*! @brief The destructor. */
        virtual ~Test04Handler(void);
        
        /*! @brief Process partially-structured input data.
         @param input The partially-structured input data.
         @param replyMechanism @c NULL if no reply is expected and non-@c NULL otherwise.
         @returns @c true if the input was correctly structured and successfully processed. */
        virtual bool handleInput(const yarp::os::Bottle &     input,
                                 yarp::os::ConnectionWriter * replyMechanism);
        
    protected:
        
    private:
        
        typedef InputHandler inherited;
        
    }; // Test04Handler
    
} // YarpPlusPlusTest

#endif // ! defined(YPPTTEST04HANDLER_H_)
