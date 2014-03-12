//
//  YPPTTest08Handler.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-28.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPTTEST08HANDLER_H_))
# define YPPTTEST08HANDLER_H_ /* */

# include "../YPPInputHandler.h"

namespace YarpPlusPlusTest
{
    /*! @brief A test input handler. */
    class Test08Handler : public YarpPlusPlus::InputHandler
    {
    public:
        
        /*! @brief The constructor. */
        Test08Handler(void);
        
        /*! @brief The destructor. */
        virtual ~Test08Handler(void);
        
        /*! @brief Process partially-structured input data.
         @param input The partially-structured input data.
         @param replyMechanism @c NULL if no reply is expected and non-@c NULL otherwise.
         @returns @c true if the input was correctly structured and successfully processed. */
        virtual bool handleInput(const yarp::os::Bottle &     input,
                                 yarp::os::ConnectionWriter * replyMechanism);
        
    protected:
        
    private:
        
        /*! @brief The class that this class is derived from. */
        typedef InputHandler inherited;
        
    }; // Test08Handler
    
} // YarpPlusPlusTest

#endif // ! defined(YPPTTEST08HANDLER_H_)
