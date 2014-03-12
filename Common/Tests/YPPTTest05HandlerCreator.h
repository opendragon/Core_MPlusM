//
//  YPPTTest05HandlerCreator.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-28.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPTTEST05HANDLERCREATOR_H_))
# define YPPTTEST05HANDLERCREATOR_H_ /* */

# include "../YPPInputHandlerCreator.h"

namespace YarpPlusPlusTest
{
    /*! @brief A test input handler factory. */
    class Test05HandlerCreator : public YarpPlusPlus::InputHandlerCreator
    {
    public:
        
        /*! @brief The constructor. */
        Test05HandlerCreator(void);
        
        /*! @brief The destructor. */
        virtual ~Test05HandlerCreator(void);
        
        /*! @brief Create a new InputHandler object to process input data.
         @returns A new InputHandler or @c NULL if one cannot be created. */
        virtual YarpPlusPlus::InputHandler * create(void);
        
    protected:
        
    private:
        
        /*! @brief The class that this class is derived from. */
        typedef InputHandlerCreator inherited;
        
    }; // Test05HandlerCreator
    
} // YarpPlusPlusTest

#endif // ! defined(YPPTTEST05HANDLERCREATOR_H_)
