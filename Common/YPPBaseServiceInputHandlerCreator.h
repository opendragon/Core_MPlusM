//
//  YPPBaseServiceInputHandlerCreator.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-21.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPBASESERVICEINPUTHANDLERCREATOR_H_))
# define YPPBASESERVICEINPUTHANDLERCREATOR_H_ /* */

# include "YPPInputHandlerCreator.h"

namespace YarpPlusPlus
{
    class BaseService;
    
    /*! @brief The minimal functionality required for a Yarp++ service. */
    class BaseServiceInputHandlerCreator : public InputHandlerCreator
    {
    public:
        
        /*! @brief The constructor. */
        BaseServiceInputHandlerCreator(BaseService & service);
        
        /*! @brief The destructor. */
        virtual ~BaseServiceInputHandlerCreator(void);
        
        /*! @brief Create a new InputHandler object to process input data.
         @returns A new InputHandler or @c NULL if one cannot be created. */
        virtual InputHandler * create(void);
        
    protected:
        
    private:
        
        typedef InputHandlerCreator inherited;
        
        /*! @brief Copy constructor.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        BaseServiceInputHandlerCreator(const BaseServiceInputHandlerCreator & other);
        
        /*! @brief Assignment operator.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        BaseServiceInputHandlerCreator & operator=(const BaseServiceInputHandlerCreator & other);

        /*! @brief The service that 'owns' this handler. */
        BaseService & _service;
        
    }; // BaseServiceInputHandlerCreator
    
} // YarpPlusPlus

#endif // ! defined(YPPBASESERVICEINPUTHANDLERCREATOR_H_)
