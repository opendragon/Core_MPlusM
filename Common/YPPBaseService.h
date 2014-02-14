//
//  YPPBaseService.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-06.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPBASESERVICE_H_))
# define YPPBASESERVICE_H_ /* */

namespace YarpPlusPlus
{
    /*! @brief The minimal functionality required for a Yarp++ service. */
    class BaseService
    {
    public:
        
        /*! @brief The constructor. */
        BaseService(void);
        
        /*! @brief The destructor. */
        virtual ~BaseService(void);
        
    protected:
        
    private:
        
    }; // BaseService
    
} // YarpPlusPlus

#endif // ! defined(YPPBASESERVICE_H_)
