//
//  YPPExampleService.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-06.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPEXAMPLESERVICE_H_))
# define YPPEXAMPLESERVICE_H_ /* */

# include "YPPBaseService.h"

namespace YarpPlusPlus
{
    /*! @brief An example Yarp++ service. */
    class ExampleService : BaseService
    {
    public:
        
        /*! @brief The constructor. */
        ExampleService(void);
        
        /*! @brief The destructor. */
        virtual ~ExampleService(void);
        
    protected:
        
    private:
        
    }; // ExampleService
    
} // YarpPlusPlus

#endif // ! defined(YPPEXAMPLESERVICE_H_)
