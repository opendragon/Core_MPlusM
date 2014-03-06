//
//  YPPTTest14Service.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-03-05.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPTTEST14SERVICE_H_))
# define YPPTTEST14SERVICE_H_ /* */

# include "../YPPBaseService.h"

namespace YarpPlusPlusTest
{
    /*! @brief A test input handler. */
    class Test14Service : public YarpPlusPlus::BaseService
    {
    public:
        
        /*! @brief The constructor.
         @param argc The number of arguments in 'argv'.
         @param argv The arguments to be used to specify the new service. */
        Test14Service(const int argc,
                      char **   argv);
        
        /*! @brief The destructor. */
        virtual ~Test14Service(void);
        
    protected:
        
    private:
        
        typedef BaseService inherited;
        
    }; // Test14Service
    
} // YarpPlusPlusTest

#endif // ! defined(YPPTTEST14SERVICE_H_)
