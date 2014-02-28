//
//  YPPTTest12Service.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-28.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPTTEST12SERVICE_H_))
# define YPPTTEST12SERVICE_H_ /* */

# include "../YPPBaseService.h"

namespace YarpPlusPlusTest
{
    /*! @brief A test input handler. */
    class Test12Service : public YarpPlusPlus::BaseService
    {
    public:
        
        /*! @brief The constructor.
         @param argc The number of arguments in 'argv'.
         @param argv The arguments to be used to specify the new service. */
        Test12Service(const int argc,
                      char **   argv);
        
        /*! @brief The destructor. */
        virtual ~Test12Service(void);
        
    protected:
        
    private:
        
        typedef BaseService inherited;
        
    }; // Test12Service
    
} // YarpPlusPlusTest

#endif // ! defined(YPPTTEST12SERVICE_H_)
