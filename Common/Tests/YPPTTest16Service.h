//
//  YPPTTest16Service.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-03-06.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPTTEST16SERVICE_H_))
# define YPPTTEST16SERVICE_H_ /* */

# include "../YPPBaseService.h"

namespace YarpPlusPlusTest
{
    /*! @brief A test input handler. */
    class Test16Service : public YarpPlusPlus::BaseService
    {
    public:
        
        /*! @brief The constructor.
         @param argc The number of arguments in 'argv'.
         @param argv The arguments to be used to specify the new service. */
        Test16Service(const int argc,
                      char **   argv);
        
        /*! @brief The destructor. */
        virtual ~Test16Service(void);
        
    protected:
        
    private:
        
        typedef BaseService inherited;
        
    }; // Test16Service
    
} // YarpPlusPlusTest

#endif // ! defined(YPPTTEST16SERVICE_H_)
