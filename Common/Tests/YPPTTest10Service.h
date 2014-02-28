//
//  YPPTTest10Service.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-28.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPTTEST10SERVICE_H_))
# define YPPTTEST10SERVICE_H_ /* */

# include "../YPPBaseService.h"

namespace YarpPlusPlusTest
{
    /*! @brief A test input handler. */
    class Test10Service : public YarpPlusPlus::BaseService
    {
    public:
        
        /*! @brief The constructor.
         @param argc The number of arguments in 'argv'.
         @param argv The arguments to be used to specify the new service. */
        Test10Service(const int argc,
                      char **   argv);
        
        /*! @brief The destructor. */
        virtual ~Test10Service(void);
        
    protected:
        
    private:
        
        typedef BaseService inherited;
        
    }; // Test10Service
    
} // YarpPlusPlusTest

#endif // ! defined(YPPTTEST10SERVICE_H_)
