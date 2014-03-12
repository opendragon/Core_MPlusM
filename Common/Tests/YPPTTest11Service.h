//
//  YPPTTest11Service.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-28.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPTTEST11SERVICE_H_))
# define YPPTTEST11SERVICE_H_ /* */

# include "../YPPBaseService.h"

namespace YarpPlusPlusTest
{
    /*! @brief A test service. */
    class Test11Service : public YarpPlusPlus::BaseService
    {
    public:
        
        /*! @brief The constructor.
         @param argc The number of arguments in 'argv'.
         @param argv The arguments to be used to specify the new service. */
        Test11Service(const int argc,
                      char **   argv);
        
        /*! @brief The destructor. */
        virtual ~Test11Service(void);
        
    protected:
        
    private:
        
        /*! @brief The class that this class is derived from. */
        typedef BaseService inherited;
        
    }; // Test11Service
    
} // YarpPlusPlusTest

#endif // ! defined(YPPTTEST11SERVICE_H_)
