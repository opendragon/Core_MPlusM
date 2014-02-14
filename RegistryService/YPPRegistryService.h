//
//  YPPRegistryService.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-06.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPREGISTRYSERVICE_H_))
# define YPPREGISTRYSERVICE_H_ /* */

# include "YPPBaseService.h"

namespace YarpPlusPlus
{
    /*! @brief The Yarp++ Service Registry service. */
    class RegistryService : BaseService
    {
    public:
        
        /*! @brief The constructor. */
        RegistryService(void);
        
        /*! @brief The destructor. */
        virtual ~RegistryService(void);
        
    protected:
        
    private:
        
    }; // RegistryService

} // YarpPlusPlus

#endif // ! defined(YPPREGISTRYSERVICE_H_)
