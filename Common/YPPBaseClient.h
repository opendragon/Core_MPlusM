//
//  YPPBaseClient.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-06.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPBASECLIENT_H_))
# define YPPBASECLIENT_H_ /* */

# include "YPPConfig.h"

namespace YarpPlusPlus
{
    /*! @brief The minimal functionality required for a Yarp++ client. */
    class BaseClient
    {
    public:
        
        /*! @brief The constructor. */
        BaseClient(void);
        
        /*! @brief The destructor. */
        virtual ~BaseClient(void);
        
    protected:
        
    private:
        
        /*! @brief Copy constructor.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        BaseClient(const BaseClient & other);
        
        /*! @brief Assignment operator.

         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        BaseClient & operator=(const BaseClient & other);
        
    }; // BaseClient
    
} // YarpPlusPlus

#endif // ! defined(YPPBASECLIENT_H_)
