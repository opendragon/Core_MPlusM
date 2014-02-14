//
//  YPPBaseClient.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-06.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPBASECLIENT_H_))
# define YPPBASECLIENT_H_ /* */

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
        
    }; // BaseClient
    
} // YarpPlusPlus

#endif // ! defined(YPPBASECLIENT_H_)
