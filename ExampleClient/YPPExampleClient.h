//
//  YPPExampleClient.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-06.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPEXAMPLECLIENT_H_))
# define YPPEXAMPLECLIENT_H_ /* */

# include "YPPBaseClient.h"

namespace YarpPlusPlus
{
    /*! @brief An example Yarp++ client. */
    class ExampleClient : BaseClient
    {
    public:
        
        /*! @brief The constructor. */
        ExampleClient(void);
        
        /*! @brief The destructor. */
        virtual ~ExampleClient(void);
        
    protected:
        
    private:
        
    }; // ExampleClient
    
} // YarpPlusPlus

#endif // ! defined(YPPEXAMPLECLIENT_H_)
