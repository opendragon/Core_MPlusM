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

namespace YarpPlusPlusExample
{
    /*! @brief An example Yarp++ client. */
    class ExampleClient : YarpPlusPlus::BaseClient
    {
    public:
        
        /*! @brief The constructor. */
        ExampleClient(void);
        
        /*! @brief The destructor. */
        virtual ~ExampleClient(void);
        
    protected:
        
    private:
        
        typedef BaseClient inherited;

        /*! @brief Copy constructor.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        ExampleClient(const ExampleClient & other);
        
        /*! @brief Assignment operator.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        ExampleClient & operator=(const ExampleClient & other);
        
    }; // ExampleClient
    
} // YarpPlusPlusExample

#endif // ! defined(YPPEXAMPLECLIENT_H_)
