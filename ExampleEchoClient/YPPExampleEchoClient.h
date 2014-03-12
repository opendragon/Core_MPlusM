//
//  YPPExampleEchoClient.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-06.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPEXAMPLEECHOCLIENT_H_))
# define YPPEXAMPLEECHOCLIENT_H_ /* */

# include "YPPBaseClient.h"

namespace YarpPlusPlusExample
{
    /*! @brief An example Yarp++ client, for the 'echo' service. */
    class ExampleEchoClient : public YarpPlusPlus::BaseClient
    {
    public:
        
        /*! @brief The constructor. */
        ExampleEchoClient(void);
        
        /*! @brief The destructor. */
        virtual ~ExampleEchoClient(void);
        
    protected:
        
    private:
        
        /*! @brief The class that this class is derived from. */
        typedef BaseClient inherited;

        /*! @brief Copy constructor.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        ExampleEchoClient(const ExampleEchoClient & other);
        
        /*! @brief Assignment operator.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        ExampleEchoClient & operator=(const ExampleEchoClient & other);
        
    }; // ExampleEchoClient
    
} // YarpPlusPlusExample

#endif // ! defined(YPPEXAMPLEECHOCLIENT_H_)
