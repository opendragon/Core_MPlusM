//
//  YPPExampleRandomNumberClient.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-03-06.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPEXAMPLERANDOMNUMBERCLIENT_H_))
# define YPPEXAMPLERANDOMNUMBERCLIENT_H_ /* */

# include "YPPBaseClient.h"

namespace YarpPlusPlusExample
{
    /*! @brief An example Yarp++ client, for the 'random' service. */
    class ExampleRandomNumberClient : public YarpPlusPlus::BaseClient
    {
    public:
        
        /*! @brief The constructor. */
        ExampleRandomNumberClient(void);
        
        /*! @brief The destructor. */
        virtual ~ExampleRandomNumberClient(void);
        
    protected:
        
    private:
        
        /*! @brief The class that this class is derived from. */
        typedef BaseClient inherited;

        /*! @brief Copy constructor.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        ExampleRandomNumberClient(const ExampleRandomNumberClient & other);
        
        /*! @brief Assignment operator.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        ExampleRandomNumberClient & operator=(const ExampleRandomNumberClient & other);
        
    }; // ExampleRandomNumberClient
    
} // YarpPlusPlusExample

#endif // ! defined(YPPEXAMPLERANDOMNUMBERCLIENT_H_)
