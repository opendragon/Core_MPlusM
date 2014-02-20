//
//  YPPException.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-07.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPEXCEPTION_H_))
# define YPPEXCEPTION_H_ /* */

# include "YPPConfig.h"
# include <yarp/os/ConstString.h>

namespace YarpPlusPlus
{
    /*! @brief A convenience class to provide distinct exception objects. */
    class Exception
    {
    public:
        
        /*! @brief The constructor.
         @param reason A description of the exception being reported. */
        Exception(const yarp::os::ConstString & reason);
        
        /*! @brief The destructor. */
        virtual ~Exception(void);
        
    protected:
        
    private:
        
        /*! @brief Copy constructor.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        Exception(const Exception & other);
        
        /*! @brief Assignment operator.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        Exception & operator=(const Exception & other);
        
    }; // Exception
    
} // YarpPlusPlus

#endif // ! defined(YPPEXCEPTION_H_)
