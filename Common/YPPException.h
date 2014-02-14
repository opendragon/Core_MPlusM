//
//  YPPException.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-07.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPEXCEPTION_H_))
# define YPPEXCEPTION_H_ /* */

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
        
    }; // Exception
    
} // YarpPlusPlus

#endif // ! defined(YPPEXCEPTION_H_)
