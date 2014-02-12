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

using yarp::os::ConstString;

namespace YarpPlusPlus
{
    class Exception
    {
    public:
        Exception(const ConstString & reason);
        
        virtual ~Exception(void);
        
    protected:
    private:
    }; // Exception
    
} // YarpPlusPlus

#endif // ! defined(YPPEXCEPTION_H_)
