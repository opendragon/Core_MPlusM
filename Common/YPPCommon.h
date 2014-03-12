//
//  YPPCommon.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-18.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPCOMMON_H_))
# define YPPCOMMON_H_ /* */

# include "YPPConfig.h"
# include <yarp/os/ConstString.h>
# include <yarp/os/Contact.h>

namespace YarpPlusPlus
{

    /*! @brief Dump out a description of the provided connection information to the log.
     @param tag A unique string used to identify the call point for the output.
     @param aContact The connection information to be reported. */
    void DumpContact(const char *              tag,
                     const yarp::os::Contact & aContact);
    
} // YarpPlusPlus

#endif // ! defined(YPPCOMMON_H_)
