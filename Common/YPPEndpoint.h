//
//  YPPEndpoint.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-07.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPENDPOINT_H_))
# define YPPENDPOINT_H_ /* */

# include <yarp/os/Contact.h>
# include <yarp/os/impl/String.h>
# include "YPPInputHandler.h"

using yarp::os::ConstString;
using yarp::os::Port;
using yarp::os::PortReport;
using yarp::os::impl::String;

namespace YarpPlusPlus
{
    class Endpoint
    {
    public:
        Endpoint(
                 const ConstString & endpointName,
                 const ConstString & hostName = "",
                 const ConstString & portNumber = "",
                 const String &      carrierName = "");
        
        virtual ~Endpoint(void);
        
        ConstString getName(void) const;
        
        void setInputHandler(InputHandler & handler);
        
        void setReporter(PortReport & reporter,
                         const bool   andReportNow = false);
        
        static ConstString getRandomPortName(void);
        
    protected:
    private:
        InputHandler * _handler;
        Port *         _port;
    }; // Endpoint
    
} // YarpPlusPlus

#endif // ! defined(YPPENDPOINT_H_)
