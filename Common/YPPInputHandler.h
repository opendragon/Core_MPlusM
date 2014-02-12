//
//  YPPInputHandler.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-011.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPINPUTHANDLER_H_))
# define YPPINPUTHANDLER_H_ /* */

# include <yarp/os/Bottle.h>
# include <yarp/os/PortReader.h>

using yarp::os::Bottle;
using yarp::os::ConnectionReader;
using yarp::os::ConnectionWriter;
using yarp::os::PortReader;

namespace YarpPlusPlus
{
    class InputHandler : public PortReader
    {
    public:
        InputHandler(void);
        
        virtual ~InputHandler(void);
        
        virtual bool handleInput(Bottle &           input,
                                 ConnectionWriter * replyMechanism) = 0;
        
        void stopProcessing(void);
        
    protected:
    private:
        virtual bool read(ConnectionReader & connection);
        
        bool _canProcessInput;
        
    }; // InputHandler
    
} // YarpPlusPlus

#endif // ! defined(YPPINPUTHANDLER_H_)
