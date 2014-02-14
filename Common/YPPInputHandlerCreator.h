//
//  YPPInputHandlerCreator.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-012.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPINPUTHANDLERCREATOR_H_))
# define YPPINPUTHANDLERCREATOR_H_ /* */

# include <yarp/os/PortReaderCreator.h>
# include "YPPInputHandler.h"

namespace YarpPlusPlus
{
    class InputHandlerCreator : public yarp::os::PortReaderCreator
    {
    public:

        InputHandlerCreator(void);
        
        virtual ~InputHandlerCreator(void);
        
        virtual yarp::os::PortReader * create(void) = 0;
        
    protected:

    private:
        
    }; // InputHandlerCreator
    
} // YarpPlusPlus

#endif // ! defined(YPPINPUTHANDLERCREATOR_H_)
