//
//  YPPInputHandler.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-011.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPINPUTHANDLER_H_))
# define YPPINPUTHANDLER_H_ /* */

# include "YPPConfig.h"
# include <yarp/os/Bottle.h>
# include <yarp/os/PortReader.h>

namespace YarpPlusPlus
{
    /*! @brief A handler for partially-structured input data. */
    class InputHandler : public yarp::os::PortReader
    {
    public:
        
        /*! @brief The constructor. */
        InputHandler(void);
        
        /*! @brief The destructor. */
        virtual ~InputHandler(void);
        
        /*! @brief Process partially-structured input data.
         @param input The partially-structured input data.
         @param replyMechanism @c NULL if no reply is expected and non-@c NULL otherwise.
         @returns @c true if the input was correctly structured and successfully processed. */
        virtual bool handleInput(const yarp::os::Bottle &     input,
                                 yarp::os::ConnectionWriter * replyMechanism) = 0;
        
        /*! @brief Terminate processing of the input data stream. */
        void stopProcessing(void);
        
    protected:
        
    private:
        
        typedef yarp::os::PortReader inherited;

        /*! @brief Copy constructor.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        InputHandler(const InputHandler & other);
        
        /*! @brief Assignment operator.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        InputHandler & operator=(const InputHandler & other);
        
        /*! @brief Read an object from the input stream.
         @param connection The input stream that is to be read from.
         @returns @c true if the object was successfully read and @c false otherwise. */
        virtual bool read(yarp::os::ConnectionReader & connection);
        
        /*! @brief @c true if input stream processing is enabled. */
        bool _canProcessInput;
# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunused-private-field"
# endif // defined(__APPLE__)
        /*! @brief Filler to pad to alignment boundary */
        char _filler[7];
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

    }; // InputHandler
    
} // YarpPlusPlus

#endif // ! defined(YPPINPUTHANDLER_H_)
