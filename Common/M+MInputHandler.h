//--------------------------------------------------------------------------------------
//
//  File:       M+MInputHandler.h
//
//  Project:    M+M
//
//  Contains:   The class declaration for the interface between M+M input handlers and
//              YARP.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2014 by HPlus Technologies Ltd. and Simon Fraser University.
//
//              All rights reserved. Redistribution and use in source and binary forms,
//              with or without modification, are permitted provided that the following
//              conditions are met:
//                * Redistributions of source code must retain the above copyright
//                  notice, this list of conditions and the following disclaimer.
//                * Redistributions in binary form must reproduce the above copyright
//                  notice, this list of conditions and the following disclaimer in the
//                  documentation and/or other materials provided with the
//                  distribution.
//                * Neither the name of the copyright holders nor the names of its
//                  contributors may be used to endorse or promote products derived
//                  from this software without specific prior written permission.
//
//              THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
//              "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
//              LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
//              PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
//              OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
//              SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
//              LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
//              DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
//              THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
//              (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
//              OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
//  Created:    2014-02-11
//
//--------------------------------------------------------------------------------------

#if (! defined(MpMInputHandler_H_))
/*! @brief Header guard. */
# define MpMInputHandler_H_ /* */

# include "M+MCommon.h"

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wc++11-extensions"
#  pragma clang diagnostic ignored "-Wdocumentation"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#  pragma clang diagnostic ignored "-Wpadded"
#  pragma clang diagnostic ignored "-Wshadow"
#  pragma clang diagnostic ignored "-Wunused-parameter"
#  pragma clang diagnostic ignored "-Wweak-vtables"
# endif // defined(__APPLE__)
# include <yarp/os/PortReader.h>
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 
 @brief The class declaration for the interface between M+M input handlers and YARP. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace Common
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
             @param senderChannel The name of the channel used to send the input data.
             @param replyMechanism @c NULL if no reply is expected and non-@c NULL otherwise.
             @returns @c true if the input was correctly structured and successfully processed. */
            virtual bool handleInput(const Package &               input,
                                     const yarp::os::ConstString & senderChannel,
                                     yarp::os::ConnectionWriter *  replyMechanism) = 0;
            
            /*! @brief Terminate processing of the input data stream. */
            void stopProcessing(void);
            
        protected:
            
        private:
            
            /*! @brief The class that this class is derived from. */
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
        
    } // Common
    
} // MplusM

#endif // ! defined(MpMInputHandler_H_)
