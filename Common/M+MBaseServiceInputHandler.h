//--------------------------------------------------------------------------------------
//
//  File:       M+MBaseServiceInputHandler.h
//
//  Project:    M+M
//
//  Contains:   The class declaration for the minimal functionality required for a M+M
//              input handler.
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
//  Created:    2014-02-21
//
//--------------------------------------------------------------------------------------

#if (! defined(MpMBaseServiceInputHandler_H_))
/*! @brief Header guard. */
# define MpMBaseServiceInputHandler_H_ /* */

# include "M+MInputHandler.h"

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 
 @brief The class declaration for the minimal functionality required for a M+M input handler. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace CommonX
    {
        class BaseService;
        
        /*! @brief The minimal functionality required for a M+M service. */
        class BaseServiceInputHandler : public InputHandler
        {
        public:
            
            /*! @brief The constructor. */
            BaseServiceInputHandler(BaseService & service);
            
            /*! @brief The destructor. */
            virtual ~BaseServiceInputHandler(void);
            
            /*! @brief Process partially-structured input data.
             @param input The partially-structured input data.
             @param senderChannel The name of the channel used to send the input data.
             @param replyMechanism @c NULL if no reply is expected and non-@c NULL otherwise.
             @returns @c true if the input was correctly structured and successfully processed. */
            virtual bool handleInput(const Package &               input,
                                     const yarp::os::ConstString & senderChannel,
                                     yarp::os::ConnectionWriter *  replyMechanism);
            
        protected:
            
        private:
            
            /*! @brief The class that this class is derived from. */
            typedef InputHandler inherited;
            
            /*! @brief Copy constructor.
             
             Note - not implemented and private, to prevent unexpected copying.
             @param other Another object to construct from. */
            BaseServiceInputHandler(const BaseServiceInputHandler & other);
            
            /*! @brief Assignment operator.
             
             Note - not implemented and private, to prevent unexpected copying.
             @param other Another object to construct from. */
            BaseServiceInputHandler & operator=(const BaseServiceInputHandler & other);
            
            /*! @brief The service that 'owns' this handler. */
            BaseService & _service;
            
        }; // BaseServiceInputHandler
        
    } // Common
    
} // MplusM

#endif // ! defined(MpMBaseServiceInputHandler_H_)
