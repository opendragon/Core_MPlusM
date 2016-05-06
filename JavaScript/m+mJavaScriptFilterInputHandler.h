//--------------------------------------------------------------------------------------------------
//
//  File:       m+mJavaScriptFilterInputHandler.h
//
//  Project:    m+m
//
//  Contains:   The class declaration for the input channel input handler used by the JavaScript
//              filter service.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2015 by H Plus Technologies Ltd. and Simon Fraser University.
//
//              All rights reserved. Redistribution and use in source and binary forms, with or
//              without modification, are permitted provided that the following conditions are met:
//                * Redistributions of source code must retain the above copyright notice, this list
//                  of conditions and the following disclaimer.
//                * Redistributions in binary form must reproduce the above copyright notice, this
//                  list of conditions and the following disclaimer in the documentation and / or
//                  other materials provided with the distribution.
//                * Neither the name of the copyright holders nor the names of its contributors may
//                  be used to endorse or promote products derived from this software without
//                  specific prior written permission.
//
//              THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY
//              EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
//              OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT
//              SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
//              INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
//              TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
//              BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
//              CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
//              ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
//              DAMAGE.
//
//  Created:    2015-01-05
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMJavaScriptFilterInputHandler_H_))
# define MpMJavaScriptFilterInputHandler_H_ /* Header guard */

# include "m+mJavaScriptFilterCommon.h"

# include <m+m/m+mBaseInputHandler.h>
# include <m+m/m+mGeneralChannel.h>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for the input channel input handler used by the %JavaScript filter
 service. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace JavaScript
    {
        class JavaScriptFilterService;

        /*! @brief A handler for partially-structured input data.

         The data is expected to be in the form of a sequence of integer or floating point
         values. */
        class JavaScriptFilterInputHandler : public Common::BaseInputHandler
        {
        public :

        protected :

        private :

            /*! @brief The class that this class is derived from. */
            typedef BaseInputHandler inherited;

        public :

            /*! @brief The constructor.
             @param owner The service that owns this handler.
             @param slotNumber The slot number of the associated channel. */
            JavaScriptFilterInputHandler(JavaScriptFilterService * owner,
                                         const size_t              slotNumber);

            /*! @brief The destructor. */
            virtual
            ~JavaScriptFilterInputHandler(void);

            /*! @brief Turn on input processing. */
            inline void
            activate(void)
            {
                _active = true;
            } // activate

            /*! @brief Turn off input processing. */
            inline void
            deactivate(void)
            {
                _active = false;
            } // deactivate

            /*! @brief Return the most recently received data.
             @returns The most recently received data. */
            inline const yarp::os::Bottle &
            getReceivedData(void)
            const
            {
                return _received;
            } // getReceivedData

        protected :

        private :

            /*! @brief The copy constructor.
             @param other The object to be copied. */
            JavaScriptFilterInputHandler(const JavaScriptFilterInputHandler & other);

            /*! @brief Process partially-structured input data.
             @param input The partially-structured input data.
             @param senderChannel The name of the channel used to send the input data.
             @param replyMechanism @c NULL if no reply is expected and non-@c NULL otherwise.
             @param numBytes The number of bytes available on the connection.
             @returns @c true if the input was correctly structured and successfully processed. */
            virtual bool
            handleInput(const yarp::os::Bottle &     input,
                        const YarpString &           senderChannel,
                        yarp::os::ConnectionWriter * replyMechanism,
                        const size_t                 numBytes);

            /*! @brief The assignment operator.
             @param other The object to be copied.
             @returns The updated object. */
            JavaScriptFilterInputHandler &
            operator =(const JavaScriptFilterInputHandler & other);

        public :

        protected :

        private :

            /*! @brief The service that owns this handler. */
            JavaScriptFilterService * _owner;

            /*! @brief The data received by this handler. */
            yarp::os::Bottle _received;

            /*! @brief The slot number of the associated channel. */
            size_t _slotNumber;

            /*! @brief @c true if the input is to be processed and @c false otherwise. */
            bool _active;

        }; // JavaScriptFilterInputHandler

    } // JavaScript

} // MplusM

#endif // ! defined(MpMJavaScriptFilterInputHandler_H_)
