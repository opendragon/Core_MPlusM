//--------------------------------------------------------------------------------------------------
//
//  File:       m+m/m+mBaseInputHandler.h
//
//  Project:    m+m
//
//  Contains:   The class declaration for the interface between m+m input handlers and YARP.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2014 by H Plus Technologies Ltd. and Simon Fraser University.
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
//  Created:    2014-02-11
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMBaseInputHandler_H_))
# define MpMBaseInputHandler_H_ /* Header guard */

# include <m+m/m+mCommon.h>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for the interface between m+m input handlers and YARP. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace Common
    {
        class BaseChannel;

        /*! @brief A handler for partially-structured input data. */
        class BaseInputHandler : public yarp::os::PortReader
        {
        public :

        protected :

        private :

            /*! @brief The class that this class is derived from. */
            typedef yarp::os::PortReader inherited;

        public :

            /*! @brief The constructor. */
            BaseInputHandler(void);

            /*! @brief The destructor. */
            virtual
            ~BaseInputHandler(void);

            /*! @brief Turn off the send / receive metrics collecting. */
            void
            disableMetrics(void);

            /*! @brief Turn on the send / receive metrics collecting. */
            void
            enableMetrics(void);

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
                        const size_t                 numBytes) = 0;

            /*! @brief Return the state of the  send / receive metrics.
             @returns @c true if the send / receive metrics are being gathered and @c false
             otherwise. */
            inline bool
            metricsAreEnabled(void)
            const
            {
                return _metricsEnabled;
            } // metricsAreEnabled

            /*! @brief Remember the channel that is feeding the input handler.
             @param theChannel The channel that is feeding the input handler. */
            void
            setChannel(BaseChannel * theChannel);

            /*! @brief Terminate processing of the input data stream. */
            void
            stopProcessing(void);

        protected :

        private :

            /*! @brief The copy constructor.
             @param other The object to be copied. */
            BaseInputHandler(const BaseInputHandler & other);
            
            /*! @brief The assignment operator.
             @param other The object to be copied.
             @returns The updated object. */
            BaseInputHandler &
            operator =(const BaseInputHandler & other);

            /*! @brief Read an object from the input stream.
             @param connection The input stream that is to be read from.
             @returns @c true if the object was successfully read and @c false otherwise. */
            virtual bool
            read(yarp::os::ConnectionReader & connection);

        public :

        protected :

        private :

            /*! @brief The channel that is feeding this input handler. */
            BaseChannel * _channel;

            /*! @brief @c true if input stream processing is enabled. */
            bool _canProcessInput;

            /*! @brief @c true if metrics are enabled and @c false otherwise. */
            bool _metricsEnabled;

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunused-private-field"
# endif // defined(__APPLE__)
            /*! @brief Filler to pad to alignment boundary */
            char _filler[6];
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

        }; // BaseInputHandler

    } // Common

} // MplusM

#endif // ! defined(MpMBaseInputHandler_H_)
