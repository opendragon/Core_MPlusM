//--------------------------------------------------------------------------------------------------
//
//  File:       m+mRunningSumInputHandler.hpp
//
//  Project:    m+m
//
//  Contains:   The class declaration for the custom control channel input handler used by the
//              Running Sum alternative adapter.
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
//  Created:    2014-04-15
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMRunningSumInputHandler_HPP_))
# define MpMRunningSumInputHandler_HPP_ /* Header guard */

# include <m+m/m+mBaseInputHandler.hpp>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for the custom input handler used by the Running Sum alternative
 adapter. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace Example
    {
        class RunningSumAdapterData;

        /*! @brief A handler for partially-structured input data.

         The data is expected to be in the form of a sequence of floating point data or commands. */
        class RunningSumInputHandler : public Common::BaseInputHandler
        {
        public :

        protected :

        private :

            /*! @brief The class that this class is derived from. */
            typedef BaseInputHandler inherited;

        public :

            /*! @brief The constructor.
             @param[in] shared The data shared between the input handlers and the main thread. */
            explicit
            RunningSumInputHandler(RunningSumAdapterData & shared);

            /*! @brief The destructor. */
            virtual
            ~RunningSumInputHandler(void);

        protected :

        private :

            /*! @brief The copy constructor.
             @param[in] other The object to be copied. */
            RunningSumInputHandler(const RunningSumInputHandler & other);

            /*! @brief Process partially-structured input data.
             @param[in] input The partially-structured input data.
             @param[in] senderChannel The name of the channel used to send the input data.
             @param[in] replyMechanism @c NULL if no reply is expected and non-@c NULL otherwise.
             @param[in] numBytes The number of bytes available on the connection.
             @return @c true if the input was correctly structured and successfully processed. */
            virtual bool
            handleInput(const yarp::os::Bottle &     input,
                        const YarpString &           senderChannel,
                        yarp::os::ConnectionWriter * replyMechanism,
                        const size_t                 numBytes);

            /*! @brief The assignment operator.
             @param[in] other The object to be copied.
             @return The updated object. */
            RunningSumInputHandler &
            operator =(const RunningSumInputHandler & other);

        public :

        protected :

        private :

            /*! @brief The shared data that describes the connection to the service that we are
             using. */
            RunningSumAdapterData & _shared;

        }; // RunningSumInputHandler

    } // Example

} // MplusM

#endif // ! defined(MpMRunningSumInputHandler_HPP_)
