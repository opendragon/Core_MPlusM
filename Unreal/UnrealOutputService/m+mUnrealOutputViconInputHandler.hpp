//--------------------------------------------------------------------------------------------------
//
//  File:       m+mUnrealOutputViconInputHandler.hpp
//
//  Project:    m+m
//
//  Contains:   The class declaration for the Vicon DataStream input channel input handler used by
//              the Unreal output service.
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
//  Created:    2014-11-18
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMUnrealOutputViconInputHandler_HPP_))
# define MpMUnrealOutputViconInputHandler_HPP_ /* Header guard */

# include <m+m/m+mBaseInputHandler.hpp>
# include <m+m/m+mStringBuffer.hpp>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for the Vicon DataStream input channel input handler used by the
 %Unreal output service. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace Unreal
    {
        class UnrealOutputService;

        /*! @brief A handler for partially-structured input data.

         The data is expected to be in the form of a sequence of Vicon subjects. */
        class UnrealOutputViconInputHandler : public Common::BaseInputHandler
        {
        public :

        protected :

        private :

            /*! @brief The class that this class is derived from. */
            typedef BaseInputHandler inherited;

        public :

            /*! @brief The constructor.
             @param[in] owner The service that this handler is connected to. */
            explicit
            UnrealOutputViconInputHandler(UnrealOutputService & owner);

            /*! @brief The destructor. */
            virtual
            ~UnrealOutputViconInputHandler(void);

            /*! @brief Set the translation scale.
             @param[in] newScale The scale factor for translation values. */
            void
            setScale(const double newScale);

            /*! @brief Set the network socket to be written to.
             @param[in] outSocket The network socket to be written to. */
            void
            setSocket(const SOCKET outSocket);

        protected :

        private :

            /*! @brief The copy constructor.
             @param[in] other The object to be copied. */
            UnrealOutputViconInputHandler(const UnrealOutputViconInputHandler & other);

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
            UnrealOutputViconInputHandler &
            operator =(const UnrealOutputViconInputHandler & other);

        public :

        protected :

        private :

            /*! @brief The service that this handler is connected to. */
            UnrealOutputService & _owner;

            /*! @brief The translation scale to be used. */
            double _scale;

            /*! @brief The network socket that is to be written to. */
            SOCKET _outSocket;

# if defined(MpM_UseCustomStringBuffer)
            /*! @brief The buffer to hold the output data. */
            Common::StringBuffer _outBuffer;
# endif // defined(MpM_UseCustomStringBuffer)

        }; // UnrealOutputViconInputHandler

    } // Unreal

} // MplusM

#endif // ! defined(MpMUnrealOutputViconInputHandler_HPP_)
