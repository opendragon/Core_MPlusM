//--------------------------------------------------------------------------------------------------
//
//  File:       m+m/m+mGeneralChannel.h
//
//  Project:    m+m
//
//  Contains:   The class declaration for general-purpose channels.
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
//  Created:    2014-04-07
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMGeneralChannel_H_))
# define MpMGeneralChannel_H_ /* Header guard */

# include <m+m/m+mBaseChannel.h>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for general-purpose channels. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace Common
    {
        /*! @brief A convenience class to provide distinct channels to and from adapters. */
        class GeneralChannel : public BaseChannel
        {
        public :

        protected :

        private :

            /*! @brief The class that this class is derived from. */
            typedef BaseChannel inherited;

        public :

            /*! @brief The constructor.
             @param isOutput @c true if the channel is used for output and @c false otherwise. */
            explicit
            GeneralChannel(const bool isOutput);

            /*! @brief The destructor. */
            virtual
            ~GeneralChannel(void);

            /*! @brief Returns @c true if the channel is used for output and @c false otherwise.
             @returns @c true if the channel is used for output and @c false otherwise. */
            inline bool
            isOutput(void)
            const
            {
                return _isOutput;
            } // isOutput

            /*! @brief Returns the protocol associated with the channel.
             @returns The protocol associated with the channel. */
            inline const YarpString &
            protocol(void)
            const
            {
                return _protocol;
            } // protocol

            /*! @brief Returns the description of the protocol associated with the channel.
             @returns The description of the protocol associated with the channel. */
            inline const YarpString &
            protocolDescription(void)
            const
            {
                return _protocolDescription;
            } // protocolDescription

            /*! @brief Sets the protocol associated with the channel.
             @param newProtocol The new protocol associated with the channel.
             @param description The description of the new protocol. */
            void
            setProtocol(const YarpString & newProtocol,
                        const YarpString & description);

        protected :

        private :

            /*! @brief The copy constructor.
             @param other The object to be copied. */
            GeneralChannel(const GeneralChannel & other);
            
            /*! @brief The assignment operator.
             @param other The object to be copied.
             @returns The updated object. */
            GeneralChannel &
            operator =(const GeneralChannel & other);

        public :

        protected :

        private :

            /*! @brief The protocol that the channel supports. */
            YarpString _protocol;

            /*! @brief The description of the protocol that the channel supports. */
            YarpString _protocolDescription;

            /*! @brief @c true if the channel is used for output and @c false otherwise. */
            bool _isOutput;

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunused-private-field"
# endif // defined(__APPLE__)
            /*! @brief Filler to pad to alignment boundary */
            char _filler[7];
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

        }; // GeneralChannel

    } // Common

} // MplusM

#endif // ! defined(MpMGeneralChannel_H_)
