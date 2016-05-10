//--------------------------------------------------------------------------------------------------
//
//  File:       m+m/m+mSendReceiveCounters.hpp
//
//  Project:    m+m
//
//  Contains:   The class declaration for the send / receive counters.
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
//  Created:    2014-10-09
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMSendReceiveCounters_HPP_))
# define MpMSendReceiveCounters_HPP_ /* Header guard */

# include <m+m/m+mCommon.hpp>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for the send / receive counters. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

/*! @brief The property keyword for the measurements channel. */
# define MpM_SENDRECEIVE_CHANNEL_     "channel"

/*! @brief The property keyword for the date. */
# define MpM_SENDRECEIVE_DATE_        "date"

/*! @brief The property keyword for the number of received bytes. */
# define MpM_SENDRECEIVE_INBYTES_     "inBytes"

/*! @brief The property keyword for the number of received messages. */
# define MpM_SENDRECEIVE_INMESSAGES_  "inMessages"

/*! @brief The property keyword for the number of sent bytes. */
# define MpM_SENDRECEIVE_OUTBYTES_    "outBytes"

/*! @brief The property keyword for the number of sent messages. */
# define MpM_SENDRECEIVE_OUTMESSAGES_ "outMessages"

/*! @brief The property keyword for the time. */
# define MpM_SENDRECEIVE_TIME_        "time"

namespace MplusM
{
    namespace Common
    {
        /*! @brief A class to hold the send / receive counters. */
        class SendReceiveCounters
        {
        public :

        protected :

        private :

        public :

            /*! @brief The constructor.
             @param initialInBytes The initial number of bytes received.
             @param initialInMessages The initial number of messages received.
             @param initialOutBytes The initial number of bytes sent.
             @param initialOutMessages The initial number of messages sent. */
            explicit
            SendReceiveCounters(const int64_t initialInBytes = 0,
                                const size_t  initialInMessages = 0,
                                const int64_t initialOutBytes = 0,
                                const size_t  initialOutMessages = 0);

            /*! @brief Add a dictionary to a list from the send / receive counters.
             @param counterList The list to be modified.
             @param channel The channel associated with the counters. */
            void
            addToList(yarp::os::Bottle & counterList,
                      const YarpString & channel);

            /*! @brief Reset the send / receive counters. */
            void
            clearCounters(void);

            /*! @brief Update the received data.
             @param moreInBytes The number of bytes received.
             @returns The modified values. */
            SendReceiveCounters &
            incrementInCounters(const int64_t moreInBytes);

            /*! @brief Update the sent data.
             @param moreOutBytes The number of bytes sent.
             @returns The modified values. */
            SendReceiveCounters &
            incrementOutCounters(const int64_t moreOutBytes);

            /*! @brief The assignment operator.
             @param other The values to be assigned to the send / receive counters.
             @returns The modified values. */
            SendReceiveCounters &
            operator =(const SendReceiveCounters & other);

            /*! @brief The additive assignment operator.
             @param other The values to be added to the send / receive counters.
             @returns The modified values. */
            SendReceiveCounters &
            operator +=(const SendReceiveCounters & other);

        protected :

        private :

        public :

        protected :

        private :

            /*! @brief The number of bytes received. */
            int64_t _inBytes;

            /*! @brief The number of bytes sent. */
            int64_t _outBytes;

            /*! @brief The number of messages received. */
            size_t _inMessages;

            /*! @brief The number of messages sent. */
            size_t _outMessages;

        }; // SendReceiveCounters

    } // Common

} // MplusM

#endif // ! defined(MpMSendReceiveCounters_HPP_)
