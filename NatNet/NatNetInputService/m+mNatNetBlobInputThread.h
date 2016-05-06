//--------------------------------------------------------------------------------------------------
//
//  File:       m+mNatNetBlobInputThread.h
//
//  Project:    m+m
//
//  Contains:   The class declaration for an output-generating thread for m+m.
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
//  Created:    2015-07-23
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMNatNetBlobInputThread_H_))
# define MpMNatNetBlobInputThread_H_ /* Header guard */

# include <m+m/m+mBaseThread.h>
# include <m+m/m+mGeneralChannel.h>
# include <m+m/m+mStringBuffer.h>

# if (! defined(MpM_BuildDummyServices))
#  include <NatNetTypes.h>
#  include <NatNetClient.h>
# endif // ! defined(MpM_BuildDummyServices)

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for an output-generating thread for m+m. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

/*! @brief The number of characters allowed in an IP address string. */
# define IPADDRESS_BUFFER_SIZE 256

namespace MplusM
{
    namespace NatNet
    {
        /*! @brief A convenience class to generate output. */
        class NatNetBlobInputThread : public Common::BaseThread
        {
        public :

        protected :

        private :

            /*! @brief The class that this class is derived from. */
            typedef BaseThread inherited;

        public :

            /*! @brief The constructor.
             @param outChannel The channel to send data bursts to.
             @param name The IP address of the Natural Point %NatNet device.
             @param commandPort The command port for the Natural Point %NatNet device.
             @param dataPort The data port for the Natural Point %NatNet device. */
            NatNetBlobInputThread(Common::GeneralChannel * outChannel,
                                  const YarpString &       name,
                                  const int                commandPort,
                                  const int                dataPort);

            /*! @brief The destructor. */
            virtual
            ~NatNetBlobInputThread(void);

            /*! @brief Stop using the output channel. */
            void
            clearOutputChannel(void);

# if defined(MpM_UseCustomStringBuffer)
            /*! @brief Provide access to the output buffer.
             This is meant to be used by the data received callback.
             @returns The output buffer. */
            inline Common::StringBuffer &
            getOutputBuffer(void)
            {
                return _outBuffer;
            } // getOutputBuffer
# endif // defined(MpM_UseCustomStringBuffer)

            /*! @brief Send a message via the output channel.
             @param message The message to send.
             @param length The length of the message. */
            void
            sendMessage(const char * message,
                        const size_t length);

            /*! @brief Set the translation scale.
             @param newScale The scale factor for translation values. */
            void
            setScale(const double newScale);

            /*! @brief Return the translation scale.
             @returns The translation scale. */
            inline double
            translationScale(void)
            const
            {
                return _translationScale;
            } // translationScale

        protected :

        private :

            /*! @brief The copy constructor.
             @param other The object to be copied. */
            NatNetBlobInputThread(const NatNetBlobInputThread & other);

            /*! @brief The assignment operator.
             @param other The object to be copied.
             @returns The updated object. */
            NatNetBlobInputThread &
            operator =(const NatNetBlobInputThread & other);

            /*! @brief The thread main body. */
            virtual void
            run(void);

            /*! @brief The thread initialization method.
             @returns @c true if the thread is ready to run. */
            virtual bool
            threadInit(void);

        public :

        protected :

        private :

            /*! @brief The channel to send data bursts to. */
            Common::GeneralChannel * _outChannel;

            /*! @brief The address of the Natural Point %NatNet device. */
            YarpString _address;

            /*! @brief The translation scale to be used. */
            double _translationScale;

            /*! @brief The command port of the Natural Point %NatNet device. */
            int _commandPort;

            /*! @brief The command port of the Natural Point %NatNet device. */
            int _dataPort;

# if (! defined(MpM_BuildDummyServices))
            /*! @brief The connection to the Natural Point %NatNet device. */
            NatNetClient * _client;
# endif // ! defined(MpM_BuildDummyServices)

# if defined(MpM_UseCustomStringBuffer)
            /*! @brief The buffer to hold the output data. */
            Common::StringBuffer _outBuffer;
# endif // defined(MpM_UseCustomStringBuffer)

            /*! @brief The %Bottle to use send the output data. */
            yarp::os::Bottle _messageBottle;

            /*! @brief The local copy of the client IP address. */
            char _clientIPAddress[IPADDRESS_BUFFER_SIZE];

            /*! @brief The local copy of the server IP address. */
            char _serverIPAddress[IPADDRESS_BUFFER_SIZE];

        }; // NatNetBlobInputThread

    } // NatNet

} // MplusM

#endif // ! defined(MpMNatNetBlobInputThread_H_)
