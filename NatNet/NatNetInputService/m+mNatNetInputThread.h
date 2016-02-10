//--------------------------------------------------------------------------------------------------
//
//  File:       m+mNatNetInputThread.h
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
//  Created:    2015-04-13
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMNatNetInputThread_H_))
# define MpMNatNetInputThread_H_ /* Header guard */

# include <m+m/m+mBaseThread.h>
# include <m+m/m+mGeneralChannel.h>

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
        class NatNetInputThread : public Common::BaseThread
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
            NatNetInputThread(Common::GeneralChannel * outChannel,
                              const YarpString &       name,
                              const int                commandPort,
                              const int                dataPort);
            
            /*! @brief The destructor. */
            virtual
            ~NatNetInputThread(void);
            
            /*! @brief Stop using the output channel. */
            void
            clearOutputChannel(void);
            
            /*! @brief Send a message via the output channel.
             @param message The message to send. */
            void
            sendMessage(yarp::os::Bottle & message);

        protected :
            
        private :
            
            DECLARE_RUN_;
            
            DECLARE_THREADINIT_;
            
            DECLARE_THREADRELEASE_;
            
            COPY_AND_ASSIGNMENT_(NatNetInputThread);
            
        public :
        
        protected :
        
        private :
            
            /*! @brief The channel to send data bursts to. */
            Common::GeneralChannel * _outChannel;
            
            /*! @brief The address of the Natural Point %NatNet device. */
            YarpString _address;

            /*! @brief The command port of the Natural Point %NatNet device. */
            int _commandPort;

            /*! @brief The command port of the Natural Point %NatNet device. */
            int _dataPort;
            
# if (! defined(MpM_BuildDummyServices))
            /*! @brief The connection to the Natural Point %NatNet device. */
            NatNetClient * _client;
# endif // ! defined(MpM_BuildDummyServices)

            /*! @brief The local copy of the client IP address. */
            char _clientIPAddress[IPADDRESS_BUFFER_SIZE];

            /*! @brief The local copy of the server IP address. */
            char _serverIPAddress[IPADDRESS_BUFFER_SIZE];

        }; // NatNetInputThread
        
    } // NatNet
    
} // MplusM

#endif // ! defined(MpMNatNetInputThread_H_)
