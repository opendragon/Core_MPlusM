//--------------------------------------------------------------------------------------------------
//
//  File:       m+mConnectionThread.h
//
//  Project:    m+m
//
//  Contains:   The class declaration for a network connection handling thread.
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
//  Created:    2015-02-12
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMConnectionThread_H_))
# define MpMConnectionThread_H_ /* Header guard */

# include <m+m/m+mBaseThread.h>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for a network connection handling thread. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace Tunnel
    {
        class TunnelService;

        /*! @brief A convenience class to handle network connections. */
        class ConnectionThread : public Common::BaseThread
        {
        public :

        protected :

        private :

            /*! @brief The class that this class is derived from. */
            typedef BaseThread inherited;

        public :

            /*! @brief The constructor.
             @param service The service that manages the network connection. */
            explicit
            ConnectionThread(TunnelService & service);

            /*! @brief The destructor. */
            virtual
            ~ConnectionThread(void);

            /*! @brief Set the address of the data source.
             @param sourceName The data source address to be connected to.
             @param sourcePort The data source port to be connected to. */
            void
            setSourceAddress(const YarpString & sourceName,
                             const int          sourcePort);

        protected :

        private :

            /*! @brief The copy constructor.
             @param other The object to be copied. */
            ConnectionThread(const ConnectionThread & other);

            /*! @brief The assignment operator.
             @param other The object to be copied.
             @returns The updated object. */
            ConnectionThread &
            operator =(const ConnectionThread & other);

            /*! @brief The thread main body. */
            virtual void
            run(void);

        public :

        protected :

        private :

            /*! @brief The service that manages the connection information. */
            TunnelService & _service;

            /*! @brief The data source address. */
            YarpString _sourceAddress;

            /*! @brief The data source port. */
            int _sourcePort;

            /*! @brief The network socket that is to be listened to. */
            SOCKET _listenSocket;

            /*! @brief The network socket that provides the source data. */
            SOCKET _sourceSocket;

        }; // ConnectionThread

    } // Tunnel

} // MplusM

#endif // ! defined(MpMConnectionThread_H_)
