//--------------------------------------------------------------------------------------------------
//
//  File:       m+mTunnelService.hpp
//
//  Project:    m+m
//
//  Contains:   The class declaration for a service that routes non-YARP data over a YARP network.
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
//  Created:    2015-02-11
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMTunnelService_HPP_))
# define MpMTunnelService_HPP_ /* Header guard */

# include <m+m/m+mBaseService.hpp>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for a service that routes non-YARP data over a YARP network. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

/*! @brief The channel name to use for the service if not provided. */
# define DEFAULT_TUNNEL_SERVICE_NAME_ BUILD_NAME_(MpM_SERVICE_BASE_NAME_, "tunnel")

/*! @brief The description of the service. */
# define TUNNEL_SERVICE_DESCRIPTION_ T_("Tunnel service")

namespace MplusM
{
    namespace Tunnel
    {
        class ConnectionThread;
        class WhereRequestHandler;

        /*! @brief The %Tunnel service. */
        class TunnelService : public Common::BaseService
        {
        public :

        protected :

        private :

            /*! @brief The class that this class is derived from. */
            typedef BaseService inherited;

        public :

            /*! @brief The constructor.
             @param[in] sourceName The data source address to be connected to.
             @param[in] sourcePort The data source port to be connected to.
             @param[in] launchPath The command-line name used to launch the service.
             @param[in] argc The number of arguments in 'argv'.
             @param[in] argv The arguments passed to the executable used to launch the service.
             @param[in] tag The modifier for the service name and port names.
             @param[in] serviceEndpointName The YARP name to be assigned to the new service.
             @param[in] servicePortNumber The port being used by the service. */
            TunnelService(const YarpString & sourceName,
                          const int          sourcePort,
                          const YarpString & launchPath,
                          const int          argc,
                          char * *           argv,
                          const YarpString & tag,
                          const YarpString & serviceEndpointName,
                          const YarpString & servicePortNumber = "");

            /*! @brief The destructor. */
            virtual
            ~TunnelService(void);

            /*! @brief Return the remembered address.
             @param[out] address The remembered address.
             @param[out] port The remembered port. */
            void
            getAddress(YarpString & address,
                       int &        port);

            /*! @brief Set the port that will be remembered.
             @param[in] port The port to be remembered. */
            inline void
            setPort(const int port)
            {
                _listenPort = port;
            } // setPort

            /*! @brief Start processing requests.
             @return @c true if the service was started and @c false if it was not. */
            virtual bool
            startService(void);

            /*! @brief Stop processing requests.
             @return @c true if the service was stopped and @c false it if was not. */
            virtual bool
            stopService(void);

        protected :

        private :

            /*! @brief The copy constructor.
             @param[in] other The object to be copied. */
            TunnelService(const TunnelService & other);

            /*! @brief Enable the standard request handlers. */
            void
            attachRequestHandlers(void);

            /*! @brief Disable the standard request handlers. */
            void
            detachRequestHandlers(void);

            /*! @brief The assignment operator.
             @param[in] other The object to be copied.
             @return The updated object. */
            TunnelService &
            operator =(const TunnelService & other);

        public :

        protected :

        private :

            /*! @brief The remembered address. */
            YarpString _listenAddress;

            /*! @brief The data source address. */
            YarpString _sourceAddress;

            /*! @brief The request handler for the 'where' request. */
            WhereRequestHandler * _whereHandler;

            /*! @brief The thread to handle the network connections. */
            ConnectionThread * _connection;

            /*! @brief The remembered port. */
            int _listenPort;

            /*! @brief The data source port. */
            int _sourcePort;

        }; // TunnelService

    } // Tunnel

} // MplusM

#endif // ! defined(MpMTunnelService_HPP_)
