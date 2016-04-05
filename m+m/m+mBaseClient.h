//--------------------------------------------------------------------------------------------------
//
//  File:       m+m/m+mBaseClient.h
//
//  Project:    m+m
//
//  Contains:   The class declaration for the minimal functionality required for an m+m client.
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
//  Created:    2014-02-06
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMBaseClient_H_))
# define MpMBaseClient_H_ /* Header guard */

# include <m+m/m+mCommon.h>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for the minimal functionality required for an m+m client. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace Common
    {
        class ChannelStatusReporter;
        class ClientChannel;
        class ServiceResponse;

        /*! @brief The minimal functionality required for an m+m client. */
        class BaseClient
        {
        public :

        protected :

        private :

        public :

            /*! @brief The constructor.
             @param baseChannelName The name to be used as the root for the client channel. */
            explicit
            BaseClient(const YarpString & baseChannelName = DEFAULT_CHANNEL_ROOT_);

            /*! @brief The destructor. */
            virtual
            ~BaseClient(void);

            /*! @brief Create a connection with the service.
             @param checker A function that provides for early exit from loops.
             @param checkStuff The private data for the early exit function.
             @returns @c true if the client is connected to the service and @c false otherwise. */
            bool
            connectToService(CheckFunction checker = NULL,
                             void *        checkStuff = NULL);

            /*! @brief Disconnect from the service.
             @param checker A function that provides for early exit from loops.
             @param checkStuff The private data for the early exit function.
             @returns @c true if the client is no longer connected to the service and @ false
             otherwise. */
            bool
            disconnectFromService(CheckFunction checker = NULL,
                                  void *        checkStuff = NULL);

            /*! @brief Find a matching service and prepare to send requests to it.
             @param criteria The criteria to use to locate the service.
             @param allowOnlyOneMatch @c true if only one match is allowed and @c false if the first
             match will be used.
             @param checker A function that provides for early exit from loops.
             @param checkStuff The private data for the early exit function.
             @returns @c true if a matching service was found and @c false if no matching service or
             too many services were found. */
            bool
            findService(const char *  criteria,
                        const bool    allowOnlyOneMatch = false,
                        CheckFunction checker = NULL,
                        void *        checkStuff = NULL);

            /*! @brief Set the channel for the client to use.
             @param newChannel The channel to be used. */
            void
            setChannel(ClientChannel * newChannel = NULL);

            /*! @brief Set the channel status reporter for the private channel.
             @param reporter The channel status reporter to be used by the private channel.
             @param andReportNow @c true if the channel status reporter is to be activated
             immediately on open. */
            void
            setReporter(ChannelStatusReporter & reporter,
                        const bool              andReportNow = false);

        protected :

            /*! @brief Re-establish the service connection if it has dropped.
             @param checker A function that provides for early exit from loops.
             @param checkStuff The private data for the early exit function. */
            void
            reconnectIfDisconnected(CheckFunction checker = NULL,
                                    void *        checkStuff = NULL);

            /*! @brief Send a request to the service associated with the client.
             @param request The name of the request.
             @param parameters The required parameters for the request.
             @returns @c true on a successful communication with the service and @c false
             otherwise. */
            bool
            send(const char *             request,
                 const yarp::os::Bottle & parameters);

            /*! @brief Send a request to the service associated with the client.
             @param request The name of the request.
             @param parameters The required parameters for the request.
             @param response The response from the request.
             @returns @c true on a successful communication with the service and @c false
             otherwise. */
            bool
            send(const char *             request,
                 const yarp::os::Bottle & parameters,
                 ServiceResponse &        response);

        private :

            COPY_AND_ASSIGNMENT_(BaseClient);

        public :

        protected :

        private :

            /*! @brief The channel status reporter that has been set for this channel. */
            ChannelStatusReporter * _reporter;

            /*! @brief The channel that the client uses for communication. */
            ClientChannel * _channel;

            /*! @brief The root name for the client channel. */
            YarpString _baseChannelName;

            /*! @brief The name of the client channel being used. */
            YarpString _channelName;

            /*! @brief The name of the service channel being used. */
            YarpString _serviceChannelName;

            /*! @brief @c true if the client owns the channel and @c false otherwise. */
            bool _clientOwnsChannel;

            /*! @brief @c true if the client is connected to the service and @c false otherwise. */
            bool _connected;

            /*! @brief @c true if the channel status is to be reported on initial creation of the
             channel. */
            bool _reportImmediately;

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunused-private-field"
# endif // defined(__APPLE__)
            /*! @brief Filler to pad to alignment boundary */
            char _filler[5];
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

        }; // BaseClient

        /*! @brief Find one or more matching services that are registered with a running %Registry
         service.
         @param criteria The matching conditions.
         @param getNames @c true if service names are to be returned and @c false if service ports
         are to be returned.
         @param checker A function that provides for early exit from loops.
         @param checkStuff The private data for the early exit function.
         @returns A (possibly empty) list of matching service ports or service names. */
        yarp::os::Bottle
        FindMatchingServices(const YarpString & criteria,
                             const bool         getNames = false,
                             CheckFunction      checker = NULL,
                             void *             checkStuff = NULL);

    } // Common

} // MplusM

#endif // ! defined(MpMBaseClient_H_)
