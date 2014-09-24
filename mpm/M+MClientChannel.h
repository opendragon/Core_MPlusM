//--------------------------------------------------------------------------------------------------
//
//  File:       mpm/M+MClientChannel.h
//
//  Project:    M+M
//
//  Contains:   The class declaration for channels for responses from a service to a client.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2014 by HPlus Technologies Ltd. and Simon Fraser University.
//
//              All rights reserved. Redistribution and use in source and binary forms, with or
//              without modification, are permitted provided that the following conditions are met:
//                * Redistributions of source code must retain the above copyright notice, this list
//                  of conditions and the following disclaimer.
//                * Redistributions in binary form must reproduce the above copyright notice, this
//                  list of conditions and the following disclaimer in the documentation and/or
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

#if (! defined(MpMClientChannel_H_))
# define MpMClientChannel_H_ /* Header guard */

# include <mpm/M+MCommon.h>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 
 @brief The class declaration for channels for responses from a service to a client. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

/*! @brief The Port class to be used for client connections. */
# if defined(MpM_ChannelsUseRpc)
#  define CLIENT_PORT_CLASS_ yarp::os::RpcClient
# else // ! defined(MpM_ChannelsUseRpc)
#  define CLIENT_PORT_CLASS_ yarp::os::Port
# endif // ! defined(MpM_ChannelsUseRpc)

namespace MplusM
{
    namespace Common
    {
        /*! @brief A convenience class to provide distinct channels for responses from a service to
         a client. */
        class ClientChannel final : public CLIENT_PORT_CLASS_
        {
        public:
            
            /*! @brief The constructor. */
            ClientChannel(void);
            
            /*! @brief The destructor. */
            virtual ~ClientChannel(void);
            
            /*! @brief Add an output to the channel, using a backoff strategy with retries.
             @param theChannelToBeAdded The output to be added to the channel.
             @param timeToWait The number of seconds allowed before a failure is considered.
             @returns @c true if the channel was opened and @c false if it could not be opened. */
            bool addOutputWithRetries(const yarp::os::ConstString & theChannelToBeAdded,
                                      const double                  timeToWait);
            
            /*! @brief Close the channel. */
            void close(void);
            
            /*! @brief Returns the name associated with the channel.
             @returns The name associated with the channel. */
            inline yarp::os::ConstString name(void)
            const
            {
                return _name;
            } // name
            
            /*! @brief Open the channel, using a backoff strategy with retries.
             @param theChannelName The name to be associated with the channel.
             @param timeToWait The number of seconds allowed before a failure is considered.
             @returns @c true if the channel was opened and @c false if it could not be opened. */
            bool openWithRetries(const yarp::os::ConstString & theChannelName,
                                 const double                  timeToWait);
            
            /*! @brief Release an allocated adapter channel.
             @param theChannel A pointer to the channel to be released. */
            static void RelinquishChannel(ClientChannel * theChannel);
            
        protected:
            
        private:
            
            /*! @brief The class that this class is derived from. */
            typedef CLIENT_PORT_CLASS_ inherited;
            
            /*! @brief Copy constructor.
             
             Note - not implemented and private, to prevent unexpected copying.
             @param other Another object to construct from. */
            ClientChannel(const ClientChannel & other);
            
            /*! @brief Assignment operator.
             
             Note - not implemented and private, to prevent unexpected copying.
             @param other Another object to construct from. */
            ClientChannel & operator =(const ClientChannel & other);
            
            /*! @brief The name associated with the channel. */
            yarp::os::ConstString _name;
            
        }; // ClientChannel
        
    } // Common
    
} // MplusM

#endif // ! defined(MpMClientChannel_H_)
