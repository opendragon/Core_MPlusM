//--------------------------------------------------------------------------------------
//
//  File:       M+MBaseInputOutputClient.h
//
//  Project:    M+M
//
//  Contains:   The class declaration for the minimal functionality required for an M+M
//              input/output client.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2014 by HPlus Technologies Ltd. and Simon Fraser University.
//
//              All rights reserved. Redistribution and use in source and binary forms,
//              with or without modification, are permitted provided that the following
//              conditions are met:
//                * Redistributions of source code must retain the above copyright
//                  notice, this list of conditions and the following disclaimer.
//                * Redistributions in binary form must reproduce the above copyright
//                  notice, this list of conditions and the following disclaimer in the
//                  documentation and/or other materials provided with the
//                  distribution.
//                * Neither the name of the copyright holders nor the names of its
//                  contributors may be used to endorse or promote products derived
//                  from this software without specific prior written permission.
//
//              THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
//              "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
//              LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
//              PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
//              OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
//              SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
//              LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
//              DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
//              THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
//              (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
//              OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
//  Created:    2014-06-25
//
//--------------------------------------------------------------------------------------

#if (! defined(MpMBaseInputOutputClient_H_))
# define MpMBaseInputOutputClient_H_ /* Header guard */

# include "M+MBaseClient.h"

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 
 @brief The class declaration for the minimal functionality required for an M+M
 input/output client. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace Common
    {
        /*! @brief An example M+M client, for the 'random' service. */
        class BaseInputOutputClient : public Common::BaseClient
        {
        public:
            
            /*! @brief The constructor.
             @param baseChannelName The name to be used as the root for the client channel. */
            BaseInputOutputClient(const char * baseChannelName = DEFAULT_CHANNEL_ROOT);
            
            /*! @brief The destructor. */
            virtual ~BaseInputOutputClient(void);
            
            /*! @brief Configure the input/output streams.
             @param details The configuration information for the input/output streams.
             @returns @c true if the service was successfully configured and @c false otherwise. */
            bool configure(const Package & details);
            
            /*! @brief Restart the input/output streams.
             @returns @c true if the input/output streams were successfully restarted and @c false otherwise. */
            bool restartStreams(void);
            
            /*! @brief Start the input/output streams.
             @returns @c true if the input/output streams were successfully started and @c false otherwise. */
            bool startStreams(void);
            
            /*! @brief Stop the input/output streams.
             @returns @c true if the input/output streams were successfully stopped and @c false otherwise. */
            bool stopStreams(void);
            
        protected:
            
        private:
            
            /*! @brief The class that this class is derived from. */
            typedef BaseClient inherited;
            
            /*! @brief Copy constructor.
             
             Note - not implemented and private, to prevent unexpected copying.
             @param other Another object to construct from. */
            BaseInputOutputClient(const BaseInputOutputClient & other);
            
            /*! @brief Assignment operator.
             
             Note - not implemented and private, to prevent unexpected copying.
             @param other Another object to construct from. */
            BaseInputOutputClient & operator=(const BaseInputOutputClient & other);
            
        }; // BaseInputOutputClient
        
    } // Common
    
} // MplusM

#endif // ! defined(MpMBaseInputOutputClient_H_)
