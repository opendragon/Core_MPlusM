//--------------------------------------------------------------------------------------
//
//  File:       M+MBaseAdapterData.h
//
//  Project:    M+M
//
//  Contains:   The class declaration for the minimal functionality required for the data
//              shared between the input handlers and main thread of a M+M adapter.
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
//  Created:    2014-03-27
//
//--------------------------------------------------------------------------------------

#if (! defined(MpMBaseAdapterData_H_))
# define MpMBaseAdapterData_H_ /* Header guard */

# include "M+MCommon.h"

# include <list>
# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wc++11-extensions"
#  pragma clang diagnostic ignored "-Wdocumentation"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#  pragma clang diagnostic ignored "-Wpadded"
#  pragma clang diagnostic ignored "-Wshadow"
#  pragma clang diagnostic ignored "-Wunused-parameter"
#  pragma clang diagnostic ignored "-Wweak-vtables"
# endif // defined(__APPLE__)
# include <yarp/os/Mutex.h>
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 
 @brief The class declaration for the minimal functionality required for the data shared
 between the input handlers and main thread of a M+M adapter. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace Common
    {
        class AdapterChannel;
        class BaseClient;
        
        /*! @brief A handler for partially-structured input data. */
        class BaseAdapterData
        {
        public:
            
            /*! @brief The constructor.
             @param client The client connection that is used to communicate with the service.
             @param output The output channel that will receive the service responses. */
            BaseAdapterData(BaseClient *     client,
                            AdapterChannel * output);
            
            /*! @brief The destructor. */
            virtual ~BaseAdapterData(void);
            
            /*! @brief Mark the adapter as active.
             @returns @c true if the adapter was already active and @c false otherwise. */
            bool activate(void);
            
            /*! @brief Lock the data unless the lock would block.
             @returns @c true if the data was locked and @c false otherwise. */
            inline bool conditionallyLock(void)
            {
                return _lock.tryLock();
            } // conditionallyLock
            
            /*! @brief Mark the adapter as inactive.
             @returns @c true if the adapter was active and @c false otherwise. */
            bool deactivate(void);
            
            /*! @brief Return the client.
             @returns The client. */
            inline BaseClient * getClient(void)
            const
            {
                return _client;
            } // getClient
            
            /*! @brief Return the output channel.
             @returns The output channel. */
            inline AdapterChannel * getOutput(void)
            const
            {
                return _output;
            } // getOutput
            
            /*! @brief Return the adapter state.
             @returns @c true if the adapter is active and @c false otherwise. */
            inline bool isActive(void)
            const
            {
                return _active;
            } // isActive
            
            
            /*! @brief Lock the data. */
            inline void lock(void)
            {
                _lock.lock();
            } // lock
            
            /*! @brief Unlock the data. */
            inline void unlock(void)
            {
                _lock.unlock();
            } // unlock
            
        protected:

        private:
            
            /*! @brief Copy constructor.
             
             Note - not implemented and private, to prevent unexpected copying.
             @param other Another object to construct from. */
            BaseAdapterData(const BaseAdapterData & other);
            
            /*! @brief Assignment operator.
             
             Note - not implemented and private, to prevent unexpected copying.
             @param other Another object to construct from. */
            BaseAdapterData & operator=(const BaseAdapterData & other);
            
            /*! @brief The contention lock used to avoid intermixing of outputs. */
            yarp::os::Mutex  _lock;
            
            /*! @brief The output channel to use. */
            AdapterChannel * _output;
            
            /*! @brief The connection to the service. */
            BaseClient *     _client;
            
            /*! @brief @c true if the adapter is active and @c false otherwise. */
            bool             _active;
            
# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunused-private-field"
# endif // defined(__APPLE__)
            /*! @brief Filler to pad to alignment boundary */
            char             _filler[7];
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)
            
        }; // BaseAdapterData
        
    } // Common
    
} // MplusM

#endif // ! defined(MpMBaseAdapterData_H_)
