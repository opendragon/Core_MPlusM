//--------------------------------------------------------------------------------------
//
//  File:       M+MRequestCounterClient.h
//
//  Project:    M+M
//
//  Contains:   The class declaration for the client of the request counter service.
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
//  Created:    2014-03-14
//
//--------------------------------------------------------------------------------------

#if (! defined(MpMRequestCounterClient_H_))
/*! @brief Header guard. */
# define MpMRequestCounterClient_H_ /* */

# include "M+MBaseClient.h"

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 
 @brief The class declaration for the client of the request counter service. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace RequestCounter
    {
        /*! @brief A client for the request counter service. */
        class RequestCounterClient : public CommonX::BaseClient
        {
        public:
            
            /*! @brief The constructor. */
            RequestCounterClient(void);
            
            /*! @brief The destructor. */
            virtual ~RequestCounterClient(void);
            
            /*! @brief Get the statistics from the service.
             @param counter The number of requests since the last reset.
             @param elapsedTime The number of seconds since the last reset.
             @returns @c true if the statistics were retrieved successfully and @c false otherwise. */
            bool getServiceStatistics(long &   counter,
                                      double & elapsedTime);
            
            /*! @brief Trigger the service counter.
             @returns @c true if the service handled the request and @c false otherwise. */
            bool pokeService(void);
            
            /*! @brief Reset the service counters.
             @returns @c true if the service handled the request and @c false otherwise. */
            bool resetServiceCounters(void);
            
        protected:
            
        private:
            
            /*! @brief The class that this class is derived from. */
            typedef BaseClient inherited;
            
            /*! @brief Copy constructor.
             
             Note - not implemented and private, to prevent unexpected copying.
             @param other Another object to construct from. */
            RequestCounterClient(const RequestCounterClient & other);
            
            /*! @brief Assignment operator.
             
             Note - not implemented and private, to prevent unexpected copying.
             @param other Another object to construct from. */
            RequestCounterClient & operator=(const RequestCounterClient & other);
            
        }; // RequestCounterClient
        
    } // RequestCounter
    
} // MplusM

#endif // ! defined(MpMRequestCounterClient_H_)
