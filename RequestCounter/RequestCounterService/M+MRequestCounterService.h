//--------------------------------------------------------------------------------------
//
//  File:       M+MRequestCounterService.h
//
//  Project:    M+M
//
//  Contains:   The class declaration for a service that collects statistic on requests.
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

#if (! defined(MpMRequestCounterService_H_))
# define MpMRequestCounterService_H_ /* Header guard */

# include "M+MBaseService.h"

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 
 @brief The class declaration for a service that collects statistic on requests. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

/*! @brief The channel name to use for the service if not provided. */
# define DEFAULT_REQUESTCOUNTER_SERVICE_NAME T_(DEFAULT_SERVICE_NAME_BASE "requestCounter")

namespace MplusM
{
    namespace RequestCounter
    {
        class DetachRequestHandler;
        class RequestCounterDefaultRequestHandler;
        class ResetRequestHandler;
        class StatsRequestHandler;
        
        /*! @brief The request counter service. */
        class RequestCounterService : public Common::BaseService
        {
        public:
            
            /*! @brief The constructor.
             @param launchPath The command-line name used to launch the service.
             @param serviceEndpointName The YARP name to be assigned to the new service.
             @param servicePortNumber The port being used by the service. */
            RequestCounterService(const yarp::os::ConstString & launchPath,
                                  const yarp::os::ConstString & serviceEndpointName,
                                  const yarp::os::ConstString & servicePortNumber = "");
            
            /*! @brief The destructor. */
            virtual ~RequestCounterService(void);
            
            /*! @brief Start processing requests.
             @returns @c true if the service was started and @c false if it was not. */
            virtual bool start(void);
            
            /*! @brief Stop processing requests.
             @returns @c true if the service was stopped and @c false it if was not. */
            virtual bool stop(void);
            
            /*! @brief Record a request.
             @param key The client-provided key. */
            void countRequest(const yarp::os::ConstString & key);
            
            /*! @brief Return the request statistics.
             @param key The client-provided key. 
             @param counter The number of requests since the last reset.
             @param elapsedTime The number of seconds since the last reset. */
            void getStatistics(const yarp::os::ConstString & key,
                               long &                        counter,
                               double &                      elapsedTime);
            
            /*! @brief Reset the request statistics counters.
             @param key The client-provided key. */
            void resetCounters(const yarp::os::ConstString & key);
            
        protected:
            
        private:
            
            /*! @brief The class that this class is derived from. */
            typedef BaseService inherited;
            
            /*! @brief Copy constructor.
             
             Note - not implemented and private, to prevent unexpected copying.
             @param other Another object to construct from. */
            RequestCounterService(const RequestCounterService & other);
            
            /*! @brief Assignment operator.
             
             Note - not implemented and private, to prevent unexpected copying.
             @param other Another object to construct from. */
            RequestCounterService & operator=(const RequestCounterService & other);
            
            /*! @brief Enable the standard request handlers. */
            void attachRequestHandlers(void);
            
            /*! @brief Disable the standard request handlers. */
            void detachRequestHandlers(void);
            
            /*! @brief The request handler for unrecognized requests. */
            RequestCounterDefaultRequestHandler * _defaultHandler;
            
            /*! @brief The request handler for the 'reset' request. */
            ResetRequestHandler *                 _resetHandler;
            
            /*! @brief The request handler for the 'stats' request. */
            StatsRequestHandler *                 _statsHandler;
            
        }; // RequestCounterService
        
    } // RequestCounter
    
} // MplusM

#endif // ! defined(MpMRequestCounterService_H_)
