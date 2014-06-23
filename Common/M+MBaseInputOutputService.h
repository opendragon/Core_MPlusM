//--------------------------------------------------------------------------------------
//
//  File:       M+MBaseInputOutputService.h
//
//  Project:    M+M
//
//  Contains:   The class declaration for the minimal functionality required for an M+M
//              input/output service.
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
//  Created:    2014-06-23
//
//--------------------------------------------------------------------------------------

#if (! defined(MpMBaseInputOutputService_H_))
# define MpMBaseInputOutputService_H_ /* Header guard */

# include "M+MBaseService.h"

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 
 @brief The class declaration for the minimal functionality required for an M+M input/output service. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace Common
    {
        class RestartStreamsRequestHandler;
        class StartStreamsRequestHandler;
        class StopStreamsRequestHandler;
        
        /*! @brief An input/output service. */
        class BaseInputOutputService : public Common::BaseService
        {
        public:
            
            /*! @brief The constructor.
             @param theKind The behavioural model for the service.
             @param launchPath The command-line name used to launch the service.
             @param useMultipleHandlers @c true if simultaneous handlers are allowed, @c false if one handler is used.
             @param canonicalName The channel-independent name of the service.
             @param description The description of the service.
             @param requestsDescription The description of the requests for the service.
             @param serviceEndpointName The YARP name to be assigned to the new service.
             @param serviceHostName The name or IP address of the machine running the service.
             @param servicePortNumber The channel being used by the service. */
            BaseInputOutputService(const ServiceKind             theKind,
                                   const char *                  launchPath,
                                   const bool                    useMultipleHandlers,
                                   const yarp::os::ConstString & canonicalName,
                                   const yarp::os::ConstString & description,
                                   const yarp::os::ConstString & requestsDescription,
                                   const yarp::os::ConstString & serviceEndpointName,
                                   const yarp::os::ConstString & serviceHostName = "",
                                   const yarp::os::ConstString & servicePortNumber = "");
            
            /*! @brief The constructor.
             @param theKind The behavioural model for the service.
             @param useMultipleHandlers @c true if simultaneous handlers are allowed, @c false if one handler is used.
             @param canonicalName The channel-independent name of the service.
             @param description The description of the service.
             @param requestsDescription The description of the requests for the service.
             @param argc The number of arguments in 'argv'.
             @param argv The arguments to be used to specify the new service. */
            BaseInputOutputService(const ServiceKind             theKind,
                                   const bool                    useMultipleHandlers,
                                   const yarp::os::ConstString & canonicalName,
                                   const yarp::os::ConstString & description,
                                   const yarp::os::ConstString & requestsDescription,
                                   const int                     argc,
                                   char * *                      argv);
            
            /*! @brief The destructor. */
            virtual ~BaseInputOutputService(void);
            
            /*! @brief Restart the input/output streams. */
            virtual void restartStreams(void) = 0;

            /*! @brief Start processing requests.
             @returns @c true if the service was started and @c false if it was not. */
            virtual bool start(void);
            
            /*! @brief Start the input/output streams. */
            virtual void startStreams(void) = 0;
            
            /*! @brief Stop processing requests.
             @returns @c true if the service was stopped and @c false it if was not. */
            virtual bool stop(void);
            
            /*! @brief Stop the input/output streams. */
            virtual void stopStreams(void) = 0;

        protected:
            
            /*! @brief Set up the input streams.
             @returns @c true if the channels were set up and @c false otherwise. */
            virtual bool setUpInputStreams(void) = 0;
            
            /*! @brief Set up the output streams.
             @returns @c true if the channels were set up and @c false otherwise. */
            virtual bool setUpOutputStreams(void) = 0;
            
            /*! @brief Shut down the input streams.
             @returns @c true if the channels were shut down and @c false otherwise. */
            virtual bool shutDownInputStreams(void) = 0;
            
            /*! @brief Shut down the output streams.
             @returns @c true if the channels were shut down and @c false otherwise. */
            virtual bool shutDownOutputStreams(void) = 0;
            
        private:
            
            /*! @brief The class that this class is derived from. */
            typedef BaseService inherited;
            
            /*! @brief Enable the standard request handlers. */
            void attachRequestHandlers(void);
            
            /*! @brief Disable the standard request handlers. */
            void detachRequestHandlers(void);
            
            /*! @brief The request handler for the 'restartStreams' request. */
            RestartStreamsRequestHandler * _restartStreamsHandler;

            /*! @brief The request handler for the 'startStreams' request. */
            StartStreamsRequestHandler *   _startStreamsHandler;
            
            /*! @brief The request handler for the 'stopStreams' request. */
            StopStreamsRequestHandler *    _stopStreamsHandler;
            
        }; // BaseInputOutputService
        
    } // Test
    
} // MplusM

#endif // ! defined(MpMBaseInputOutputService_H_)
