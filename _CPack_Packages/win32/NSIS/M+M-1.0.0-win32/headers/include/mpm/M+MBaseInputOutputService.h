//--------------------------------------------------------------------------------------------------
//
//  File:       mpm/M+MBaseInputOutputService.h
//
//  Project:    M+M
//
//  Contains:   The class declaration for the minimal functionality required for an M+M input/output
//              service.
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
//  Created:    2014-06-23
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMBaseInputOutputService_H_))
# define MpMBaseInputOutputService_H_ /* Header guard */

# include <mpm/M+MBaseService.h>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 
 @brief The class declaration for the minimal functionality required for an M+M input/output
 service. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace Common
    {
        class ConfigureRequestHandler;
        class GeneralChannel;
        class RestartStreamsRequestHandler;
        class StartStreamsRequestHandler;
        class StopStreamsRequestHandler;
        
        /*! @brief An input/output service. */
        class BaseInputOutputService : public Common::BaseService
        {
        public :
            
            /*! @brief The constructor.
             @param theKind The behavioural model for the service.
             @param launchPath The command-line name used to launch the service.
             @param tag The modifier for the service name.
             @param useMultipleHandlers @c true if simultaneous handlers are allowed, @c false if
             one handler is used.
             @param canonicalName The channel-independent name of the service.
             @param description The description of the service.
             @param requestsDescription The description of the requests for the service.
             @param serviceEndpointName The YARP name to be assigned to the new service.
             @param servicePortNumber The channel being used by the service. */
            BaseInputOutputService(const ServiceKind             theKind,
                                   const yarp::os::ConstString & launchPath,
                                   const yarp::os::ConstString & tag,
                                   const bool                    useMultipleHandlers,
                                   const yarp::os::ConstString & canonicalName,
                                   const yarp::os::ConstString & description,
                                   const yarp::os::ConstString & requestsDescription,
                                   const yarp::os::ConstString & serviceEndpointName,
                                   const yarp::os::ConstString & servicePortNumber = "");
            
            /*! @brief The destructor. */
            virtual ~BaseInputOutputService(void);
            
            /*! @brief Configure the input/output streams.
             @param details The configuration information for the input/output streams.
             @returns @c true if the service was successfully configured and @c false otherwise. */
            virtual bool configure(const yarp::os::Bottle & details) = 0;
            
            /*! @brief Fill in the metrics for the service.
             @param metrics The gathered metrics. */
            virtual void gatherMetrics(yarp::os::Bottle & metrics);
            
            /*! @brief Return @c true if the streams are processing data and @c false otherwise.
             @returns @c true if the streams are processing data and @c false otherwise. */
            inline bool isActive(void)
            const
            {
                return _active;
            } // isActive
            
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
            
        protected :
            
            /*! @brief A set of channels. */
            typedef std::vector<GeneralChannel *> StreamVector;
            
            /*! @brief Add a set of input channels from a set of descriptions.
             @param descriptions The descriptions of the channels.
             @returns @c true if the channels were constructed and @c false otherwise. */
            bool addInStreamsFromDescriptions(const ChannelVector & descriptions);
            
            /*! @brief Add a set of output channels from a set of descriptions.
             @param descriptions The descriptions of the channels.
             @returns @c true if the channels were constructed and @c false otherwise. */
            bool addOutStreamsFromDescriptions(const ChannelVector & descriptions);
            
            /*! @brief Indicate that the streams are not processing data. */
            inline void clearActive(void)
            {
                _active = false;
            } // clearActive
            
            /*! @brief Indicate that the streams are processing data. */
            inline void setActive(void)
            {
                _active = true;
            } // setActive
            
            /*! @brief Set up the input streams.
             @returns @c true if the channels were set up and @c false otherwise. */
            virtual bool setUpInputStreams(void);
            
            /*! @brief Set up the output streams.
             @returns @c true if the channels were set up and @c false otherwise. */
            virtual bool setUpOutputStreams(void);
            
            /*! @brief Set up the descriptions that will be used to construct the input/output
             streams.
             @returns @c true if the descriptions were set up and @c false otherwise. */
            virtual bool setUpStreamDescriptions(void) = 0;
            
            /*! @brief Shut down the input streams.
             @returns @c true if the channels were shut down and @c false otherwise. */
            virtual bool shutDownInputStreams(void);
            
            /*! @brief Shut down the output streams.
             @returns @c true if the channels were shut down and @c false otherwise. */
            virtual bool shutDownOutputStreams(void);
            
        private :
            
            COPY_AND_ASSIGNMENT_(BaseInputOutputService);
            
            /*! @brief Enable the standard request handlers. */
            void attachRequestHandlers(void);
            
            /*! @brief Disable the standard request handlers. */
            void detachRequestHandlers(void);
            
            /*! @brief Fill in a list of secondary input channels for the service.
             @param channels The list of channels to be filled in. */
            virtual void fillInSecondaryInputChannelsList(ChannelVector & channels);
            
            /*! @brief Fill in a list of secondary output channels for the service.
             @param channels The list of channels to be filled in. */
            virtual void fillInSecondaryOutputChannelsList(ChannelVector & channels);
            
        public :
        
        protected :
        
            /*! @brief The set of input channels. */
            StreamVector _inStreams;
            
            /*! @brief The set of output channels. */
            StreamVector _outStreams;
            
        private :
            
            /*! @brief The class that this class is derived from. */
            typedef BaseService inherited;
            
            /*! @brief The request handler for the 'configure' request. */
            ConfigureRequestHandler * _configureHandler;
            
            /*! @brief The request handler for the 'restartStreams' request. */
            RestartStreamsRequestHandler * _restartStreamsHandler;
            
            /*! @brief The request handler for the 'startStreams' request. */
            StartStreamsRequestHandler * _startStreamsHandler;
            
            /*! @brief The request handler for the 'stopStreams' request. */
            StopStreamsRequestHandler * _stopStreamsHandler;
            
            /*! @brief @c true if the streams are processing data and @c false otherwise. */
            bool _active;
            
# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunused-private-field"
# endif // defined(__APPLE__)
            /*! @brief Filler to pad to alignment boundary */
            char _filler[7];
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)
            
        }; // BaseInputOutputService
        
    } // Common
    
} // MplusM

#endif // ! defined(MpMBaseInputOutputService_H_)
