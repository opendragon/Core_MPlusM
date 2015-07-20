//--------------------------------------------------------------------------------------------------
//
//  File:       m+m/m+mBaseAdapterService.h
//
//  Project:    m+m
//
//  Contains:   The class declaration for the minimal functionality required for an m+m adapter
//              service.
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
//  Created:    2015-05-27
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMBaseAdapterService_H_))
# define MpMBaseAdapterService_H_ /* Header guard */

# include <m+m/m+mBaseInputOutputService.h>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for the minimal functionality required for an m+m adapter service. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace Common
    {
        class BaseAdapterData;

        /*! @brief An adapter service. */
        class BaseAdapterService : public Common::BaseInputOutputService
        {
        public :
            
            /*! @brief The constructor.
             @param argumentList Descriptions of the arguments to the executable.
             @param launchPath The command-line name used to launch the service.
             @param argc The number of arguments in 'argv'.
             @param argv The arguments passed to the executable used to launch the service.
             @param tag The modifier for the service name.
             @param useMultipleHandlers @c true if simultaneous handlers are allowed, @c false if
             one handler is used.
             @param canonicalName The channel-independent name of the service.
             @param description The description of the service.
             @param requestsDescription The description of the requests for the service.
             @param serviceEndpointName The YARP name to be assigned to the new service.
             @param servicePortNumber The channel being used by the service. */
            BaseAdapterService(const Utilities::DescriptorVector & argumentList,
                               const YarpString &                  launchPath,
                               const int                           argc,
                               char * *                            argv,
                               const YarpString &                  tag,
                               const bool                          useMultipleHandlers,
                               const YarpString &                  canonicalName,
                               const YarpString &                  description,
                               const YarpString &                  requestsDescription,
                               const YarpString &                  serviceEndpointName,
                               const YarpString &                  servicePortNumber = "");
            
            /*! @brief The destructor. */
            virtual ~BaseAdapterService(void);
            
            /*! @brief Start the service and set up its configuration.
             @param sharedData The shared data for the input handlers.
             @param helpText The help text to be displayed.
             @param goWasSet @c true if the service is to be started immediately.
             @param stdinAvailable @c true if running in the foreground and @c false otherwise.
             @param reportOnExit @c true if service metrics are to be reported on exit and @c false
             otherwise. */
            void performLaunch(BaseAdapterData &  sharedData,
                               const YarpString & helpText,
                               const bool         goWasSet,
                               const bool         stdinAvailable,
                               const bool         reportOnExit);
            
        protected :
            
        private :
            
            COPY_AND_ASSIGNMENT_(BaseAdapterService);
            
            /*! @brief Set up the client channels.
             @returns @c true if the channels were set up and @c false otherwise. */
            virtual bool setUpClientStreams(void);
            
            /*! @brief Set up the input channels.
             @returns @c true if the channels were set up and @c false otherwise. */
            virtual bool setUpInputStreams(void);
            
            /*! @brief Set up the output channels.
             @returns @c true if the channels were set up and @c false otherwise. */
            virtual bool setUpOutputStreams(void);
            
            /*! @brief Shut down the client streams.
             @returns @c true if the channels were shut down and @c false otherwise. */
            virtual bool shutDownClientStreams(void);
            
            /*! @brief Shut down the input streams.
             @returns @c true if the channels were shut down and @c false otherwise. */
            virtual bool shutDownInputStreams(void);
            
            /*! @brief Shut down the output streams.
             @returns @c true if the channels were shut down and @c false otherwise. */
            virtual bool shutDownOutputStreams(void);
            
        public :
        
        protected :
        
            /*! @brief The descriptions of the client streams. */
            ChannelVector _clientDescriptions;

            /*! @brief The descriptions of the input streams. */
            ChannelVector _inDescriptions;
            
            /*! @brief The descriptions of the output streams. */
            ChannelVector _outDescriptions;
            
        private :
            
            /*! @brief The class that this class is derived from. */
            typedef BaseInputOutputService inherited;
            
        }; // BaseAdapterService
        
    } // Common
    
} // MplusM

#endif // ! defined(MpMBaseAdapterService_H_)
