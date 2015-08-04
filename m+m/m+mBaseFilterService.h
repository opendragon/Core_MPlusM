//--------------------------------------------------------------------------------------------------
//
//  File:       m+m/m+mBaseFilterService.h
//
//  Project:    m+m
//
//  Contains:   The class declaration for the minimal functionality required for an m+m filter
//              service.
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
//  Created:    2014-06-23
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMBaseFilterService_H_))
# define MpMBaseFilterService_H_ /* Header guard */

# include <m+m/m+mBaseInputOutputService.h>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for the minimal functionality required for an m+m filter service. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace Common
    {
        /*! @brief A filter service. */
        class BaseFilterService : public Common::BaseInputOutputService
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
            BaseFilterService(const Utilities::DescriptorVector & argumentList,
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
            virtual ~BaseFilterService(void);
            
        protected :
            
        private :
            
            COPY_AND_ASSIGNMENT_(BaseFilterService);
            
            DECLARE_SETUPINPUTSTREAMS_;
            
            DECLARE_SETUPOUTPUTSTREAMS_;
            
            DECLARE_SHUTDOWNINPUTSTREAMS_;
            
            DECLARE_SHUTDOWNOUTPUTSTREAMS_;
            
        public :
        
        protected :
        
            /*! @brief The descriptions of the input streams. */
            ChannelVector _inDescriptions;
            
            /*! @brief The descriptions of the output streams. */
            ChannelVector _outDescriptions;
            
        private :
            
            /*! @brief The class that this class is derived from. */
            typedef BaseInputOutputService inherited;
            
        }; // BaseFilterService
        
    } // Common
    
} // MplusM

#endif // ! defined(MpMBaseFilterService_H_)
