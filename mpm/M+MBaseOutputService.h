//--------------------------------------------------------------------------------------------------
//
//  File:       mpm/M+MBaseOutputService.h
//
//  Project:    M+M
//
//  Contains:   The class declaration for the minimal functionality required for an M+M output
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

#if (! defined(MpMBaseOutputService_H_))
# define MpMBaseOutputService_H_ /* Header guard */

# include <mpm/M+MBaseInputOutputService.h>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for the minimal functionality required for an M+M output service. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace Common
    {
        /*! @brief An output service. */
        class BaseOutputService : public Common::BaseInputOutputService
        {
        public :
            
            /*! @brief The constructor.
             @param launchPath The command-line name used to launch the service.
             @param tag The modifier for the service name.
             @param useMultipleHandlers @c true if simultaneous handlers are allowed, @c false if
             one handler is used.
             @param canonicalName The channel-independent name of the service.
             @param description The description of the service.
             @param requestsDescription The description of the requests for the service.
             @param serviceEndpointName The YARP name to be assigned to the new service.
             @param servicePortNumber The channel being used by the service. */
            BaseOutputService(const yarp::os::ConstString & launchPath,
                              const yarp::os::ConstString & tag,
                              const bool                    useMultipleHandlers,
                              const yarp::os::ConstString & canonicalName,
                              const yarp::os::ConstString & description,
                              const yarp::os::ConstString & requestsDescription,
                              const yarp::os::ConstString & serviceEndpointName,
                              const yarp::os::ConstString & servicePortNumber = "");
            
            /*! @brief The destructor. */
            virtual ~BaseOutputService(void);
            
        protected :
            
            /*! @brief The descriptions of the input streams. */
            ChannelVector _inDescriptions;
            
        private :
            
            COPY_AND_ASSIGNMENT_(BaseOutputService);
            
            /*! @brief Set up the input channels.
             @returns @c true if the channels were set up and @c false otherwise. */
            virtual bool setUpInputStreams(void);
            
            /*! @brief Shut down the input streams.
             @returns @c true if the channels were shut down and @c false otherwise. */
            virtual bool shutDownInputStreams(void);
            
        public :
        
        protected :
        
        private :
            
            /*! @brief The class that this class is derived from. */
            typedef BaseInputOutputService inherited;
            
        }; // BaseOutputService
        
    } // Common
    
} // MplusM

#endif // ! defined(MpMBaseOutputService_H_)
