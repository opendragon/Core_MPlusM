//--------------------------------------------------------------------------------------------------
//
//  File:       M+MChordGeneratorService.h
//
//  Project:    M+M
//
//  Contains:   The class declaration for an M+M Chord Generator service.
//
//  Written by: Norman Jaffe, Johnty Wang
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
//  Created:    2014-06-05
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMChordGeneratorService_H_))
/*! @brief Header guard. */
# define MpMChordGeneratorService_H_ /* */

# include <mpm/M+MBaseService.h>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 
 @brief The class declaration for a simple M+M service. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

/*! @brief The channel name to use for the service if not provided. */
# define DEFAULT_CHORDGENERATOR_SERVICE_NAME T_(DEFAULT_SERVICE_NAME_BASE "examples/chordGenerator")

namespace MplusM
{
    namespace Example
    {
        class ChordGeneratorRequestHandler;
        
        /*! @brief An example M+M service, handling requests for chords. A list of MIDI note value
         is returned as INTs for a given root note */
        class ChordGeneratorService final : public Common::BaseService
        {
        public:
            
            /*! @brief The constructor.
             @param launchPath The command-line name used to launch the service.
             @param serviceEndpointName The YARP name to be assigned to the new service.
             @param servicePortNumber The port being used by the service. */
            ChordGeneratorService(const yarp::os::ConstString & launchPath,
                                  const yarp::os::ConstString & serviceEndpointName,
                                  const yarp::os::ConstString & servicePortNumber = "");
            
            /*! @brief The destructor. */
            virtual ~ChordGeneratorService(void);
            
            /*! @brief Start processing requests.
             @returns @c true if the service was started and @c false if it was not. */
            virtual bool start(void)
            override;
            
            /*! @brief Stop processing requests.
             @returns @c true if the service was stopped and @c false it if was not. */
            virtual bool stop(void)
            override;
            
        protected:
            
        private:
            
            /*! @brief The class that this class is derived from. */
            typedef BaseService inherited;
            
            /*! @brief Copy constructor.
             
             Note - not implemented and private, to prevent unexpected copying.
             @param other Another object to construct from. */
            ChordGeneratorService(const ChordGeneratorService & other);
            
            /*! @brief Assignment operator.
             
             Note - not implemented and private, to prevent unexpected copying.
             @param other Another object to construct from. */
            ChordGeneratorService & operator =(const ChordGeneratorService & other);
            
            /*! @brief Enable the standard request handlers. */
            void attachRequestHandlers(void);
            
            /*! @brief Disable the standard request handlers. */
            void detachRequestHandlers(void);
            
            /*! @brief The request handler for the Chord request. */
            ChordGeneratorRequestHandler * _chordReqHandler;
            
        }; // ChordGeneratorService
        
    } // Example
    
} // MplusM

#endif // ! defined(MpMChordGeneratorService_H_)
