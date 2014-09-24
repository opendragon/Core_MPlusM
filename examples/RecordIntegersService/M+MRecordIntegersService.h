//--------------------------------------------------------------------------------------------------
//
//  File:       M+MRecordIntegersService.h
//
//  Project:    M+M
//
//  Contains:   The class declaration for the record integers output service.
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
//  Created:    2014-06-24
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMRecordIntegersService_H_))
# define MpMRecordIntegersService_H_ /* Header guard */

# include <mpm/M+MBaseOutputService.h>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 
 @brief The class declaration for the record integers output service. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

/*! @brief The base channel name to use for the service if not provided. */
# define DEFAULT_RECORDINTEGERS_SERVICE_NAME T_( \
        DEFAULT_SERVICE_NAME_BASE "examples/recordintegers_")

namespace MplusM
{
    namespace Example
    {
        class RecordIntegersInputHandler;
        
        /*! @brief The record integers output stream service. */
        class RecordIntegersService final : public Common::BaseOutputService
        {
        public:
            
            /*! @brief The constructor.
             @param launchPath The command-line name used to launch the service.
             @param tag The modifier for the service name.
             @param serviceEndpointName The YARP name to be assigned to the new service.
             @param servicePortNumber The port being used by the service. */
            RecordIntegersService(const yarp::os::ConstString & launchPath,
                                  const yarp::os::ConstString & tag,
                                  const yarp::os::ConstString & serviceEndpointName,
                                  const yarp::os::ConstString & servicePortNumber = "");
            
            /*! @brief The destructor. */
            virtual ~RecordIntegersService(void);
            
            /*! @brief Configure the input/output streams.
             @param details The configuration information for the input/output streams.
             @returns @c true if the service was successfully configured and @c false otherwise. */
            virtual bool configure(const yarp::os::Bottle & details)
            override;
            
            /*! @brief Restart the input/output streams. */
            virtual void restartStreams(void)
            override;
            
            /*! @brief Start processing requests.
             @returns @c true if the service was started and @c false if it was not. */
            virtual bool start(void)
            override;
            
            /*! @brief Start the input/output streams. */
            virtual void startStreams(void)
            override;
            
            /*! @brief Stop processing requests.
             @returns @c true if the service was stopped and @c false it if was not. */
            virtual bool stop(void)
            override;
            
            /*! @brief Stop the input/output streams. */
            virtual void stopStreams(void)
            override;
            
        protected:
            
        private:
            
            /*! @brief The class that this class is derived from. */
            typedef BaseOutputService inherited;
            
            /*! @brief Copy constructor.
             
             Note - not implemented and private, to prevent unexpected copying.
             @param other Another object to construct from. */
            RecordIntegersService(const RecordIntegersService & other);
            
            /*! @brief Assignment operator.
             
             Note - not implemented and private, to prevent unexpected copying.
             @param other Another object to construct from. */
            RecordIntegersService & operator =(const RecordIntegersService & other);
            
            /*! @brief Set up the descriptions that will be used to construct the input/output
             streams. */
            virtual bool setUpStreamDescriptions(void)
            override;
            
            /*! @brief The path to the output file used for recording. */
            yarp::os::ConstString _outPath;
            
            /*! @brief The file output to be written to. */
            FILE * _outFile;
            
            /*! @brief The handler for input data. */
            RecordIntegersInputHandler * _inHandler;
            
        }; // RecordIntegersService
        
    } // Example
    
} // MplusM

#endif // ! defined(MpMRecordIntegersService_H_)
