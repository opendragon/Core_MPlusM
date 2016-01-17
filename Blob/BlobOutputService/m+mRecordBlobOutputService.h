//--------------------------------------------------------------------------------------------------
//
//  File:       m+mRecordBlobOutputService.h
//
//  Project:    m+m
//
//  Contains:   The class declaration for the Record Blob output service.
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
//  Created:    2015-06-25
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMRecordBlobOutputService_H_))
# define MpMRecordBlobOutputService_H_ /* Header guard */

# include <m+m/m+mBaseOutputService.h>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for the Record %Blob output service. */

/*! @namespace MplusM::RecordBlob
 @brief The classes that support recording messages as JSON. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

/*! @brief The base channel name to use for the service if not provided. */
# define DEFAULT_RECORDBLOBOUTPUT_SERVICE_NAME_ T_(DEFAULT_SERVICE_NAME_BASE_ \
                                                     "output/recordblob")

/*! @brief The description of the service. */
# define RECORDBLOBOUTPUT_SERVICE_DESCRIPTION_ T_("Record Blob output service")

namespace MplusM
{
    namespace RecordBlob
    {
        class RecordBlobOutputInputHandler;
        
        /*! @brief The Record As JSON output service. */
        class RecordBlobOutputService : public Common::BaseOutputService
        {
        public :
        
        protected :
        
        private :
            
            /*! @brief The class that this class is derived from. */
            typedef BaseOutputService inherited;
            
        public :
            
            /*! @brief The constructor.
             @param argumentList Descriptions of the arguments to the executable.
             @param launchPath The command-line name used to launch the service.
             @param argc The number of arguments in 'argv'.
             @param argv The arguments passed to the executable used to launch the service.
             @param tag The modifier for the service name and port names.
             @param serviceEndpointName The YARP name to be assigned to the new service.
             @param servicePortNumber The port being used by the service. */
            RecordBlobOutputService(const Utilities::DescriptorVector & argumentList,
                                    const YarpString &                  launchPath,
                                    const int                           argc,
                                    char * *                            argv,
                                    const YarpString &                  tag,
                                    const YarpString &                  serviceEndpointName,
                                    const YarpString &                  servicePortNumber = "");
            
            /*! @brief The destructor. */
            virtual
            ~RecordBlobOutputService(void);
            
            DECLARE_CONFIGURE_;
            
            DECLARE_DISABLEMETRICS_;
            
            DECLARE_ENABLEMETRICS_;
            
            DECLARE_GETCONFIGURATION_;

            DECLARE_RESTARTSTREAMS_;
            
            DECLARE_STARTSERVICE_;
            
            DECLARE_STARTSTREAMS_;
            
            DECLARE_STOPSERVICE_;
            
            DECLARE_STOPSTREAMS_;
            
        protected :
            
        private :
            
            COPY_AND_ASSIGNMENT_(RecordBlobOutputService);
            
            DECLARE_SETUPSTREAMDESCRIPTIONS_;
            
        public :
        
        protected :
        
        private :
            
            /*! @brief The path to the output file used for recording. */
            YarpString _outPath;
            
            /*! @brief The file output to be written to. */
            FILE * _outFile;
            
            /*! @brief The handler for input data. */
            RecordBlobOutputInputHandler * _inHandler;
            
        }; // RecordBlobOutputService
        
    } // RecordBlob
    
} // MplusM

#endif // ! defined(MpMRecordBlobOutputService_H_)
