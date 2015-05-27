//--------------------------------------------------------------------------------------------------
//
//  File:       mpm/M+MBaseAdapterService.h
//
//  Project:    M+M
//
//  Contains:   The class declaration for the minimal functionality required for an M+M adapter
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

# include <mpm/M+MBaseInputOutputService.h>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for the minimal functionality required for an M+M adapter service. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace Common
    {
        /*! @brief An adapter service. */
        class BaseAdapterService : public Common::BaseInputOutputService
        {
        public :
            
            /*! @brief The constructor.
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
            BaseAdapterService(const YarpString & launchPath,
                               const int          argc,
                               char * *           argv,
                               const YarpString & tag,
                               const bool         useMultipleHandlers,
                               const YarpString & canonicalName,
                               const YarpString & description,
                               const YarpString & requestsDescription,
                               const YarpString & serviceEndpointName,
                               const YarpString & servicePortNumber = "");
            
            /*! @brief The destructor. */
            virtual ~BaseAdapterService(void);
            
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
        
        /*! @brief Process the standard options for service executables.
         The option '-c' / '--channel' displays the endpoint name after applying all other
         options and retunrs @c false.
         The option '-e' / '--endpoint' specifies the endpoint name to be used.
         The option '-g' / '--go' indicates that the service is to be started immediately.
         The option '-h' / '--help' displays the list of optional parameters and arguments and
         returns @c false.
         The option '-i' / '--info' displays the type of the executable, the available options and
         the description of the executable and returns @c false.
         The option '-p' / '--port' specifie the port number to be used.
         The option '-r' / '--report' indicates that the service metrics are to be reported on exit.
         The option '-t' / '--tag' specifies the tag modifier, which is applied to the name of the
         channel, if the name was not specified. It is also applied to the service name as a suffix.
         The option '-v' / '--vers'displays the version and copyright information and returns
         @c false.
         @param argc The number of arguments in 'argv'.
         @param argv The arguments to be used with the service.
         @param argumentDescriptions Descriptions of the arguments to the service.
         @param defaultEndpointNameRoot The default endpoint root name.
         @param adapterDescription A description of the adapter.
         @param matchingCriteria The criteria used to locate the service that the adapter attaches
         to.
         @param year The copyright year for the calling application.
         @param copyrightHolder The name of the entity holding the copyright to the utility.
         @param goWasSet Set to @c true if the service is to be started immediately.
         @param reportOnExit Set to @c true if the -r option is seen.
         @param tag Set to the argument of the last -t option seen.
         @param serviceEndpointName Set to the endpoint name to be used, based on the last -e and -t
         options.
         @param servicePortNumber Set to the argument of the last -p option seen.
         @param skipOptions The command-line options to be skipped.
         @param arguments If non-@c NULL, returns the arguments for the service.
         @returns @c true if the service should continue and @c false if it should leave. */
        bool ProcessStandardAdapterOptions(const int                     argc,
                                           char * *                      argv,
                                           Utilities::DescriptorVector & argumentDescriptions,
                                           const YarpString &            defaultEndpointNameRoot,
                                           const YarpString &            adapterDescription,
                                           const YarpString &            matchingCriteria,
                                           const int                     year,
                                           const char *                  copyrightHolder,
                                           bool &                        goWasSet,
                                           bool &                        reportOnExit,
                                           YarpString &                  tag,
                                           YarpString &                  serviceEndpointName,
                                           YarpString &                  servicePortNumber,
                                           const OptionsMask             skipOptions = kSkipNone,
                                           YarpStringVector *            arguments = NULL);

    } // Common
    
} // MplusM

#endif // ! defined(MpMBaseAdapterService_H_)
