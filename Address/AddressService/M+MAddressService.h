//--------------------------------------------------------------------------------------------------
//
//  File:       M+MAddressService.h
//
//  Project:    M+M
//
//  Contains:   The class declaration for a service that returns an internet address upon request.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2015 by HPlus Technologies Ltd. and Simon Fraser University.
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
//  Created:    2015-02-11
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMAddressService_H_))
# define MpMAddressService_H_ /* Header guard */

# include <mpm/M+MBaseService.h>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for a service that returns an IP address and port upon request. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

/*! @brief The channel name to use for the service if not provided. */
# define DEFAULT_ADDRESS_SERVICE_NAME T_(DEFAULT_SERVICE_NAME_BASE "address")

namespace MplusM
{
    namespace Address
    {
        class WhereRequestHandler;
        
        /*! @brief The request counter service. */
        class AddressService : public Common::BaseService
        {
        public :
            
            /*! @brief The constructor.
             @param hostName The host address to be returned.
             @param hostPort The port to be returned.
             @param launchPath The command-line name used to launch the service.
             @param tag The modifier for the service name and port names.
             @param serviceEndpointName The YARP name to be assigned to the new service.
             @param servicePortNumber The port being used by the service. */
            AddressService(const yarp::os::ConstString & hostName,
                           const int                     hostPort,
                           const yarp::os::ConstString & launchPath,
                           const yarp::os::ConstString & tag,
                           const yarp::os::ConstString & serviceEndpointName,
                           const yarp::os::ConstString & servicePortNumber = "");
            
            /*! @brief The destructor. */
            virtual ~AddressService(void);
            
            /*! @brief Return the remembered address.
             @param address The remembered address.
             @param port The remembered port. */
            void getAddress(yarp::os::ConstString & address,
                            int &                   port);
            
            /*! @brief Start processing requests.
             @returns @c true if the service was started and @c false if it was not. */
            virtual bool start(void);
            
            /*! @brief Stop processing requests.
             @returns @c true if the service was stopped and @c false it if was not. */
            virtual bool stop(void);
            
        protected :
            
        private :
            
            COPY_AND_ASSIGNMENT_(AddressService);
            
            /*! @brief Enable the standard request handlers. */
            void attachRequestHandlers(void);
            
            /*! @brief Disable the standard request handlers. */
            void detachRequestHandlers(void);
            
        public :
        
        protected :
        
        private :
            
            /*! @brief The class that this class is derived from. */
            typedef BaseService inherited;
            
            /*! @brief The remembered address. */
            yarp::os::ConstString _address;
            
            /*! @brief The request handler for the 'where' request. */
            WhereRequestHandler * _whereHandler;
            
            /*! @brief The remembered port. */
            int _port;
            
        }; // AddressService
        
    } // Address
    
} // MplusM

#endif // ! defined(MpMAddressService_H_)
