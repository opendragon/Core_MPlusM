//--------------------------------------------------------------------------------------------------
//
//  File:       m+mAddressService.h
//
//  Project:    m+m
//
//  Contains:   The class declaration for a service that returns an internet address upon request.
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
//  Created:    2015-02-11
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMAddressService_H_))
# define MpMAddressService_H_ /* Header guard */

# include <m+m/m+mBaseService.h>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for a service that returns an IP address and port upon request. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

/*! @brief The channel name to use for the service if not provided. */
# define DEFAULT_ADDRESS_SERVICE_NAME_ BUILD_NAME_(MpM_SERVICE_BASE_NAME_, "address")

/*! @brief The description of the service. */
# define ADDRESS_SERVICE_DESCRIPTION_ T_("Address service")

namespace MplusM
{
    namespace Address
    {
        class WhereRequestHandler;

        /*! @brief The Address service. */
        class AddressService : public Common::BaseService
        {
        public :

        protected :

        private :

            /*! @brief The class that this class is derived from. */
            typedef BaseService inherited;

        public :

            /*! @brief The constructor.
             @param hostName The host address to be returned.
             @param hostPort The port to be returned.
             @param launchPath The command-line name used to launch the service.
             @param argc The number of arguments in 'argv'.
             @param argv The arguments passed to the executable used to launch the service.
             @param tag The modifier for the service name and port names.
             @param serviceEndpointName The YARP name to be assigned to the new service.
             @param servicePortNumber The port being used by the service. */
            AddressService(const YarpString & hostName,
                           const int          hostPort,
                           const YarpString & launchPath,
                           const int          argc,
                           char * *           argv,
                           const YarpString & tag,
                           const YarpString & serviceEndpointName,
                           const YarpString & servicePortNumber = "");

            /*! @brief The destructor. */
            virtual
            ~AddressService(void);

            /*! @brief Return the remembered address.
             @param address The remembered address.
             @param port The remembered port. */
            void
            getAddress(YarpString & address,
                       int &        port);

        protected :

        private :

            /*! @brief The copy constructor.
             @param other The object to be copied. */
            AddressService(const AddressService & other);

            /*! @brief Enable the standard request handlers. */
            void
            attachRequestHandlers(void);

            /*! @brief Disable the standard request handlers. */
            void
            detachRequestHandlers(void);

            /*! @brief The assignment operator.
             @param other The object to be copied.
             @returns The updated object. */
            AddressService &
            operator =(const AddressService & other);

        public :

        protected :

        private :

            /*! @brief The remembered address. */
            YarpString _address;

            /*! @brief The request handler for the 'where' request. */
            WhereRequestHandler * _whereHandler;

            /*! @brief The remembered port. */
            int _port;

        }; // AddressService

    } // Address

} // MplusM

#endif // ! defined(MpMAddressService_H_)
