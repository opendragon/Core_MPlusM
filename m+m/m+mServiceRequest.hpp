//--------------------------------------------------------------------------------------------------
//
//  File:       m+m/m+mServiceRequest.hpp
//
//  Project:    m+m
//
//  Contains:   The class declaration for an m+m request.
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
//  Created:    2014-02-06
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMServiceRequest_HPP_))
# define MpMServiceRequest_HPP_ /* Header guard */

# include <m+m/m+mCommon.hpp>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for an m+m request. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace Common
    {
        class ClientChannel;
        class Endpoint;
        class ServiceResponse;

        /*! @brief The data constituting a service request. */
        class ServiceRequest
        {
        public :

        protected :

        private :

        public :

            /*! @brief The constructor.
             @param[in] requestName The request to be processed. */
            explicit
            ServiceRequest(const YarpString & requestName);

            /*! @brief The constructor.
             @param[in] requestName The request to be processed.
             @param[in] parameters The (optional) parameters for the request. */
            ServiceRequest(const YarpString &       requestName,
                           const yarp::os::Bottle & parameters);

            /*! @brief The destructor. */
            virtual
            ~ServiceRequest(void);

            /*! @brief Send the request to an endpoint for processing.
             @param[in] usingChannel The channel that is to send the request.
             @returns @c true if the request was successfully transmitted. */
            bool
            send(ClientChannel & usingChannel);

            /*! @brief Send the request to an endpoint for processing.
             @param[in] usingChannel The channel that is to send the request.
             @param[in,out] response The response from the request.
             @returns @c true if the request was successfully transmitted. */
            bool
            send(ClientChannel &   usingChannel,
                 ServiceResponse & response);

        protected :

        private :

            /*! @brief The copy constructor.
             @param[in] other The object to be copied. */
            ServiceRequest(const ServiceRequest & other);

            /*! @brief The assignment operator.
             @param[in] other The object to be copied.
             @returns The updated object. */
            ServiceRequest &
            operator =(const ServiceRequest & other);

        public :

        protected :

        private :

            /*! @brief The request name. */
            YarpString _name;

            /*! @brief The request parameters. */
            yarp::os::Bottle _parameters;

        }; // ServiceRequest

    } // Common

} // MplusM

#endif // ! defined(MpMServiceRequest_HPP_)
