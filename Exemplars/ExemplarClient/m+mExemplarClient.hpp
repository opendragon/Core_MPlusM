//--------------------------------------------------------------------------------------------------
//
//  File:       m+mExemplarClient.hpp
//
//  Project:    m+m
//
//  Contains:   The class declaration for the exemplar client.
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
//  Created:    2014-09-15
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMExemplarClient_HPP_))
# define MpMExemplarClient_HPP_ /* Header guard */

# include <m+m/m+mBaseClient.hpp>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for the exemplar client. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace Exemplar
    {
        /*! @brief The exemplar client. */
        class ExemplarClient : public Common::BaseClient
        {
        public :

        protected :

        private :

            /*! @brief The class that this class is derived from. */
            typedef BaseClient inherited;

        public :

            /*! @brief The constructor. */
            ExemplarClient(void);

            /*! @brief The destructor. */
            virtual
            ~ExemplarClient(void);

            /*! @brief Get one random number from the service.
             @param[out] result Where to return the number.
             @return @c true if the number was retrieved successfully and @c false otherwise. */
            bool
            getOneRandomNumber(double & result);

            /*! @brief Get a sequence of random numbers from the service.
             @param[in] howMany The number of random numbers to retrieve.
             @param[out] result Where to return the numbers.
             @return @c true if the numbere were retrieved successfully and @c false otherwise. */
            bool
            getRandomNumbers(const int              howMany,
                             Common::DoubleVector & result);

        protected :

        private :

            /*! @brief The copy constructor.
             @param[in] other The object to be copied. */
            ExemplarClient(const ExemplarClient & other);

            /*! @brief The assignment operator.
             @param[in] other The object to be copied.
             @return The updated object. */
            ExemplarClient &
            operator =(const ExemplarClient & other);

        public :

        protected :

        private :

        }; // ExemplarClient

    } // Exemplar

} // MplusM

#endif // ! defined(MpMExemplarClient_HPP_)
