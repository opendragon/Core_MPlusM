//--------------------------------------------------------------------------------------------------
//
//  File:       m+mMovementDbClient.hpp
//
//  Project:    m+m
//
//  Contains:   The class declaration for the client of the movement database service.
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
//  Created:    2014-09-02
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMMovementDbClient_HPP_))
# define MpMMovementDbClient_HPP_ /* Header guard */

# include <m+m/m+mBaseClient.hpp>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for the client of the movement database service. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace MovementDb
    {
        /*! @brief A client for the movement database service. */
        class MovementDbClient : public Common::BaseClient
        {
        public :

        protected :

        private :

            /*! @brief The class that this class is derived from. */
            typedef BaseClient inherited;

        public :

            /*! @brief The constructor. */
            MovementDbClient(void);

            /*! @brief The destructor. */
            virtual
            ~MovementDbClient(void);

            /*! @brief Add a file entry to the backend database.
             @param[in] filePath The filesystem path to the file.
             @returns @c true if the file entry was added successfully and @c false otherwise. */
            bool
            addFileToDb(const YarpString & filePath);

            /*! @brief Set the active data track.
             @param[in] dataTrack The data track to use with subsequent files.
             @returns @c true if the data track was successfully set and @c false otherwise. */
            bool
            setDataTrackForDb(const YarpString & dataTrack);

            /*! @brief Set the active e-mail address.
             @param[in] emailAddress The e-mail address of the user that will own subsequent files.
             @returns @c true if the e-mail address was successfully set and @c false otherwise. */
            bool
            setEmailAddressForDb(const YarpString & emailAddress);

            /*! @brief Stop the database connection for this client.
             @returns @c true if the service handled the request and @c false otherwise. */
            bool
            stopDbConnection(void);

        protected :

        private :

            /*! @brief The copy constructor.
             @param[in] other The object to be copied. */
            MovementDbClient(const MovementDbClient & other);

            /*! @brief The assignment operator.
             @param[in] other The object to be copied.
             @returns The updated object. */
            MovementDbClient &
            operator =(const MovementDbClient & other);

        public :

        protected :

        private :

        }; // MovementDbClient

    } // MovementDb

} // MplusM

#endif // ! defined(MpMMovementDbClient_HPP_)
