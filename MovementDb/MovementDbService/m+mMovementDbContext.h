//--------------------------------------------------------------------------------------------------
//
//  File:       m+mMovementDbContext.h
//
//  Project:    m+m
//
//  Contains:   The class declaration for a context used with the movement database service.
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
//  Created:    2014-09-04
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMMovementDbContext_H_))
# define MpMMovementDbContext_H_ /* Header guard */

# include <m+m/m+mBaseContext.hpp>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for a context used with the movement database service. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace MovementDb
    {
        /*! @brief A convenience class to provide context objects for the movement database
         service. */
        class MovementDbContext : public Common::BaseContext
        {
        public :

        protected :

        private :

            /*! @brief The class that this class is derived from. */
            typedef BaseContext inherited;

        public :

            /*! @brief The constructor. */
            MovementDbContext(void);

            /*! @brief The destructor. */
            virtual
            ~MovementDbContext(void);

            /*! @brief An accessor for the data track. */
            inline YarpString &
            dataTrack(void)
            {
                return _dataTrack;
            } // dataTrack

            /*! @brief An accessor for the e-mail address. */
            inline YarpString &
            emailAddress(void)
            {
                return _emailAddress;
            } // emailAddress

        protected :

        private :

            /*! @brief The copy constructor.
             @param other The object to be copied. */
            MovementDbContext(const MovementDbContext & other);

            /*! @brief The assignment operator.
             @param other The object to be copied.
             @returns The updated object. */
            MovementDbContext &
            operator =(const MovementDbContext & other);

        public :

        protected :

        private :

            /*! @brief The data track to use. */
            YarpString _dataTrack;

            /*! @brief The e-mail address to use. */
            YarpString _emailAddress;

        }; // MovementDbContext

    } // MovementDb

} // MplusM

#endif // ! defined(MpMMovementDbContext_H_)
