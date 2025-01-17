//--------------------------------------------------------------------------------------------------
//
//  File:       m+m/m+mBaseThread.hpp
//
//  Project:    m+m
//
//  Contains:   The class declaration for the common thread class for m+m.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2016 by H Plus Technologies Ltd. and Simon Fraser University.
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
//  Created:    2016-01-17
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMBaseThread_HPP_))
# define MpMBaseThread_HPP_ /* Header guard */

# include <m+m/m+mCommon.hpp>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for the common thread class for m+m. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace Common
    {
        /*! @brief A convenience class for threads. */
        class BaseThread : public yarp::os::Thread
        {
        public :

        protected :

        private :

            /*! @brief The class that this class is derived from. */
            typedef yarp::os::Thread inherited;

        public :

            /*! @brief The constructor. */
            BaseThread(void);

            /*! @brief The destructor. */
            virtual
            ~BaseThread(void);

        protected :

        private :

            /*! @brief The copy constructor.
             @param[in] other The object to be copied. */
            BaseThread(const BaseThread & other);

            /*! @brief The assignment operator.
             @param[in] other The object to be copied.
             @return The updated object. */
            BaseThread &
            operator =(const BaseThread & other);

            /*! @brief The thread main body. */
            virtual inline void
            run(void)
            {
            } // run

            /*! @brief The thread initialization method.
             @return @c true if the thread is ready to run. */
            virtual inline bool
            threadInit(void)
            {
                return true;
            } // threadInit

            /*! @brief The thread termination method. */
            virtual inline void
            threadRelease(void)
            {
            } // threadRelease

        public :

        protected :

        private :

        }; // BaseThread

    } // Common

} // MplusM

#endif // ! defined(MpMBaseThread_HPP_)
