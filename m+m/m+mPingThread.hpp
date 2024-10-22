//--------------------------------------------------------------------------------------------------
//
//  File:       m+m/m+mPingThread.hpp
//
//  Project:    m+m
//
//  Contains:   The class declaration for a ping thread for m+m.
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
//  Created:    2014-06-18
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMPingThread_HPP_))
# define MpMPingThread_HPP_ /* Header guard */

# include <m+m/m+mBaseThread.hpp>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for a ping thread for m+m. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace Common
    {
        class BaseService;

        /*! @brief A convenience class to generate pings. */
        class PingThread : public Common::BaseThread
        {
        public :

        protected :

        private :

            /*! @brief The class that this class is derived from. */
            typedef BaseThread inherited;

        public :

            /*! @brief The constructor.
             @param[in] channelName The channel that we are acting on the behalf of.
             @param[in] service The service associated with the request. */
            PingThread(const YarpString & channelName,
                       BaseService &      service);

            /*! @brief The destructor. */
            virtual
            ~PingThread(void);

        protected :

        private :

            /*! @brief The copy constructor.
             @param[in] other The object to be copied. */
            PingThread(const PingThread & other);

            /*! @brief The assignment operator.
             @param[in] other The object to be copied.
             @return The updated object. */
            PingThread &
            operator =(const PingThread & other);

            /*! @brief The thread main body. */
            virtual void
            run(void);

            /*! @brief The thread initialization method.
             @return @c true if the thread is ready to run. */
            virtual bool
            threadInit(void);

        public :

        protected :

        private :

            /*! @brief The name of the service channel to report. */
            YarpString _channelName;

            /*! @brief The service that is associated with the request. */
            BaseService & _service;

            /*! @brief The time at which the thread will next 'ping'. */
            double _pingTime;

        }; // PingThread

    } // Common

} // MplusM

#endif // ! defined(MpMPingThread_HPP_)
