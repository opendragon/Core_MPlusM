//--------------------------------------------------------------------------------------------------
//
//  File:       m+m/m+mAdapterServiceThread.hpp
//
//  Project:    m+m
//
//  Contains:   The class declaration for an adapter service thread for m+m.
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
//  Created:    2016-05-09
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMAdapterServiceThread_HPP_))
# define MpMAdapterServiceThread_HPP_ /* Header guard */

# include <m+m/m+mBaseThread.hpp>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for an adapter service thread for m+m. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace Common
    {
        class BaseAdapterData;
        class BaseAdapterService;

        /*! @brief A convenience class to run an adapter service in a thread. */
        class AdapterServiceThread : public Common::BaseThread
        {
        public :

        protected :

        private :

            /*! @brief The class that this class is derived from. */
            typedef BaseThread inherited;

        public :

            /*! @brief The constructor.
             @param service The service associated with the request.
             @param sharedData The shared data for the input handlers.
             @param helpText The help text to be displayed.
             @param goWasSet @c true if the service is to be started immediately.
             @param stdinAvailable @c true if running in the foreground and @c false otherwise.
             @param reportOnExit @c true if service metrics are to be reported on exit and @c false
             otherwise. */
            AdapterServiceThread(BaseAdapterService * service,
                                 BaseAdapterData &    sharedData,
                                 const YarpString &   helpText,
                                 const bool           goWasSet,
                                 const bool           stdinAvailable,
                                 const bool           reportOnExit);

            /*! @brief The destructor. */
            virtual
            ~AdapterServiceThread(void);

        protected :

        private :

            /*! @brief The copy constructor.
             @param other The object to be copied. */
            AdapterServiceThread(const AdapterServiceThread & other);

            /*! @brief The assignment operator.
             @param other The object to be copied.
             @returns The updated object. */
            AdapterServiceThread &
            operator =(const AdapterServiceThread & other);

            /*! @brief The thread main body. */
            virtual void
            run(void);

            /*! @brief The thread initialization method.
             @returns @c true if the thread is ready to run. */
            virtual bool
            threadInit(void);

        public :

        protected :

        private :

            /*! @brief The service that is associated with the thread. */
            BaseAdapterService * _service;

            /*! @brief The shared data for the input handlers of the service. */
            BaseAdapterData & _sharedData;

            /*! @brief The help text for the service. */
            YarpString _helpText;

            /*! @brief @c true if the service is to be started immediately. */
            bool _goWasSet;

            /*! @brief @c true if the service metrices are to be reported on exit. */
            bool _reportOnExit;

            /*! @brief @c true if running in the foreground and @c false otherwise. */
            bool _stdinAvailable;

        }; // AdapterServiceThread

    } // Common

} // MplusM

#endif // ! defined(MpMAdapterServiceThread_HPP_)
