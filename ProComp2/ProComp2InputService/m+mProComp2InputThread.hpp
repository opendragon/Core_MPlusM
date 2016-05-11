//--------------------------------------------------------------------------------------------------
//
//  File:       m+mProComp2InputThread.hpp
//
//  Project:    m+m
//
//  Contains:   The class declaration for a thread that generates output from ProComp2 data.
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
//  Created:    2015-04-13
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMProComp2InputThread_HPP_))
# define MpMProComp2InputThread_HPP_ /* Header guard */

# include <m+m/m+mBaseThread.hpp>
# include <m+m/m+mGeneralChannel.hpp>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for a thread that generates output from %ProComp2 data. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace ProComp2
    {
        /*! @brief A class to generate output from %ProComp2 data. */
        class ProComp2InputThread : public Common::BaseThread
        {
        public :

        protected :

        private :

            /*! @brief The class that this class is derived from. */
            typedef BaseThread inherited;

        public :

            /*! @brief The constructor.
             @param outChannel The channel to send data bursts to. */
            explicit
            ProComp2InputThread(Common::GeneralChannel * outChannel);

            /*! @brief The destructor. */
            virtual
            ~ProComp2InputThread(void);

            /*! @brief Stop using the output channel. */
            void
            clearOutputChannel(void);

        protected :

        private :

            /*! @brief The copy constructor.
             @param other The object to be copied. */
            ProComp2InputThread(const ProComp2InputThread & other);

            /*! @brief The assignment operator.
             @param other The object to be copied.
             @returns The updated object. */
            ProComp2InputThread &
            operator =(const ProComp2InputThread & other);

# if (! defined(MpM_BuildDummyServices))
            /*! @brief Extract the data for all channels and send it.
             @param time The time at which the channels are processed. */
            void
            readChannelData(const DWORD time);
# endif // ! defined(MpM_BuildDummyServices)

            /*! @brief The thread main body. */
            virtual void
            run(void);

            /*! @brief Prepare any attached encoders for use.
             @returns @c true if at least one encoder was set up. */
            bool
            setupEncoders(void);

            /*! @brief The thread initialization method.
             @returns @c true if the thread is ready to run. */
            virtual bool
            threadInit(void);

            /*! @brief The thread termination method. */
            virtual void
            threadRelease(void);

        public :

        protected :

        private :

            /*! @brief The channel to send data bursts to. */
            Common::GeneralChannel * _outChannel;

        }; // ProComp2InputThread

    } // ProComp2

} // MplusM

#endif // ! defined(MpMProComp2InputThread_HPP_)
