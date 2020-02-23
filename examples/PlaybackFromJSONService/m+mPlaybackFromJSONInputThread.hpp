//--------------------------------------------------------------------------------------------------
//
//  File:       m+mPlaybackFromJSONInputThread.hpp
//
//  Project:    m+m
//
//  Contains:   The class declaration for an output-generating thread for m+m.
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
//  Created:    2015-08-24
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMPlaybackFromJSONInputThread_HPP_))
# define MpMPlaybackFromJSONInputThread_HPP_ /* Header guard */

# include <m+m/m+mBaseThread.hpp>
# include <m+m/m+mGeneralChannel.hpp>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for an output-generating thread for m+m. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace Example
    {
        /*! @brief A convenience class to generate output. */
        class PlaybackFromJSONInputThread : public Common::BaseThread
        {
        public :

        protected :

        private :

            /*! @brief The class that this class is derived from. */
            typedef BaseThread inherited;

        public :

            /*! @brief The constructor.
             @param[in] outChannel The channel to send the data to.
             @param[in] outMessage The data to be used.
             @param[in] playbackRatio The speed at which to send data.
             @param[in] initialDelay The number of seconds to delay before the first message send.
             @param[in] loopPlayback @c true if the data is to be repeated indefinitely and @c false
             otherwise. */
            PlaybackFromJSONInputThread(Common::GeneralChannel * outChannel,
                                        yarp::os::Bottle &       outMessage,
                                        const double             playbackRatio,
                                        const double             initialDelay,
                                        const bool               loopPlayback);

            /*! @brief The destructor. */
            virtual
            ~PlaybackFromJSONInputThread(void);

            /*! @brief Stop using the output channel. */
            void
            clearOutputChannel(void);

        protected :

        private :

            /*! @brief The copy constructor.
             @param[in] other The object to be copied. */
            PlaybackFromJSONInputThread(const PlaybackFromJSONInputThread & other);

            /*! @brief The assignment operator.
             @param[in] other The object to be copied.
             @return The updated object. */
            PlaybackFromJSONInputThread &
            operator =(const PlaybackFromJSONInputThread & other);

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

            /*! @brief The data to be used. */
            yarp::os::Bottle & _outMessage;

            /*! @brief The channel to send data bursts to. */
            Common::GeneralChannel * _outChannel;

            /*! @brief The initial delay. */
            double _initialDelay;

            /*! @brief The time at which the thread will send data. */
            double _nextTime;

            /*! @brief The speed at which to send data. */
            double _playbackRatio;

            /*! @brief The index of the next value to be sent. */
            int _nextIndex;

            /*! @brief @c true if the output should repeat when the end of the input is reached and
             @c false otherwise. */
            bool _loopPlayback;

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunused-private-field"
# endif // defined(__APPLE__)
            /*! @brief Filler to pad to alignment boundary */
            char _filler1[7];
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

        }; // PlaybackFromJSONInputThread

    } // Example

} // MplusM

#endif // ! defined(MpMPlaybackFromJSONInputThread_HPP_)
