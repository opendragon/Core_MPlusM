//--------------------------------------------------------------------------------------------------
//
//  File:       M+MKinectV2EventHandler.h
//
//  Project:    M+M
//
//  Contains:   The class declaration for an output-generating event handler for M+M.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2014 by HPlus Technologies Ltd. and Simon Fraser University.
//
//              All rights reserved. Redistribution and use in source and binary forms, with or
//              without modification, are permitted provided that the following conditions are met:
//                * Redistributions of source code must retain the above copyright notice, this list
//                  of conditions and the following disclaimer.
//                * Redistributions in binary form must reproduce the above copyright notice, this
//                  list of conditions and the following disclaimer in the documentation and/or
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
//  Created:    2014-09-23
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMKinectV2EventHandler_H_))
# define MpMKinectV2EventHandler_H_ /* Header guard */

# include <mpm/M+MGeneralChannel.h>

# include "stdafx.h"

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 
 @brief The class declaration for an output-generating event handler for M+M. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace KinectV2
    {
        /*! @brief A convenience class to generate output. */
        class KinectV2EventHandler
        {
        public :
            
            /*! @brief The constructor.
             @param outChannel The channel to send data bursts to. */
            KinectV2EventHandler(Common::GeneralChannel * outChannel);
            
            /*! @brief The destructor. */
            virtual ~KinectV2EventHandler(void);
            
            /*! @brief Stop using the output channel. */
            void clearOutputChannel(void);
            
#if 0
            /*! @brief The thread main body. */
            virtual void run(void);
            
            /*! @brief The thread initialization method.
             @returns @c true if the thread is ready to run. */
            virtual bool threadInit(void);
            
            /*! @brief The thread termination method. */
            virtual void threadRelease(void);
#endif//0

        protected :
            
        private :
            
            COPY_AND_ASSIGNMENT_(KinectV2EventHandler);

            /*! @brief Initialize the default Kinect V2 sensor
            @returns @c S_OK on success, a failure code otherwise. */
            HRESULT initializeDefaultSensor(void);

#if 0
            /*! @brief Collect the sensor data and add it to a message.
            @param message The message to be updated.
            @returns @c true if the message has data added and @c false otherwise. */
            bool updateData(yarp::os::Bottle & message);
#endif//0
        public :

        protected :

        private :

            WAITABLE_HANDLE _frameEventHandle;

            /*! @brief The current Kinect V2 sensor. */
            IKinectSensor * _kinectSensor;

            /*! @brief The body frame reader. */
            IBodyFrameReader * _bodyFrameReader;

            /*! @brief The body frame source. */
            IBodyFrameSource * _bodyFrameSource;

            /*! @brief The channel to send data bursts to. */
            Common::GeneralChannel * _outChannel;
            
        }; // KinectV2EventHandler
        
    } // KinectV2
    
} // MplusM

#endif // ! defined(MpMKinectV2EventHandler_H_)
