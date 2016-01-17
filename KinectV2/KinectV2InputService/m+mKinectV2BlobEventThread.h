//--------------------------------------------------------------------------------------------------
//
//  File:       m+mKinectV2BlobEventThread.h
//
//  Project:    m+m
//
//  Contains:   The class declaration for a thread that generates output from Kinect V2 data.
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
//  Created:    2015-07-26
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMKinectV2BlobEventThread_H_))
# define MpMKinectV2BlobEventThread_H_ /* Header guard */

# include "stdafx.h"

# include <m+m/m+mBaseThread.h>
# include <m+m/m+mGeneralChannel.h>
# include <m+m/m+mStringBuffer.h>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for a thread that generates output from Kinect V2 data. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace KinectV2Blob
    {
        /*! @brief A class to generate output from Kinect V2 data. */
        class KinectV2BlobEventThread : public Common::BaseThread
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
            KinectV2BlobEventThread(Common::GeneralChannel * outChannel);
            
            /*! @brief The destructor. */
            virtual
            ~KinectV2BlobEventThread(void);
            
            /*! @brief Stop using the output channel. */
            void
            clearOutputChannel(void);

            /*! @brief Set the translation scale.
             @param newScale The scale factor for translation values. */
            void
            setScale(const double newScale);

            /*! @brief Return the translation scale.
             @returns The translation scale. */
            inline double
            translationScale(void)
            const
            {
                return _translationScale;
            } // translationScale
            
        protected :
            
        private :
            
            /*! @brief Initialize the default Kinect V2 sensor.
             @returns @c S_OK on success, a failure code otherwise. */
            HRESULT
            initializeDefaultSensor(void);
            
            /*! @brief Handle the sensor data associated with the event. */
            void
            processEventData(void);
            
            DECLARE_RUN_;
            
            DECLARE_THREADINIT_;
            
            DECLARE_THREADRELEASE_;
            
            COPY_AND_ASSIGNMENT_(KinectV2BlobEventThread);

        public :

        protected :

        private :

            /*! @brief The translation scale to be used. */
            double _translationScale;
            
# if defined(MpM_UseCustomStringBuffer)
            /*! @brief The buffer to hold the output data. */
            Common::StringBuffer _outBuffer;
# endif // defined(MpM_UseCustomStringBuffer)

            /*! @brief The %Bottle to use send the output data. */
            yarp::os::Bottle _messageBottle;

            /* @brief The event from the device that we are waiting for. */
            WAITABLE_HANDLE _frameEventHandle;

            /*! @brief The current Kinect V2 sensor. */
            IKinectSensor * _kinectSensor;

            /*! @brief The body frame reader. */
            IBodyFrameReader * _bodyFrameReader;

            /*! @brief The body frame source. */
            IBodyFrameSource * _bodyFrameSource;

            /*! @brief The channel to send data bursts to. */
            Common::GeneralChannel * _outChannel;

        }; // KinectV2BlobEventThread
        
    } // KinectV2Blob
    
} // MplusM

#endif // ! defined(MpMKinectV2BlobEventThread_H_)
