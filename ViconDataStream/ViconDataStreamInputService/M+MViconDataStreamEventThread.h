//--------------------------------------------------------------------------------------------------
//
//  File:       M+MViconDataStreamEventThread.h
//
//  Project:    M+M
//
//  Contains:   The class declaration for a thread that generates output from Vicon data.
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
//  Created:    2014-11-07
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMViconDataStreamEventThread_H_))
# define MpMViconDataStreamEventThread_H_ /* Header guard */

# include "stdafx.h"

# include <mpm/M+MGeneralChannel.h>

# include <Client.h>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for a thread that generates output from Vicon data. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace ViconDataStream
    {
        /*! @brief A class to generate output from Vicon data. */
        class ViconDataStreamEventThread : public yarp::os::Thread
        {
        public :
            
            /*! @brief The constructor.
             @param outChannel The channel to send data bursts to.
			 @param nameAndPort The host name and port to connect to the Vicon server. */
            ViconDataStreamEventThread(Common::GeneralChannel * outChannel,
				                       const YarpString &       nameAndPort);
            
            /*! @brief The destructor. */
            virtual ~ViconDataStreamEventThread(void);
            
            /*! @brief Stop using the output channel. */
            void clearOutputChannel(void);

        protected :
            
        private :
            
            /*! @brief Initialize the connection to the Vicon device.
             @returns @c true on success and @c false otherwise. */
            bool initializeConnection(void);
            
            /*! @brief Handle the sensor data associated with the current frame.
             @param subjectCount The number of subjects in the data. */
            void processEventData(const unsigned int subjectCount);
            
            /*! @brief The thread main body. */
            virtual void run(void);
            
            /*! @brief The thread initialization method.
             @returns @c true if the thread is ready to run. */
            virtual bool threadInit(void);
            
            /*! @brief The thread termination method. */
            virtual void threadRelease(void);
            
            COPY_AND_ASSIGNMENT_(ViconDataStreamEventThread);

        public :

        protected :

        private :

            /*! @brief The class that this class is derived from. */
            typedef yarp::os::Thread inherited;

			/* @brief The connection to the Vicon device. */
			ViconDataStreamSDK::CPP::Client _viconClient;

			/* @brief The host name and port to connect to the Vicon server. */
			YarpString _nameAndPort;

			/*! @brief The channel to send data bursts to. */
            Common::GeneralChannel * _outChannel;

        }; // ViconDataStreamEventThread
        
    } // ViconDataStream
    
} // MplusM

#endif // ! defined(MpMViconDataStreamEventThread_H_)
