//--------------------------------------------------------------------------------------------------
//
//  File:       mpm/M+MBailOutThread.h
//
//  Project:    M+M
//
//  Contains:   The class declaration for a timeout thread for M+M.
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
//  Created:    2014-04-01
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMBailOutThread_H_))
# define MpMBailOutThread_H_ /* Header guard */

# include <mpm/M+MCommon.h>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 
 @brief The class declaration for a timeout thread for M+M. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace Common
    {
        class AdapterChannel;
        class ClientChannel;
        class GeneralChannel;
        class ServiceChannel;
        
        /*! @brief A convenience class to timeout objects. */
        class BailOutThread : public yarp::os::Thread
        {
        public :
            
            /*! @brief The constructor.
             @param timeToWait The number of seconds to delay before triggering. */
            BailOutThread(const double timeToWait);
            
            /*! @brief The constructor.
             @param channelOfInterest The channel that we are waiting for.
             @param timeToWait The number of seconds to delay before triggering. */
            BailOutThread(AdapterChannel & channelOfInterest,
                          const double     timeToWait);
            
            /*! @brief The constructor.
             @param channelOfInterest The channel that we are waiting for.
             @param timeToWait The number of seconds to delay before triggering. */
            BailOutThread(ClientChannel & channelOfInterest,
                          const double    timeToWait);
            
            /*! @brief The constructor.
             @param channelOfInterest The channel that we are waiting for.
             @param timeToWait The number of seconds to delay before triggering. */
            BailOutThread(GeneralChannel & channelOfInterest,
                          const double     timeToWait);
            
            /*! @brief The constructor.
             @param channelOfInterest The channel that we are waiting for.
             @param timeToWait The number of seconds to delay before triggering. */
            BailOutThread(ServiceChannel & channelOfInterest,
                          const double     timeToWait);
            
            /*! @brief The destructor. */
            virtual ~BailOutThread(void);
            
            /*! @brief The thread main body. */
            virtual void run(void);
            
            /*! @brief The thread initialization method.
             @returns @c true if the thread is ready to run. */
            virtual bool threadInit(void);
            
            /*! @brief The thread termination method. */
            virtual void threadRelease(void);
            
        protected :
            
        private :
            
            COPY_AND_ASSIGNMENT_(BailOutThread);
            
        public :
        
        protected :
        
        private :
            
            /*! @brief The class that this class is derived from. */
            typedef yarp::os::Thread inherited;
            
# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunused-private-field"
# endif // defined(__APPLE__)
            /*! @brief Filler to pad to alignment boundary */
            char _filler1[7];
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)
            
            /*! @brief The adapter channel that we are waiting on. */
            AdapterChannel * _adapterChannel;
            
            /*! @brief The client channel that we are waiting on. */
            ClientChannel * _clientChannel;
            
            /*! @brief The general channel that we are waiting on. */
            GeneralChannel * _generalChannel;
            
            /*! @brief The service channel that we are waiting on. */
            ServiceChannel * _serviceChannel;
            
            /*! @brief The time at which the thread will stop running. */
            double _endTime;
            
            /*! @brief The number of seconds to delay before triggering. */
            double _timeToWait;
            
        }; // BailOutThread
        
    } // Common
    
} // MplusM

#endif // ! defined(MpMBailOutThread_H_)
