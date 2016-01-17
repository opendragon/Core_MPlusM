//--------------------------------------------------------------------------------------------------
//
//  File:       m+m/m+mBailOut.h
//
//  Project:    m+m
//
//  Contains:   The class declaration for a timeout mechanism for m+m.
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
//  Created:    2014-04-01
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMBailOut_H_))
# define MpMBailOut_H_ /* Header guard */

# include <m+m/m+mCommon.h>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for a timeout mechanism for m+m. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace Common
    {
        class BailOutThread;
        class BaseChannel;
        
        /*! @brief A convenience class to timeout objects. */
        class BailOut
        {
        public :
        
        protected :
        
        private :
            
        public :
            
            /*! @brief The constructor.
             @param timeToWait The number of seconds to delay before triggering. */
            explicit
            BailOut(const double timeToWait);
            
            /*! @brief The constructor.
             @param channelOfInterest The channel that we are waiting for.
             @param timeToWait The number of seconds to delay before triggering. */
            BailOut(BaseChannel & channelOfInterest,
                    const double  timeToWait);
            
            /*! @brief The destructor. */
            virtual
            ~BailOut(void);
            
        protected :
            
        private :
            
            COPY_AND_ASSIGNMENT_(BailOut);
            
        public :
        
        protected :
        
        private :
            
            /*! @brief The bailout thread to use. */
            BailOutThread * _bailer;
            
            /*! @brief The amount of time to wait for the thread to stop. */
            double _stopTime;
            
        }; // BailOut
        
    } // Common
    
} // MplusM

#endif // ! defined(MpMBailOut_H_)
