//--------------------------------------------------------------------------------------
//
//  File:       MoMeBailOut.h
//
//  Project:    MoAndMe
//
//  Contains:   The class declaration for a timeout mechanism for MoAndMe.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2014 by HPlus Technologies Ltd. and Simon Fraser University.
//
//              All rights reserved. Redistribution and use in source and binary forms,
//              with or without modification, are permitted provided that the following
//              conditions are met:
//                * Redistributions of source code must retain the above copyright
//                  notice, this list of conditions and the following disclaimer.
//                * Redistributions in binary form must reproduce the above copyright
//                  notice, this list of conditions and the following disclaimer in the
//                  documentation and/or other materials provided with the
//                  distribution.
//                * Neither the name of the copyright holders nor the names of its
//                  contributors may be used to endorse or promote products derived
//                  from this software without specific prior written permission.
//
//              THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
//              "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
//              LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
//              PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
//              OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
//              SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
//              LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
//              DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
//              THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
//              (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
//              OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
//  Created:    2014-04-01
//
//--------------------------------------------------------------------------------------

#if (! defined(MOMEBAILOUT_H_))
/*! @brief Header guard. */
# define MOMEBAILOUT_H_ /* */

# include "MoMeCommon.h"

# if (defined(__APPLE__) || defined(__linux__))
#  include <csignal>
# endif // defined(__APPLE__) || defined(__linux__)

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 
 @brief The class declaration for a timeout mechanism for MoAndMe. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MoAndMe
{
    namespace Common
    {
        class BailOutThread;
        
        /*! @brief A convenience class to timeout objects. */
        class BailOut
        {
        public:
            
            /*! @brief The constructor.
             @param channelOfInterest The channel that we are waiting for.
             @param timeToWait The number of seconds to delay before triggering. */
            BailOut(Channel *    channelOfInterest = NULL,
                    const double timeToWait = STANDARD_WAIT_TIME);
            
            /*! @brief The destructor. */
            virtual ~BailOut(void);
            
        protected:
            
        private:
            
            /*! @brief Copy constructor.
             
             Note - not implemented and private, to prevent unexpected copying.
             @param other Another object to construct from. */
            BailOut(const BailOut & other);
            
            /*! @brief Assignment operator.
             
             Note - not implemented and private, to prevent unexpected copying.
             @param other Another object to construct from. */
            BailOut & operator=(const BailOut & other);
            
            /*! @brief The bailout thread to use. */
            BailOutThread * _bailer;
            
        }; // BailOut
        
    } // Common
    
} // MoAndMe

#endif // ! defined(MOMEBAILOUT_H_)
