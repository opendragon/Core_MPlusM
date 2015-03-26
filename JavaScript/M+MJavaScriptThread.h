//--------------------------------------------------------------------------------------------------
//
//  File:       M+MJavaScriptThread.h
//
//  Project:    M+M
//
//  Contains:   The class declaration for an output-generating thread for M+M.
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
//  Created:    2015-01-15
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMJavaScriptThread_H_))
# define MpMJavaScriptThread_H_ /* Header guard */

# include <mpm/M+MGeneralChannel.h>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Winvalid-offsetof"
# endif // defined(__APPLE__)
# include <js/RequiredDefines.h>
# include <jsapi.h>
# include <js/CallArgs.h>
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for an output-generating thread for M+M. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

struct JSContext;

namespace MplusM
{
    namespace JavaScript
    {
        /*! @brief A convenience class to generate output. */
        class JavaScriptThread : public yarp::os::Thread
        {
        public :
            
            /*! @brief The constructor.
             @param timeToWait The number of seconds to delay before triggering.
             @param context The %JavaScript engine context.
             @param global The %JavaScript global object.
             @param threadFunc The %JavaScript handler function for the thread. */
            JavaScriptThread(const double            timeToWait,
                             JSContext *             context,
                             JS::RootedObject &      global,
                             const JS::RootedValue & threadFunc);
            
            /*! @brief The destructor. */
            virtual ~JavaScriptThread(void);
            
            /*! @brief Stop using the output channel. */
            void clearOutputChannel(void);
            
            /*! @brief The thread main body. */
            virtual void run(void);
            
            /*! @brief The thread initialization method.
             @returns @c true if the thread is ready to run. */
            virtual bool threadInit(void);
            
            /*! @brief The thread termination method. */
            virtual void threadRelease(void);
            
        protected :
            
        private :
            
            COPY_AND_ASSIGNMENT_(JavaScriptThread);
            
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
            
            /*! @brief The %JavaScript thread function. */
            JS::RootedValue _threadFunc;
            
            /*! @brief The %JavaScript global object for this execution environment. */
            JS::RootedObject & _global;
            
            /*! @brief The %JavaScript execution environment. */
            JSContext * _context;
            
            /*! @brief The time at which the thread will send data. */
            double _nextTime;
            
            /*! @brief The number of seconds to delay before triggering. */
            double _timeToWait;
            
        }; // JavaScriptThread
        
    } // JavaScript
    
} // MplusM

#endif // ! defined(MpMJavaScriptThread_H_)
