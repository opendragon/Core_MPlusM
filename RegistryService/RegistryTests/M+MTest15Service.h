//--------------------------------------------------------------------------------------
//
//  File:       M+MTest15Service.h
//
//  Project:    M+M
//
//  Contains:   The class declaration for a simple service used by the unit tests.
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
//  Created:    2014-03-05
//
//--------------------------------------------------------------------------------------

#if (! defined(MpMTest15Service_H_))
/*! @brief Header guard. */
# define MpMTest15Service_H_ /* */

# include "M+MBaseService.h"

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 
 @brief The class declaration for a simple service used by the unit tests. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace Test
    {
        class Test15EchoRequestHandler;
        
        /*! @brief A test service. */
        class Test15Service : public Common::BaseService
        {
        public:
            
            /*! @brief The constructor.
             @param argc The number of arguments in 'argv'.
             @param argv The arguments to be used to specify the new service. */
            Test15Service(const int argc,
                          char * *  argv);
            
            /*! @brief The destructor. */
            virtual ~Test15Service(void);
            
            /*! @brief Start processing requests.
             @returns @c true if the service was started and @c false if it was not. */
            virtual bool start(void);
            
            /*! @brief Stop processing requests.
             @returns @c true if the service was stopped and @c false it if was not. */
            virtual bool stop(void);
            
        protected:
            
        private:
            
            /*! @brief The class that this class is derived from. */
            typedef BaseService inherited;
            
            /*! @brief Enable the standard request handlers. */
            void attachRequestHandlers(void);
            
            /*! @brief Disable the standard request handlers. */
            void detachRequestHandlers(void);
            
            /*! @brief The request handler for the 'echo' request. */
            Test15EchoRequestHandler * _echoHandler;
            
        }; // Test15Service
        
    } // Test
    
} // MplusM

#endif // ! defined(MpMTest15Service_H_)
