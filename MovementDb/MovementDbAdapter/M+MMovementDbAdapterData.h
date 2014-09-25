//--------------------------------------------------------------------------------------------------
//
//  File:       M+MMovementDbAdapterData.h
//
//  Project:    M+M
//
//  Contains:   The class declaration for the data shared between the input handlers and main thread
//              of the movement database adapter.
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
//  Created:    2014-09-04
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMMovementDbAdapterData_H_))
# define MpMMovementDbAdapterData_H_ /* Header guard */

# include <mpm/M+MBaseAdapterData.h>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 
 @brief The class declaration for the data shared between the input handlers and main thread of the
 movement database adapter. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace Common
    {
        class AdapterChannel;
    } // Common
    
    namespace MovementDb
    {
        class MovementDbClient;
        
        /*! @brief A handler for partially-structured input data. */
        class MovementDbAdapterData : public Common::BaseAdapterData
        {
        public :
            
            /*! @brief The constructor.
             @param client The client connection that is used to communicate with the service.
             @param output The output channel that will receive the service responses. */
            MovementDbAdapterData(MovementDbClient *       client,
                                  Common::AdapterChannel * output);
            
            /*! @brief The destructor. */
            virtual ~MovementDbAdapterData(void);
            
        protected :
            
        private :
            
            COPY_AND_ASSIGNMENT_(MovementDbAdapterData);
            
        public :
        
        protected :
        
        private :
            
            /*! @brief The class that this class is derived from. */
            typedef BaseAdapterData inherited;
            
        }; // MovementDbAdapterData
        
    } // MovementDb
    
} // MplusM

#endif // ! defined(MpMMovementDbAdapterData_H_)
