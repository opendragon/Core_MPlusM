//--------------------------------------------------------------------------------------------------
//
//  File:       mpm/M+MBaseServiceInputHandlerCreator.h
//
//  Project:    M+M
//
//  Contains:   The class declaration for the minimal functionality required for an M+M input
//              handler factory object.
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
//  Created:    2014-02-21
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMBaseServiceInputHandlerCreator_H_))
# define MpMBaseServiceInputHandlerCreator_H_ /* Header guard */

# include <mpm/M+MBaseInputHandlerCreator.h>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 
 @brief The class declaration for the minimal functionality required for an M+M input handler
 factory object. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace Common
    {
        class BaseService;
        
        /*! @brief The minimal functionality required for an M+M service. */
        class BaseServiceInputHandlerCreator : public BaseInputHandlerCreator
        {
        public :
            
            /*! @brief The constructor. */
            BaseServiceInputHandlerCreator(BaseService & service);
            
            /*! @brief The destructor. */
            virtual ~BaseServiceInputHandlerCreator(void);
            
            /*! @brief Create a new BaseInputHandler object to process input data.
             @returns A new BaseInputHandler or @c NULL if one cannot be created. */
            virtual BaseInputHandler * create(void);
            
        protected :
            
        private :
            
            COPY_AND_ASSIGNMENT_(BaseServiceInputHandlerCreator);
            
        public :
        
        protected :
        
        private :
            
            /*! @brief The class that this class is derived from. */
            typedef BaseInputHandlerCreator inherited;
            
            /*! @brief The service that 'owns' this handler. */
            BaseService & _service;
            
        }; // BaseServiceInputHandlerCreator
        
    } // Common
    
} // MplusM

#endif // ! defined(MpMBaseServiceInputHandlerCreator_H_)
