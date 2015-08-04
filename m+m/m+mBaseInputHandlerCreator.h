//--------------------------------------------------------------------------------------------------
//
//  File:       m+m/m+mBaseInputHandlerCreator.h
//
//  Project:    m+m
//
//  Contains:   The class declaration for the interface between m+m input handler factories and
//              YARP.
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
//  Created:    2014-02-12
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMBaseInputHandlerCreator_H_))
# define MpMBaseInputHandlerCreator_H_ /* Header guard */

# include <m+m/m+mBaseInputHandler.h>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for the interface between m+m input handler factories and YARP. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

/*@ @brief Declare the createHandler method, which returns a new PortReader or @c NULL if one
 cannot be created. */
# define DECLARE_CREATE_ \
    virtual yarp::os::PortReader * create(void)

/*! @brief Define the createHandler method. */
# define DEFINE_CREATE_(class_) \
    yarp::os::PortReader * class_::create(void)

namespace MplusM
{
    namespace Common
    {
        /*! @brief A factory for BaseInputHandler objects. */
        class BaseInputHandlerCreator : public yarp::os::PortReaderCreator
        {
        public :
            
            /*! @brief The constructor. */
            BaseInputHandlerCreator(void);
            
            /*! @brief The destructor. */
            virtual ~BaseInputHandlerCreator(void);
            
            DECLARE_CREATE_ = 0;
            
            /*! @brief Remember the channel that is feeding the input handler.
             @param theChannel The channel that is feeding the input handler. */
            void setChannel(BaseChannel * theChannel);
            
        protected :
            
            /*! @brief The channel that is feeding the input handlers. */
            BaseChannel * _channel;
            
        private :
            
            COPY_AND_ASSIGNMENT_(BaseInputHandlerCreator);
            
        public :
        
        protected :
        
        private :
            
            /*! @brief The class that this class is derived from. */
            typedef yarp::os::PortReaderCreator inherited;
            
        }; // BaseInputHandlerCreator
        
    } // Common
    
} // MplusM

#endif // ! defined(MpMBaseInputHandlerCreator_H_)
