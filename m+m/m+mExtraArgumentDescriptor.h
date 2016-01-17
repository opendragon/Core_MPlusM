//--------------------------------------------------------------------------------------------------
//
//  File:       m+m/m+mExtraArgumentDescriptor.h
//
//  Project:    m+m
//
//  Contains:   The class declaration for the minimal functionality required to represent a
//              placeholder for trailing command-line arguments.
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
//  Created:    2015-06-08
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMExtraArgumentDescriptor_H_))
# define MpMExtraArgumentDescriptor_H_ /* Header guard */

# include <m+m/m+mBaseArgumentDescriptor.h>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for the minimal functionality required to represent a placeholder for
 trailing command-line arguments. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace Utilities
    {
        /*! @brief A trailing arguments description.
         
         The external representation of a trailing arguments description is:
         
         extraTagAndInfo ::= 'E';
         
         Note that the concept of 'optional' versus 'mandatory' is not applicable to trailing
         arguments, as it's possible to have zero or more of them.
         Likewise, there is no 'default' value that makes sense or, for that matter, a single
         argument reference is of no practical use. */
        class ExtraArgumentDescriptor : public BaseArgumentDescriptor
        {
        public :
        
        protected :
        
        private :
            
            /*! @brief The class that this class is derived from. */
            typedef BaseArgumentDescriptor inherited;
            
        public :
            
            /*! @brief The constructor.
             @param argName The name of the command-line argument.
             @param argDescription A description of the command-line argument. */
            ExtraArgumentDescriptor(const YarpString & argName,
                                    const YarpString & argDescription);
            
            /*! @brief The destructor. */
            virtual
            ~ExtraArgumentDescriptor(void);
            
            /*! @brief Construct a descriptor, if at all possible, from the input string.
             @param inString The input string in 'arguments' format.
             @returns A valid descriptor or @c NULL if the input is not recognized. */
            static BaseArgumentDescriptor *
            parseArgString(const YarpString & inString);

        protected :
        
        private :
            
            DECLARE_ADDVALUETOBOTTLE_;
            
            DECLARE_CLONE_;

            DECLARE_GETDEFAULTVALUE_;
            
            DECLARE_GETPROCESSEDVALUE_;
            
            DECLARE_ISEXTRA_
            {
                return true;
            } // isExtra
            
            DECLARE_SETTODEFAULTVALUE_;
            
            DECLARE_TOSTRING_;
            
            DECLARE_VALIDATE_;
            
            COPY_AND_ASSIGNMENT_(ExtraArgumentDescriptor);
            
        public :
        
        protected :
        
        private :
            
        }; // ExtraArgumentDescriptor

    } // Utilities
    
} // MplusM

#endif // ! defined(MpMExtraArgumentDescriptor_H_)
