//--------------------------------------------------------------------------------------------------
//
//  File:       m+m/m+mChannelArgumentDescriptor.h
//
//  Project:    m+m
//
//  Contains:   The class declaration for the minimal functionality required to represent a
//              channel-type command-line argument.
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
//  Created:    2015-05-15
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMChannelArgumentDescriptor_H_))
# define MpMChannelArgumentDescriptor_H_ /* Header guard */

# include <m+m/m+mStringArgumentDescriptor.h>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for the minimal functionality required to represent a channel-type
 command-line argument. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace Utilities
    {
        /*! @brief A channel-type argument description.
         
         The external representation of a channel-type argument description is:
         
         channelTagAndInfo ::= optionalChannelTag | mandatoryChannelTag;
         
         optionalChannelTag ::= 'c';
         
         mandatoryChannelTag ::= 'C'; */
        class ChannelArgumentDescriptor : public StringArgumentDescriptor
        {
        public :
            
            /*! @brief The constructor.
             @param argName The name of the command-line argument.
             @param argDescription A description of the command-line argument.
             @param defaultValue The default value for the command-line argument.
             @param isOptional @c true if the argument is optional and @c false otherwise.
             @param argumentReference If non-@c NULL, the variable to be set with the argument
             value. */
            ChannelArgumentDescriptor(const YarpString & argName,
                                      const YarpString & argDescription,
                                      const YarpString & defaultValue,
                                      const bool         isOptional,
                                      YarpString *       argumentReference = nullptr);
            
            /*! @brief The destructor. */
            virtual ~ChannelArgumentDescriptor(void);
            
            /*! @brief Construct a descriptor, if at all possible, from the input string.
             @param inString The input string in 'arguments' format.
             @returns A valid descriptor or @c NULL if the input is not recognized. */
            static BaseArgumentDescriptor * parseArgString(const YarpString & inString);

        protected :
        
        private :
            
            /*! @brief Convert to a printable representation.
             @returns A printable representation of the descriptor. */
            virtual YarpString toString(void);
            
            /*! @brief Check an input value against the constraints of the descriptor.
             @param value The value to be checked.
             @returns @c true if the value is within the domain of the descriptor and @c false
             otherwise. */
            virtual bool validate(const YarpString & value)
            const;
            
            COPY_AND_ASSIGNMENT_(ChannelArgumentDescriptor);
            
        public :
        
        protected :
        
        private :
            
            /*! @brief The class that this class is derived from. */
            typedef StringArgumentDescriptor inherited;
            
        }; // ChannelArgumentDescriptor
        
    } // Utilities
    
} // MplusM

#endif // ! defined(MpMChannelArgumentDescriptor_H_)