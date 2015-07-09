//--------------------------------------------------------------------------------------------------
//
//  File:       m+m/m+mIntegerArgumentDescriptor.h
//
//  Project:    m+m
//
//  Contains:   The class declaration for the minimal functionality required to represent an integer
//              command-line argument.
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

#if (! defined(MpMIntegerArgumentDescriptor_H_))
# define MpMIntegerArgumentDescriptor_H_ /* Header guard */

# include <m+m/m+mBaseArgumentDescriptor.h>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for the minimal functionality required to represent an integer
 command-line argument. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace Utilities
    {
        /*! @brief An integer argument description.
         
         The external representation of an integer argument description is:
         
         integerTagAndInfo ::= optionalIntegerTag integerRange |
                                    mandatoryIntegerTag integerRange;
         
         optionalIntegerTag ::= 'i';
         
         mandatoryIntegerTag ::= 'I';
         
         integerRange ::= sep minIntegerValue sep maxIntegerValue;
         
         minIntegerValue ::= integerValue | ;
         # empty value indicates no minimum constraint.
         
         maxIntegerValue ::= integerValue | ;
         # empty value indicates no maximum constraint. */
        class IntegerArgumentDescriptor : public BaseArgumentDescriptor
        {
        public :
            
            /*! @brief The constructor.
             @param argName The name of the command-line argument.
             @param argDescription A description of the command-line argument.
             @param defaultValue The default value for the command-line argument.
             @param argMode The mode of the command-line argument.
             @param hasMinimumValue @c true if the value must be greater than or equal to a
             specified minimum and @c false otherwise.
             @param minimumValue The minimum value that is acceptable.
             @param hasMaximumValue @c true if the value must be less than or equal to a specified
             maximum and @c false otherwise.
             @param maximumValue The maximum value that is acceptable.
             @param argumentReference If non-@c NULL, the variable to be set with the argument
             value. */
            IntegerArgumentDescriptor(const YarpString & argName,
                                      const YarpString & argDescription,
                                      const ArgumentMode argMode,
                                      const int          defaultValue,
                                      const bool         hasMinimumValue,
                                      const int          minimumValue,
                                      const bool         hasMaximumValue,
                                      const int          maximumValue,
                                      int *              argumentReference = nullptr);
            
            /*! @brief The destructor. */
            virtual ~IntegerArgumentDescriptor(void);
            
            /*! @brief Construct a descriptor, if at all possible, from the input string.
             @param inString The input string in 'arguments' format.
             @returns A valid descriptor or @c NULL if the input is not recognized. */
            static BaseArgumentDescriptor * parseArgString(const YarpString & inString);

        protected :
        
            /*! @brief Return the default value.
             @returns The default value. */
            virtual YarpString getDefaultValue(void);
            
        private :
            
            /*! @brief Return the processed value.
             @returns The processed value. */
            virtual YarpString getProcessedValue(void);
            
            /*! @brief Set the associated variable to the default value. */
            virtual void setToDefault(void);
            
            /*! @brief Convert to a printable representation.
             @returns A printable representation of the descriptor. */
            virtual YarpString toString(void);
            
            /*! @brief Check an input value against the constraints of the descriptor.
             @param value The value to be checked.
             @returns @c true if the value is within the domain of the descriptor and @c false
             otherwise. */
            virtual bool validate(const YarpString & value)
            const;
            
            COPY_AND_ASSIGNMENT_(IntegerArgumentDescriptor);
            
        public :
        
        protected :
        
            /*! @brief The address of the variable to be set with the argument value. */
            int * _argumentReference;
            
        private :
            
            /*! @brief The class that this class is derived from. */
            typedef BaseArgumentDescriptor inherited;
            
            /*! @brief The default value for the command-line argument. */
            int _defaultValue;

            /*! @brief The maximum value that is acceptable. */
            int _maximumValue;
            
            /*! @brief The minimum value that is acceptable. */
            int _minimumValue;
            
            /*! @brief @c true if the value must be less than or equal to the specified maximum. */
            bool _hasMaximumValue;
            
            /*! @brief @c true if the value must be greater than or equal to the specified minimum
             or @c false otherwise. */
            bool _hasMinimumValue;
            
# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunused-private-field"
# endif // defined(__APPLE__)
            /*! @brief Filler to pad to alignment boundary */
            char _filler[6];
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)
            
        }; // IntegerArgumentDescriptor
        
    } // Utilities
    
} // MplusM

#endif // ! defined(MpMIntegerArgumentDescriptor_H_)
