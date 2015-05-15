//--------------------------------------------------------------------------------------------------
//
//  File:       mpm/M+MDoubleArgumentDescriptor.h
//
//  Project:    M+M
//
//  Contains:   The class declaration for the minimal functionality required to represent a floating
//              point command-line argument.
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

#if (! defined(MpMDoubleArgumentDescriptor_H_))
# define MpMDoubleArgumentDescriptor_H_ /* Header guard */

# include <mpm/M+MBaseArgumentDescriptor.h>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for the minimal functionality required to represent a floating point
 command-line argument. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace Utilities
    {
        /*! @brief A floating point argument description. */
        class DoubleArgumentDescriptor : public BaseArgumentDescriptor
        {
        public :
            
            /*! @brief The constructor.
             @param argName The name of the command-line argument.
             @param argDescription A description of the command-line argument.
             @param isOptional @c true if the argument is optional and @c false otherwise.
             @param hasMinimumValue @c true if the value must be greater than or equal to a
             specified minimum and @c false otherwise.
             @param minimumValue The minimum value that is acceptable.
             @param hasMaximumValue @c true if the value must be less than or equal to a specified
             maximum and @c false otherwise.
             @param maximumValue The maximum value that is acceptable.
             @param argumentReference If non-@c NULL, the variable to be set with the argument
             value. */
            DoubleArgumentDescriptor(const Common::YarpString & argName,
                                     const Common::YarpString & argDescription,
                                     const Common::YarpString & defaultValue,
                                     const bool                 isOptional,
                                     const bool                 hasMinimumValue,
                                     const double               minimumValue,
                                     const bool                 hasMaximumValue,
                                     const double               maximumValue,
                                     Common::YarpString *       argumentReference = NULL);
            
            /*! @brief The destructor. */
            virtual ~DoubleArgumentDescriptor(void);
            
            /*! @brief Convert to a printable representation.
             @returns A printable representation of the descriptor. */
            virtual Common::YarpString toString(void)
            const;
            
            /*! @brief Check an input value against the constraints of the descriptor.
             @param value The value to be checked.
             @returns @c true if the value is within the domain of the descriptor and @c false
             otherwise. */
            virtual bool validate(const Common::YarpString & value)
            const;
            
        protected :
        
        private :
            
            COPY_AND_ASSIGNMENT_(DoubleArgumentDescriptor);
            
        public :
        
        protected :
        
        private :
            
            /*! @brief The class that this class is derived from. */
            typedef BaseArgumentDescriptor inherited;
            
            /*! @brief The maximum value that is acceptable. */
            double _maximumValue;
            
            /*! @brief The minimum value that is acceptable. */
            double _minimumValue;
            
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
            
        }; // DoubleArgumentDescriptor
        
    } // Utilities
    
} // MplusM

#endif // ! defined(MpMDoubleArgumentDescriptor_H_)
