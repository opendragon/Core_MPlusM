//--------------------------------------------------------------------------------------------------
//
//  File:       m+m/m+mBoolArgumentDescriptor.hpp
//
//  Project:    m+m
//
//  Contains:   The class declaration for the minimal functionality required to represent a boolean
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
//  Created:    2015-08-25
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMBoolArgumentDescriptor_HPP_))
# define MpMBoolArgumentDescriptor_HPP_ /* Header guard */

# include <m+m/m+mBaseArgumentDescriptor.hpp>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for the minimal functionality required to represent a boolean
 command-line argument. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace Utilities
    {
        /*! @brief A boolean argument description.

         The external representation of a boolean argument description is:

         booleanTagAndInfo ::= 'B'; */
        class BoolArgumentDescriptor : public BaseArgumentDescriptor
        {
        public :

        protected :

        private :

            /*! @brief The class that this class is derived from. */
            typedef BaseArgumentDescriptor inherited;

        public :

            /*! @brief The constructor.
             @param[in] argName The name of the command-line argument.
             @param[in] argDescription A description of the command-line argument.
             @param[in] argMode The mode of the command-line argument.
             @param[in] defaultValue The default value for the command-line argument. */
            BoolArgumentDescriptor(const YarpString & argName,
                                   const YarpString & argDescription,
                                   const ArgumentMode argMode,
                                   const bool         defaultValue);

            /*! @brief The destructor. */
            virtual
            ~BoolArgumentDescriptor(void);

            /*! @brief Return the current value.
             @return The current value. */
            inline bool
            getCurrentValue(void)
            const
            {
                return _currentValue;
            } // getCurrentValue

            /*! @brief Return @c true if the argument is for Boolean arguments.
             @return @c true if the argument is for Boolean arguments and @c false otherwise. */
            virtual bool
            isBoolean(void)
            const
            {
                return true;
            } // isBoolean

            /*! @brief Construct a descriptor, if at all possible, from the input string.
             @param[in] inString The input string in 'arguments' format.
             @return A valid descriptor or @c NULL if the input is not recognized. */
            static BaseArgumentDescriptor *
            parseArgString(const YarpString & inString);

        protected :

            /*! @brief Return the default value.
             @return The default value. */
            virtual YarpString
            getDefaultValue(void);

        private :

            /*! @brief The copy constructor.
             @param[in] other The object to be copied. */
            BoolArgumentDescriptor(const BoolArgumentDescriptor & other);

            /*! @brief Add the processed value to a bottle.
             @param[in,out] container The bottle to be modified. */
            virtual void
            addValueToBottle(yarp::os::Bottle & container);

            /*! @brief Return a copy of the descriptor, with only non-pointer types duplicated.
             @return A copy of the descriptor, with only non-pointer types duplicated. */
            virtual BaseArgumentDescriptor *
            clone(void);

            /*! @brief Return the processed value.
             @return The processed value. */
            virtual YarpString
            getProcessedValue(void);

            /*! @brief The assignment operator.
             @param[in] other The object to be copied.
             @return The updated object. */
            BoolArgumentDescriptor &
            operator =(const BoolArgumentDescriptor & other);

            /*! @brief Set the associated variable to the default value. */
            virtual void
            setToDefaultValue(void);

            /*! @brief Convert to a printable representation.
             @return A printable representation of the descriptor. */
            virtual YarpString
            toString(void);

            /*! @brief Check an input value against the constraints of the descriptor.
             @param[in] value The value to be checked.
             @return @c true if the value is within the domain of the descriptor and @c false
             otherwise. */
            virtual bool
            validate(const YarpString & value);

        public :

        protected :

            /*! @brief The address of the variable to be set with the argument value. */
            bool * _argumentReference;

            /*! @brief The default value for the command-line argument. */
            bool _defaultValue;

        private :

            /*! @brief The current value of the command-line argument. */
            bool _currentValue;

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunused-private-field"
# endif // defined(__APPLE__)
            /*! @brief Filler to pad to alignment boundary */
            char _filler[7];
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

        }; // BoolArgumentDescriptor

    } // Utilities

} // MplusM

#endif // ! defined(MpMBoolArgumentDescriptor_HPP_)
