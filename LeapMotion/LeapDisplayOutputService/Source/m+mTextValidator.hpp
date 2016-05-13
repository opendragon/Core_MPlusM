//--------------------------------------------------------------------------------------------------
//
//  File:       m+mTextValidator.hpp
//
//  Project:    m+m
//
//  Contains:   The class declaration for a text validating object.
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
//  Created:    2015-06-11
//
//--------------------------------------------------------------------------------------------------

#if (! defined(mpmTextValidator_HPP_))
# define mpmTextValidator_HPP_ /* Header guard */

# include "m+mManagerDataTypes.hpp"

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file

 @brief The class declaration for a text validating object. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MPlusM_Manager
{
    /*! @brief A text validating object. */
    class TextValidator
    {
    public :

    protected :

    private :

    public :

        /*! @brief The constructor.
         @param fieldDescriptor A description of the attributes of the field being validated. */
        explicit
        TextValidator(MplusM::Utilities::BaseArgumentDescriptor & fieldDescriptor);

        /*! @brief The destructor. */
        virtual
        ~TextValidator(void);

        /*! @brief Check if the provided value is valid according to the field description.
         @param toBeChecked The value to be checked.
         @returns @c true if the value is accepted by the field description and @c false
         otherwise. */
        bool
        checkValidity(const String & toBeChecked)
        const;

        /*! @brief Check if the provided value is valid according to the field description.
         @param toBeChecked The value to be checked.
         @param argsToUse A set of valid arguments.
         @returns @c true if the value is accepted by the field description and @c false
         otherwise. */
        bool
        checkValidity(const String & toBeChecked,
                      StringArray &  argsToUse)
        const;

        /*! @brief Return @c true if the validator is for file paths and @c false otherwise.
         @param isForOutput Set to @c true if the validator is for output files and @c false
         otherwise.
         @returns @c true if the validator is for file paths and @c false otherwise. */
        bool
        isForFiles(bool & isForOutput)
        const;

    protected :

    private :

    public :

    protected :

    private :

        /*! @brief A description of the attributes of the field being validated. */
        MplusM::Utilities::BaseArgumentDescriptor & _fieldDescriptor;

        JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR(TextValidator)

    }; // TextValidator

} // MPlusM_Manager

#endif // ! defined(mpmTextValidator_HPP_)
