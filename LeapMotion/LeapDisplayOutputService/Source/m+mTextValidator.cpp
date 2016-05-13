//--------------------------------------------------------------------------------------------------
//
//  File:       m+mTextValidator.cpp
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

#include "m+mTextValidator.hpp"

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)

#if (! MAC_OR_LINUX_)
# include <Windows.h>
#endif //! MAC_OR_LINUX_

/*! @file

 @brief The class declaration for a text validating object. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MPlusM_Manager;
using namespace std;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

TextValidator::TextValidator(Utilities::BaseArgumentDescriptor & fieldDescriptor) :
    _fieldDescriptor(fieldDescriptor)
{
    ODL_ENTER(); //####
    ODL_P1("fieldDescriptor = ", &fieldDescriptor); //####
    ODL_EXIT_P(this); //####
} // TextValidator::TextValidator

TextValidator::~TextValidator(void)
{
    ODL_OBJENTER(); //####
    ODL_OBJEXIT(); //####
} // TextValidator::~TextValidator

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

bool
TextValidator::checkValidity(const String & toBeChecked)
const
{
    ODL_OBJENTER(); //####
    ODL_S1s("toBeChecked = ", toBeChecked.toStdString()); //####
    bool result;

    if (0 == toBeChecked.length())
    {
        if (_fieldDescriptor.isOptional())
        {
            result = true;
        }
        else
        {
            result = false;
        }
    }
    else if (_fieldDescriptor.validate(toBeChecked.toStdString()))
    {
        result = true;
    }
    else
    {
        result = false;
    }
    ODL_OBJEXIT_B(result); //####
    return result;
} // TextValidator::checkValidity

bool
TextValidator::checkValidity(const String & toBeChecked,
                             StringArray &  argsToUse)
const
{
    ODL_OBJENTER(); //####
    ODL_S1s("toBeChecked = ", toBeChecked.toStdString()); //####
    ODL_P1("argsToUse = ", &argsToUse); //####
    bool result;

    if (0 == toBeChecked.length())
    {
        if (_fieldDescriptor.isOptional())
        {
            result = true;
            argsToUse.add(_fieldDescriptor.getDefaultValue().c_str());
        }
        else
        {
            result = false;
        }
    }
    else if (_fieldDescriptor.validate(toBeChecked.toStdString()))
    {
        result = true;
        argsToUse.add(toBeChecked);
    }
    else
    {
        result = false;
    }
    ODL_OBJEXIT_B(result); //####
    return result;
} // TextValidator::checkValidity

bool
TextValidator::isForFiles(bool & isForOutput)
const
{
    ODL_OBJENTER(); //####
    bool result = _fieldDescriptor.isForFiles(isForOutput);

    ODL_OBJEXIT_B(result); //####
    return result;
} // TextValidator::isForFiles

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
