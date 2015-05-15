//--------------------------------------------------------------------------------------------------
//
//  File:       mpm/M+MDoubleArgumentDescriptor.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for the minimal functionality required to represent a floating
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

#include <mpm/M+MDoubleArgumentDescriptor.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The definition for the minimal functionality required to represent a floating point
 command-line argument. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Utilities;

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

DoubleArgumentDescriptor::DoubleArgumentDescriptor(const YarpString & argName,
                                                   const YarpString & argDescription,
                                                   const YarpString & defaultValue,
                                                   const bool         isOptional,
                                                   const bool         hasMinimumValue,
                                                   const double       minimumValue,
                                                   const bool         hasMaximumValue,
                                                   const double       maximumValue,
                                                   YarpString *       argumentReference) :
    inherited(argName, argDescription, defaultValue, isOptional, argumentReference),
    _maximumValue(maximumValue), _minimumValue(minimumValue), _hasMaximumValue(hasMaximumValue),
    _hasMinimumValue(hasMinimumValue)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S3("argName = ", argName, "argDescription = ", argDescription, "defaultValue = ", //####
              defaultValue); //####
    OD_LOG_B3("isOptional = ", isOptional, "hasMinimumValue = ", hasMinimumValue, //####
              "hasMaximumValue = ", hasMaximumValue); //####
    OD_LOG_D2("minimumValue = ", minimumValue, "maximumValue = ", maximumValue); //####
    OD_LOG_P1("argumentReference = ", argumentReference); //####
    OD_LOG_EXIT_P(this); //####
} // DoubleArgumentDescriptor::DoubleArgumentDescriptor

DoubleArgumentDescriptor::~DoubleArgumentDescriptor(void)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_OBJEXIT(); //####
} // DoubleArgumentDescriptor::~DoubleArgumentDescriptor

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

Common::YarpString DoubleArgumentDescriptor::toString(void)
const
{
    OD_LOG_OBJENTER(); //####
    Common::YarpString result(isOptional() ? "d" : "D");
    
    if (_hasMinimumValue || _hasMaximumValue)
    {
        result += "r";
        if (_hasMinimumValue)
        {
            std::stringstream buff;
            
            buff << _minimumValue;
            result += buff.str();
        }
        result += _parameterSeparator;
        if (_hasMaximumValue)
        {
            std::stringstream buff;
            
            buff << _maximumValue;
            result += buff.str();
        }
    }
    result += standardFields();
    OD_LOG_OBJEXIT_s(result); //####
    return result;
} // DoubleArgumentDescriptor::toString

bool DoubleArgumentDescriptor::validate(const Common::YarpString & value)
const
{
    OD_LOG_OBJENTER(); //####
    bool         result = false;
    const char * startPtr = value.c_str();
    char *       endPtr;
    double       intValue = strtod(startPtr, &endPtr);
    
    if ((startPtr != endPtr) && (! *endPtr))
    {
        result = true;
        if (_hasMinimumValue && (intValue < _minimumValue))
        {
            result = false;
        }
        if (_hasMaximumValue && (intValue > _maximumValue))
        {
            result = false;
        }
    }
    if (result && _argumentReference)
    {
        *_argumentReference = value;
    }
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // DoubleArgumentDescriptor::validate

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
