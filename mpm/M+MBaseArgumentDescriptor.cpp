//--------------------------------------------------------------------------------------------------
//
//  File:       mpm/M+MBaseArgumentDescriptor.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for the minimal functionality required to represent a
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

#include <mpm/M+MBaseArgumentDescriptor.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the minimal functionality required to represent a command-line
 argument. */
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

Common::YarpString BaseArgumentDescriptor::_parameterSeparator(":");

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

BaseArgumentDescriptor::BaseArgumentDescriptor(const YarpString & argName,
                                               const YarpString & argDescription,
                                               const YarpString & defaultValue,
                                               const bool         isOptional,
                                               YarpString *       argumentReference) :
    _argumentReference(argumentReference), _argDescription(argDescription), _argName(argName),
    _defaultValue(defaultValue), _isOptional(isOptional)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S3("argName = ", argName, "argDescription = ", argDescription, "defaultValue = ", //####
              defaultValue); //####
    OD_LOG_B1("isOptional = ", isOptional); //####
    OD_LOG_P1("argumentReference = ", argumentReference); //####
    OD_LOG_EXIT_P(this); //####
} // BaseArgumentDescriptor::BaseArgumentDescriptor

BaseArgumentDescriptor::~BaseArgumentDescriptor(void)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_OBJEXIT(); //####
} // BaseArgumentDescriptor::~BaseArgumentDescriptor

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

YarpString BaseArgumentDescriptor::getProcessedValue(void)
const
{
    OD_LOG_OBJENTER(); //####
    YarpString result;
    
    if (_argumentReference)
    {
        result = *_argumentReference;
    }
    else
    {
        result = _defaultValue;
    }
    OD_LOG_OBJEXIT_s(result); //####
    return result;
} // BaseArgumentDescriptor::getProcessedValue

void BaseArgumentDescriptor::setToDefault(void)
{
    OD_LOG_OBJENTER(); //####
    if (_argumentReference)
    {
        *_argumentReference = _defaultValue;
    }
    OD_LOG_OBJEXIT(); //####
} // BaseArgumentDescriptor::setToDefault

YarpString BaseArgumentDescriptor::standardFields(void)
const
{
    OD_LOG_OBJENTER(); //####
    YarpString result(_parameterSeparator);
    
    if (0 < _defaultValue.length())
    {
        // Determine an appropriate delimiter
        static const char possibles[] = "~!@#$%^&*_-+=|;\"'?/ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                        "abcdefghijklmnopqrtuvwxyz0123456789";
        char              charToUse = '\\';
        
        for (size_t ii = 0, mm = sizeof(possibles); mm > ii; ++ii)
        {
            if (_defaultValue.npos == _defaultValue.find(possibles[ii], 0))
            {
                charToUse = possibles[ii];
                break;
            }

        }
        result += charToUse;
        result += _defaultValue + charToUse;
    }
    else
    {
        result += "||";
    }
    result += _parameterSeparator + _argDescription;
    OD_LOG_OBJEXIT_s(result); //####
    return result;
} // BaseArgumentDescriptor::standardFields

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

YarpString Utilities::ArgumentsToArgString(const DescriptorVector & arguments)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P1("arguments = ", &arguments); //####
    YarpString result;
    size_t     numOptional = 0;
    
    for (int ii = 0, mm = arguments.size(); mm > ii; ++ii)
    {
        BaseArgumentDescriptor * anArg = arguments[ii];
        
        if (anArg)
        {
            if (0 < ii)
            {
                result += " ";
            }
            if (anArg->isOptional())
            {
                result += "[";
                ++numOptional;
            }
            result += anArg->argumentName();
        }
    }
    if (0 < numOptional)
    {
        result += YarpString(numOptional, ']');
    }
    OD_LOG_EXIT_s(result); //####
    return result;
} // ArgumentsToArgString

void Utilities::ArgumentsToDescriptionArray(const DescriptorVector & arguments,
                                            YarpStringVector &       output,
                                            const size_t             minSpace)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("arguments = ", &arguments, "output = ", &output); //####
    int nameSize = -1;
    
    for (int ii = 0, mm = arguments.size(); mm > ii; ++ii)
    {
        BaseArgumentDescriptor * anArg = arguments[ii];
        
        if (anArg)
        {
            int len = anArg->argumentName().length();
            
            if (nameSize < len)
            {
                nameSize = len;
            }
        }
    }
    if (0 < nameSize)
    {
        nameSize += minSpace;
        for (int ii = 0, mm = arguments.size(); mm > ii; ++ii)
        {
            BaseArgumentDescriptor * anArg = arguments[ii];
            
            if (anArg)
            {
                YarpString aLine(anArg->argumentName());
                int        len = aLine.length();
                
                aLine += YarpString(nameSize - len, ' ');
                if (anArg->isOptional())
                {
                    aLine += "(Optional) ";
                }
                aLine += anArg->argumentDescription();
                output.push_back(aLine);
            }
        }
    }
    OD_LOG_EXIT(); //####
} // Utilities::ArgumentsToDescriptionArray

YarpString Utilities::CombineArguments(const DescriptorVector & arguments,
                                       const YarpString &       sep)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s(sep, sep); //####
    YarpString result;
    
    for (int ii = 0, mm = arguments.size(); mm > ii; ++ii)
    {
        BaseArgumentDescriptor * anArg = arguments[ii];
        
        if (0 < ii)
        {
            result += sep;
        }
        if (anArg)
        {
            result += anArg->getProcessedValue();
        }
    }
    OD_LOG_EXIT_s(result); //####
    return result;
} // Utilities::CombineArguments

bool Utilities::ProcessArguments(const DescriptorVector & arguments,
                                 Option_::Parser &        parseResult)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("arguments = ", &arguments, "parseResult = ", &parseResult); //####
    bool   result = true;
    size_t numArgs = arguments.size();
    size_t numValues = parseResult.nonOptionsCount();
    size_t numToCheck = std::min(numArgs, numValues);
    
    for (size_t ii = 0; result && (numToCheck > ii); ++ii)
    {
        BaseArgumentDescriptor * anArg = arguments[ii];
        
        if (anArg)
        {
            result = anArg->validate(parseResult.nonOption(ii));
        }
    }
    if (result && (numToCheck < numArgs))
    {
        for (size_t ii = numToCheck; result && (numArgs > ii); ++ii)
        {
            BaseArgumentDescriptor * anArg = arguments[ii];
            
            if (anArg)
            {
                if (anArg->isOptional())
                {
                    anArg->setToDefault();
                }
                else
                {
                    result = false;
                }
            }
        }
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // Utilities::ProcessArguments
