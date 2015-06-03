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
#include <mpm/M+MAddressArgumentDescriptor.h>
#include <mpm/M+MChannelArgumentDescriptor.h>
#include <mpm/M+MDoubleArgumentDescriptor.h>
#include <mpm/M+MFilePathArgumentDescriptor.h>
#include <mpm/M+MIntegerArgumentDescriptor.h>
#include <mpm/M+MPortArgumentDescriptor.h>
#include <mpm/M+MStringArgumentDescriptor.h>

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

YarpString BaseArgumentDescriptor::_parameterSeparator(":");

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
                                               const bool         isOptional) :
    _argDescription(argDescription), _argName(argName), _isOptional(isOptional)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S2s("argName = ", argName, "argDescription = ", argDescription); //####
    OD_LOG_B1("isOptional = ", isOptional); //####
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

bool BaseArgumentDescriptor::partitionString(const YarpString & inString,
                                             const size_t       indexOfDefaultValue,
                                             YarpStringVector & result)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("inString = ", inString); //####
    OD_LOG_LL1("indexOfDefaultValue = ", indexOfDefaultValue); //####
    OD_LOG_P1("result = ", &result); //####
    bool       okSoFar = false;
    YarpString workingCopy(inString);

    // We need to split the input into 'type and tag'
    result.clear();
    for (size_t fieldNumber = 0; 0 < workingCopy.length(); ++fieldNumber)
    {
        if (indexOfDefaultValue == fieldNumber)
        {
            // The default value field is special, as it has two delimiters - the inner one, which
            // is a character that is not present in the default value field, and the normal
            // separator character.
            char innerChar = workingCopy[0];

            workingCopy = workingCopy.substr(1);
            if (0 < workingCopy.length())
            {
                size_t innerIndx = workingCopy.find(innerChar, 0);
                
                if (YarpString::npos == innerIndx)
                {
                    // Badly formatted - the matching delimiter is missing!
                    break;
                }
                
                result.push_back(workingCopy.substr(0, innerIndx));
                workingCopy = workingCopy.substr(innerIndx + 1);
                if (0 < workingCopy.length())
                {
                    if (0 == workingCopy.find(_parameterSeparator))
                    {
                        workingCopy = workingCopy.substr(1);
                        okSoFar = true;
                    }
                    else
                    {
                        // Badly formatted - the delimiter is not followed by the separator!
                        break;
                    }
                    
                }
            }
            else
            {
                break;
            }
            
        }
        else
        {
            size_t indx = workingCopy.find(_parameterSeparator);

            if (YarpString::npos == indx)
            {
                result.push_back(workingCopy);
                workingCopy = "";
            }
            else
            {
                result.push_back(workingCopy.substr(0, indx));
                workingCopy = workingCopy.substr(indx + 1);
            }
        }
    }
    okSoFar &= (result.size() > indexOfDefaultValue);
    OD_LOG_EXIT_B(okSoFar); //####
    return okSoFar;
} // BaseArgumentDescriptor::partitionString

YarpString BaseArgumentDescriptor::prefixFields(const YarpString & tagForMandatoryField,
                                                        const YarpString & tagForOptionalField)
const
{
    OD_LOG_OBJENTER(); //####
    YarpString result(_argName);

    result += _parameterSeparator + (_isOptional ? tagForOptionalField : tagForMandatoryField);
    OD_LOG_OBJEXIT_s(result); //####
    return result;
} // BaseArgumentDescriptor::prefixFields

YarpString BaseArgumentDescriptor::suffixFields(void)
const
{
    OD_LOG_OBJENTER(); //####
    static const char possibles[] = "~!@#$%^&*_-+=|;\"'?./ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                    "abcdefghijklmnopqrtuvwxyz0123456789";
    char              charToUse = possibles[0];
    YarpString        result(_parameterSeparator);
    YarpString        defaultValue(getDefaultValue());
    
    if (0 < defaultValue.length())
    {
        // Determine an appropriate delimiter
        for (size_t ii = 0, mm = sizeof(possibles); mm > ii; ++ii)
        {
            if (defaultValue.npos == defaultValue.find(possibles[ii], 0))
            {
                charToUse = possibles[ii];
                break;
            }

        }
    }
    result += charToUse;
    result += defaultValue + charToUse + _parameterSeparator + _argDescription;
    OD_LOG_OBJEXIT_s(result); //####
    return result;
} // BaseArgumentDescriptor::suffixFields

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
    OD_LOG_S1s("sep = ", sep); //####
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

BaseArgumentDescriptor * Utilities::ConvertStringToArgument(const YarpString & inString)
{
    OD_LOG_ENTER(); //####
    BaseArgumentDescriptor * result = NULL;

    result = AddressArgumentDescriptor::parseArgString(inString);
    if (! result)
    {
        result = ChannelArgumentDescriptor::parseArgString(inString);
    }
    if (! result)
    {
        result = DoubleArgumentDescriptor::parseArgString(inString);
    }
    if (! result)
    {
        result = FilePathArgumentDescriptor::parseArgString(inString);
    }
    if (! result)
    {
        result = IntegerArgumentDescriptor::parseArgString(inString);
    }
    if (! result)
    {
        result = PortArgumentDescriptor::parseArgString(inString);
    }
    if (! result)
    {
        result = StringArgumentDescriptor::parseArgString(inString);
    }
    OD_LOG_EXIT_P(result); //####
    return result;
} // Utilities::ConvertStringToArguments

bool Utilities::ProcessArguments(const DescriptorVector & arguments,
                                 Option_::Parser &        parseResult)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("arguments = ", &arguments, "parseResult = ", &parseResult); //####
    bool   result = true;
    bool   sawOptional = false;
    size_t numArgs = arguments.size();
    size_t numValues = parseResult.nonOptionsCount();
#if MAC_OR_LINUX_
    size_t numToCheck = std::min(numArgs, numValues);
#else // ! MAC_OR_LINUX_
    size_t numToCheck = min(numArgs, numValues);
#endif // ! MAC_OR_LINUX_

    // Check if there are required arguments after optional arguments
    for (size_t ii = 0; result && (numArgs > ii); ++ii)
    {
        BaseArgumentDescriptor * anArg = arguments[ii];
        
        if (anArg)
        {
            if (anArg->isOptional())
            {
                sawOptional = true;
            }
            else
            {
                result = (! sawOptional);
            }
        }
    }
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

bool Utilities::PromptForValues(const DescriptorVector & arguments)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P1("arguments = ", &arguments); //####
    bool result = true;

    for (int ii = 0, mm = arguments.size(); mm > ii; ++ii)
    {
        Utilities::BaseArgumentDescriptor * anArg = arguments[ii];
        
        if (anArg)
        {
            char        inChar;
            std::string inputLine;
            
            std::cout << anArg->argumentDescription().c_str() << ": ";
            std::cout.flush();
            // Eat whitespace until we get something useful.
            for ( ; ; )
            {
                inChar = std::cin.peek();
                if (isspace(inChar))
                {
                    // Eat it.
                    std::cin.get();
                }
                else
                {
                    break;
                }
                
                if (! std::cin)
                {
                    break;
                }
                
            }
            if (getline(std::cin, inputLine))
            {
                if (! anArg->validate(inputLine))
                {
                    result = false;
                }
            }
        }
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // Utilities::PromptForValues
