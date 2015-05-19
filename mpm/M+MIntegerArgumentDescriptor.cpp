//--------------------------------------------------------------------------------------------------
//
//  File:       mpm/M+MIntegerArgumentDescriptor.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for the minimal functionality required to represent an integer
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

#include <mpm/M+MIntegerArgumentDescriptor.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The definition for the minimal functionality required to represent an integer command-line
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

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

IntegerArgumentDescriptor::IntegerArgumentDescriptor(const YarpString & argName,
                                                     const YarpString & argDescription,
                                                     const int          defaultValue,
                                                     const bool         isOptional,
                                                     const bool         hasMinimumValue,
                                                     const int          minimumValue,
                                                     const bool         hasMaximumValue,
                                                     const int          maximumValue,
                                                     int *              argumentReference) :
    inherited(argName, argDescription, isOptional), _argumentReference(argumentReference),
    _defaultValue(defaultValue), _maximumValue(maximumValue), _minimumValue(minimumValue),
    _hasMaximumValue(hasMaximumValue), _hasMinimumValue(hasMinimumValue)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S2s("argName = ", argName, "argDescription = ", argDescription); //####
    OD_LOG_LL3("defaultValue = ", defaultValue, "minimumValue = ", minimumValue, //####
               "maximumValue = ", maximumValue); //####
    OD_LOG_B3("isOptional = ", isOptional, "hasMinimumValue = ", hasMinimumValue, //####
              "hasMaximumValue = ", hasMaximumValue); //####
    OD_LOG_P1("argumentReference = ", argumentReference); //####
    OD_LOG_EXIT_P(this); //####
} // IntegerArgumentDescriptor::IntegerArgumentDescriptor

IntegerArgumentDescriptor::~IntegerArgumentDescriptor(void)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_OBJEXIT(); //####
} // IntegerArgumentDescriptor::~IntegerArgumentDescriptor

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

YarpString IntegerArgumentDescriptor::getDefaultValue(void)
const
{
    OD_LOG_OBJENTER(); //####
    YarpString        result;
    std::stringstream buff;

    buff << _defaultValue;
    result = buff.str();
    OD_LOG_OBJEXIT_s(result); //####
    return result;
} // IntegerArgumentDescriptor::getDefaultValue

YarpString IntegerArgumentDescriptor::getProcessedValue(void)
const
{
    OD_LOG_OBJENTER(); //####
    YarpString        result;
    std::stringstream buff;

    buff << (_argumentReference ? *_argumentReference : _defaultValue);
    result = buff.str();
    OD_LOG_OBJEXIT_s(result); //####
    return result;
} // IntegerArgumentDescriptor::getProcessedValue

BaseArgumentDescriptor * IntegerArgumentDescriptor::parseArgString(const YarpString & inString)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("inString = ", inString); //####
    BaseArgumentDescriptor * result = NULL;
    YarpStringVector         inVector;

    if (partitionString(inString, 4, inVector))
    {
        bool       isOptional = false;
        bool       okSoFar = true;
        int        defaultValue;
        int        maxValue;
        int        minValue;
        YarpString name(inVector[0]);
        YarpString typeTag(inVector[1]);
        YarpString minValString(inVector[2]);
        YarpString maxValString(inVector[3]);
        YarpString defaultString(inVector[4]);
        YarpString description(inVector[5]);

        if (typeTag == "i")
        {
            isOptional = true;
        }
        else if (typeTag != "I")
        {
            okSoFar = false;
        }
        if (okSoFar && (0 < defaultString.length()))
        {
            const char * startPtr = defaultString.c_str();
            char *       endPtr;

            defaultValue = strtol(startPtr, &endPtr, 10);
            if ((startPtr == endPtr) || *endPtr)
            {
                okSoFar = false;
            }
        }
        if (okSoFar && (0 < minValString.length()))
        {
            const char * startPtr = minValString.c_str();
            char *       endPtr;

            minValue = strtol(startPtr, &endPtr, 10);
            if ((startPtr == endPtr) || *endPtr)
            {
                okSoFar = false;
            }
        }
        if (okSoFar && (0 < maxValString.length()))
        {
            const char * startPtr = maxValString.c_str();
            char *       endPtr;

            maxValue = strtol(startPtr, &endPtr, 10);
            if ((startPtr == endPtr) || *endPtr)
            {
                okSoFar = false;
            }
        }
        if (okSoFar)
        {
            bool hasMaximumValue = (0 < maxValString.length());
            bool hasMinimumValue = (0 < minValString.length());

            result = new IntegerArgumentDescriptor(name, description, defaultValue, isOptional,
                                                   hasMinimumValue, hasMinimumValue ? minValue : 0,
                                                   hasMaximumValue, hasMaximumValue ? maxValue : 0,
                                                   NULL);
        }
    }
    OD_LOG_EXIT_P(result); //####
    return result;
} // IntegerArgumentDescriptor::parseArgString

void IntegerArgumentDescriptor::setToDefault(void)
const
{
    OD_LOG_OBJENTER(); //####
    if (_argumentReference)
    {
        *_argumentReference = _defaultValue;
    }
    OD_LOG_OBJEXIT(); //####
} // IntegerArgumentDescriptor::setToDefault

YarpString IntegerArgumentDescriptor::toString(void)
const
{
    OD_LOG_OBJENTER(); //####
    YarpString result(prefixFields("I", "i"));

    result += _parameterSeparator;
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
    result += suffixFields();
    OD_LOG_OBJEXIT_s(result); //####
    return result;
} // IntegerArgumentDescriptor::toString

bool IntegerArgumentDescriptor::validate(const YarpString & value)
const
{
    OD_LOG_OBJENTER(); //####
    bool         result = false;
    const char * startPtr = value.c_str();
    char *       endPtr;
    int          intValue = strtol(startPtr, &endPtr, 10);
    
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
        *_argumentReference = intValue;
    }
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // IntegerArgumentDescriptor::validate

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
