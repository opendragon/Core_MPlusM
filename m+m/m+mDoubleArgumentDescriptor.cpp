//--------------------------------------------------------------------------------------------------
//
//  File:       m+m/m+mDoubleArgumentDescriptor.cpp
//
//  Project:    m+m
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

#include "m+mDoubleArgumentDescriptor.h"

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
                                                   const ArgumentMode argMode,
                                                   const double       defaultValue,
                                                   const bool         hasMinimumValue,
                                                   const double       minimumValue,
                                                   const bool         hasMaximumValue,
                                                   const double       maximumValue) :
    inherited(argName, argDescription, argMode), _defaultValue(defaultValue),
    _maximumValue(maximumValue), _minimumValue(minimumValue), _hasMaximumValue(hasMaximumValue),
    _hasMinimumValue(hasMinimumValue)
{
    ODL_ENTER(); //####
    ODL_S2s("argName = ", argName, "argDescription = ", argDescription); //####
    ODL_D3("defaultValue = ", defaultValue, "minimumValue = ", minimumValue, //####
              "maximumValue = ", maximumValue); //####
    ODL_B2("hasMinimumValue = ", hasMinimumValue, "hasMaximumValue = ", hasMaximumValue); //####
    ODL_EXIT_P(this); //####
} // DoubleArgumentDescriptor::DoubleArgumentDescriptor

DoubleArgumentDescriptor::~DoubleArgumentDescriptor(void)
{
    ODL_OBJENTER(); //####
    ODL_OBJEXIT(); //####
} // DoubleArgumentDescriptor::~DoubleArgumentDescriptor

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

DEFINE_ADDVALUETOBOTTLE_(DoubleArgumentDescriptor)
{
    ODL_ENTER(); //####
    ODL_P1("container = ", &container); //####

    container.addDouble(_currentValue);
    ODL_EXIT(); //####
} // DoubleArgumentDescriptor::addValueToBottle

DEFINE_CLONE_(DoubleArgumentDescriptor)
{
    ODL_OBJENTER(); //####
    BaseArgumentDescriptor * result = new DoubleArgumentDescriptor(argumentName(),
                                                                   argumentDescription(),
                                                                   argumentMode(), _defaultValue,
                                                                   _hasMinimumValue, _minimumValue,
                                                                   _hasMaximumValue, _maximumValue);

    ODL_EXIT_P(result);
    return result;
} // DoubleArgumentDescriptor::clone

DEFINE_GETDEFAULTVALUE_(DoubleArgumentDescriptor)
{
    ODL_OBJENTER(); //####
    YarpString        result;
    std::stringstream buff;

    buff << _defaultValue;
    result = buff.str();
    ODL_OBJEXIT_s(result); //####
    return result;
} // DoubleArgumentDescriptor::getDefaultValue

DEFINE_GETPROCESSEDVALUE_(DoubleArgumentDescriptor)
{
    ODL_OBJENTER(); //####
    YarpString        result;
    std::stringstream buff;

    buff << _currentValue;
    result = buff.str();
    ODL_OBJEXIT_s(result); //####
    return result;
} // DoubleArgumentDescriptor::getProcessedValue

BaseArgumentDescriptor *
DoubleArgumentDescriptor::parseArgString(const YarpString & inString)
{
    ODL_ENTER(); //####
    ODL_S1s("inString = ", inString); //####
    BaseArgumentDescriptor * result = NULL;
    YarpStringVector         inVector;

    if (partitionString(inString, 5, inVector))
    {
        ArgumentMode argMode;
        bool         okSoFar = true;
        double       defaultValue = 0;
        double       maxValue = 0;
        double       minValue = 0;
        YarpString   name(inVector[0]);
        YarpString   typeTag(inVector[1]);
        YarpString   modeString(inVector[2]);
        YarpString   minValString(inVector[3]);
        YarpString   maxValString(inVector[4]);
        YarpString   defaultString(inVector[5]);
        YarpString   description(inVector[6]);

        if (typeTag != "D")
        {
            okSoFar = false;
        }
        if (okSoFar)
        {
            argMode = ModeFromString(modeString);
            okSoFar = (kArgModeUnknown != argMode);
        }
        else
        {
            argMode = kArgModeUnknown;
        }
        if (okSoFar && (0 < defaultString.length()))
        {
            const char * startPtr = defaultString.c_str();
            char *       endPtr;

            defaultValue = strtod(startPtr, &endPtr);
            if ((startPtr == endPtr) || *endPtr)
            {
                okSoFar = false;
            }
        }
        if (okSoFar && (0 < minValString.length()))
        {
            const char * startPtr = minValString.c_str();
            char *       endPtr;

            minValue = strtod(startPtr, &endPtr);
            if ((startPtr == endPtr) || *endPtr)
            {
                okSoFar = false;
            }
        }
        if (okSoFar && (0 < maxValString.length()))
        {
            const char * startPtr = maxValString.c_str();
            char *       endPtr;

            maxValue = strtod(startPtr, &endPtr);
            if ((startPtr == endPtr) || *endPtr)
            {
                okSoFar = false;
            }
        }
        if (okSoFar)
        {
            bool hasMaximumValue = (0 < maxValString.length());
            bool hasMinimumValue = (0 < minValString.length());

            result = new DoubleArgumentDescriptor(name, description, argMode, defaultValue,
                                                  hasMinimumValue, hasMinimumValue ? minValue : 0,
                                                  hasMaximumValue, hasMaximumValue ? maxValue : 0);
        }
    }
    ODL_EXIT_P(result); //####
    return result;
} // DoubleArgumentDescriptor::parseArgString

DEFINE_SETTODEFAULTVALUE_(DoubleArgumentDescriptor)
{
    ODL_OBJENTER(); //####
    _currentValue = _defaultValue;
    ODL_D1("_currentValue <- ", _currentValue); //####
    ODL_OBJEXIT(); //####
} // DoubleArgumentDescriptor::setToDefaultValue

DEFINE_TOSTRING_(DoubleArgumentDescriptor)
{
    ODL_OBJENTER(); //####
    YarpString result(prefixFields("D"));

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
    result += suffixFields(getDefaultValue());
    ODL_OBJEXIT_s(result); //####
    return result;
} // DoubleArgumentDescriptor::toString

DEFINE_VALIDATE_(DoubleArgumentDescriptor)
{
    ODL_OBJENTER(); //####
    const char * startPtr = value.c_str();
    char *       endPtr;
    double       dblValue = strtod(startPtr, &endPtr);

    if ((startPtr != endPtr) && (! *endPtr))
    {
        _valid = true;
        ODL_B1("_valid <- ", _valid); //####
        if (_hasMinimumValue && (dblValue < _minimumValue))
        {
            _valid = false;
            ODL_B1("_valid <- ", _valid); //####
        }
        if (_hasMaximumValue && (dblValue > _maximumValue))
        {
            _valid = false;
            ODL_B1("_valid <- ", _valid); //####
        }
    }
    else
    {
        _valid = false;
        ODL_B1("_valid <- ", _valid); //####
    }
    if (_valid)
    {
        _currentValue = dblValue;
        ODL_D1("_currentValue <- ", _currentValue); //####
    }
    ODL_OBJEXIT_B(_valid); //####
    return _valid;
} // DoubleArgumentDescriptor::validate

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
