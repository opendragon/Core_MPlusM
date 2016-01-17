//--------------------------------------------------------------------------------------------------
//
//  File:       m+m/m+mBoolArgumentDescriptor.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the minimal functionality required to represent a boolean
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

#include "m+mBoolArgumentDescriptor.h"

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The definition for the minimal functionality required to represent a boolean command-line
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

BoolArgumentDescriptor::BoolArgumentDescriptor(const YarpString & argName,
                                               const YarpString & argDescription,
                                               const ArgumentMode argMode,
                                               const bool         defaultValue) :
    inherited(argName, argDescription, argMode), _defaultValue(defaultValue)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S2s("argName = ", argName, "argDescription = ", argDescription); //####
    OD_LOG_B1("defaultValue = ", defaultValue); //####
    OD_LOG_EXIT_P(this); //####
} // BoolArgumentDescriptor::BoolArgumentDescriptor

BoolArgumentDescriptor::~BoolArgumentDescriptor(void)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_OBJEXIT(); //####
} // BoolArgumentDescriptor::~BoolArgumentDescriptor

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

DEFINE_ADDVALUETOBOTTLE_(BoolArgumentDescriptor)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P1("container = ", &container); //####
    container.addInt(_currentValue ? 1 : 0);
    OD_LOG_EXIT(); //####
} // BoolArgumentDescriptor::addValueToBottle

DEFINE_CLONE_(BoolArgumentDescriptor)
{
    OD_LOG_OBJENTER(); //####
    BaseArgumentDescriptor * result = new BoolArgumentDescriptor(argumentName(),
                                                                 argumentDescription(),
                                                                 argumentMode(), _defaultValue);

    OD_LOG_EXIT_P(result);
    return result;
} // BoolArgumentDescriptor::clone

DEFINE_GETDEFAULTVALUE_(BoolArgumentDescriptor)
{
    OD_LOG_OBJENTER(); //####
    YarpString result(_defaultValue ? "1" : "0");
    
    OD_LOG_OBJEXIT_s(result); //####
    return result;
} // BoolArgumentDescriptor::getDefaultValue

DEFINE_GETPROCESSEDVALUE_(BoolArgumentDescriptor)
{
    OD_LOG_OBJENTER(); //####
    YarpString result(_currentValue ? "1" : "0");

    OD_LOG_OBJEXIT_s(result); //####
    return result;
} // BoolArgumentDescriptor::getProcessedValue

BaseArgumentDescriptor *
BoolArgumentDescriptor::parseArgString(const YarpString & inString)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("inString = ", inString); //####
    BaseArgumentDescriptor * result = NULL;
    YarpStringVector         inVector;

    if (partitionString(inString, 3, inVector))
    {
        ArgumentMode argMode;
        bool         okSoFar = true;
        bool         defaultValue;
        YarpString   name(inVector[0]);
        YarpString   typeTag(inVector[1]);
        YarpString   modeString(inVector[2]);        
        YarpString   defaultString(inVector[3]);
        YarpString   description(inVector[4]);

        if (typeTag != "B")
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
        if (okSoFar)
        {
            if (0 < defaultString.length())
            {
                if ('1' == defaultString[0])
                {
                    defaultValue = true;
                }
                else if ('0' == defaultString[0])
                {
                    defaultValue = false;
                }
                else
                {
                    okSoFar = false;
                }
            }
            else
            {
                okSoFar = false;
            }
        }
        if (okSoFar)
        {
            result = new BoolArgumentDescriptor(name, description, argMode, defaultValue);
        }
    }
    OD_LOG_EXIT_P(result); //####
    return result;
} // BoolArgumentDescriptor::parseArgString

DEFINE_SETTODEFAULTVALUE_(BoolArgumentDescriptor)
{
    OD_LOG_OBJENTER(); //####
    _currentValue = _defaultValue;
    OD_LOG_B1("_currentValue <- ", _currentValue); //####
    OD_LOG_OBJEXIT(); //####
} // BoolArgumentDescriptor::setToDefaultValue

DEFINE_TOSTRING_(BoolArgumentDescriptor)
{
    OD_LOG_OBJENTER(); //####
    YarpString result(prefixFields("B"));

    result += suffixFields(getDefaultValue());
    OD_LOG_OBJEXIT_s(result); //####
    return result;
} // BoolArgumentDescriptor::toString

DEFINE_VALIDATE_(BoolArgumentDescriptor)
{
    OD_LOG_OBJENTER(); //####
    bool boolValue;
    char firstChar = tolower(value[0]);
    
    if (('0' == firstChar) || ('f' == firstChar) || ('n' == firstChar))
    {
        boolValue = false;
        _valid = true;
        OD_LOG_B1("_valid <- ", _valid); //####
    }
    else if (('1' == firstChar) || ('t' == firstChar) || ('y' == firstChar))
    {
        boolValue = true;
        _valid = true;
        OD_LOG_B1("_valid <- ", _valid); //####
    }
    else
    {
        _valid = false;
        OD_LOG_B1("_valid <- ", _valid); //####
    }
    if (_valid)
    {
        _currentValue = boolValue;
        OD_LOG_B1("_currentValue <- ", _currentValue); //####
    }
    OD_LOG_OBJEXIT_B(_valid); //####
    return _valid;
} // BoolArgumentDescriptor::validate

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
