//--------------------------------------------------------------------------------------------------
//
//  File:       m+m/m+mChannelArgumentDescriptor.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the minimal functionality required to represent a
//              channel-type command-line argument.
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

#include "m+mChannelArgumentDescriptor.hpp"

#include <m+m/m+mEndpoint.hpp>

//#include <odlEnable.h>
#include <odlInclude.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The definition for the minimal functionality required to represent a channel-type
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

ChannelArgumentDescriptor::ChannelArgumentDescriptor(const YarpString & argName,
                                                     const YarpString & argDescription,
                                                     const ArgumentMode argMode,
                                                     const YarpString & defaultValue) :
    inherited(argName, argDescription, argMode, defaultValue)
{
    ODL_ENTER(); //####
    ODL_S3s("argName = ", argName, "argDescription = ", argDescription, "defaultValue = ", //####
            defaultValue); //####
    ODL_EXIT_P(this); //####
} // ChannelArgumentDescriptor::ChannelArgumentDescriptor

ChannelArgumentDescriptor::~ChannelArgumentDescriptor(void)
{
    ODL_OBJENTER(); //####
    ODL_OBJEXIT(); //####
} // ChannelArgumentDescriptor::~ChannelArgumentDescriptor

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

BaseArgumentDescriptor *
ChannelArgumentDescriptor::clone(void)
{
    ODL_OBJENTER(); //####
    BaseArgumentDescriptor * result = new ChannelArgumentDescriptor(argumentName(),
                                                                    argumentDescription(),
                                                                    argumentMode(),
                                                                    getDefaultValue());

    ODL_EXIT_P(result);
    return result;
} // ChannelArgumentDescriptor::clone

BaseArgumentDescriptor *
ChannelArgumentDescriptor::parseArgString(const YarpString & inString)
{
    ODL_ENTER(); //####
    ODL_S1s("inString = ", inString); //####
    BaseArgumentDescriptor * result = NULL;
    YarpStringVector         inVector;

    if (partitionString(inString, 3, inVector))
    {
        ArgumentMode argMode;
        bool         okSoFar = true;
        YarpString   name(inVector[0]);
        YarpString   typeTag(inVector[1]);
        YarpString   modeString(inVector[2]);
        YarpString   defaultString(inVector[3]);
        YarpString   description(inVector[4]);

        if (typeTag != "C")
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
            okSoFar = Endpoint::CheckEndpointName(defaultString);
        }
        if (okSoFar)
        {
            result = new ChannelArgumentDescriptor(name, description, argMode, defaultString);
        }
    }
    ODL_EXIT_P(result); //####
    return result;
} // ChannelArgumentDescriptor::parseArgString

YarpString
ChannelArgumentDescriptor::toString(void)
{
    ODL_OBJENTER(); //####
    YarpString result(prefixFields("C"));

    result += suffixFields(getDefaultValue());
    ODL_OBJEXIT_s(result); //####
    return result;
} // ChannelArgumentDescriptor::toString

bool
ChannelArgumentDescriptor::validate(const YarpString & value)
{
    ODL_OBJENTER(); //####
    _valid = Endpoint::CheckEndpointName(value);
    ODL_B1("_valid <- ", _valid); //####
    if (_valid)
    {
        _currentValue = value;
        ODL_S1s("_currentValue <- ", _currentValue); //####
    }
    ODL_OBJEXIT_B(_valid); //####
    return _valid;
} // ChannelArgumentDescriptor::validate

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
