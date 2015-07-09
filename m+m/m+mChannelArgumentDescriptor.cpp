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

#include "m+mChannelArgumentDescriptor.h"

#include <m+m/m+mEndpoint.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

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
                                                     const YarpString & defaultValue,
                                                     YarpString *       argumentReference) :
    inherited(argName, argDescription, argMode, defaultValue, argumentReference)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S3s("argName = ", argName, "argDescription = ", argDescription, "defaultValue = ", //####
               defaultValue); //####
    OD_LOG_P1("argumentReference = ", argumentReference); //####
    OD_LOG_EXIT_P(this); //####
} // ChannelArgumentDescriptor::ChannelArgumentDescriptor

ChannelArgumentDescriptor::~ChannelArgumentDescriptor(void)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_OBJEXIT(); //####
} // ChannelArgumentDescriptor::~ChannelArgumentDescriptor

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

BaseArgumentDescriptor * ChannelArgumentDescriptor::parseArgString(const YarpString & inString)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("inString = ", inString); //####
    BaseArgumentDescriptor * result = nullptr;
    YarpStringVector         inVector;

    if (partitionString(inString, 2, inVector))
    {
        ArgumentMode argMode = kArgModeRequired;
        bool         okSoFar = true;
        YarpString   name(inVector[0]);
        YarpString   typeTag(inVector[1]);
        YarpString   defaultString(inVector[2]);
        YarpString   description(inVector[3]);

        if (typeTag == "c")
        {
            argMode = kArgModeOptional;
        }
        else if (typeTag != "C")
        {
            okSoFar = false;
        }
        if (okSoFar)
        {
            okSoFar = Endpoint::CheckEndpointName(defaultString);
        }
        if (okSoFar)
        {
            result = new ChannelArgumentDescriptor(name, description, argMode, defaultString,
                                                   nullptr);
        }
    }
    OD_LOG_EXIT_P(result); //####
    return result;
} // ChannelArgumentDescriptor::parseArgString

YarpString ChannelArgumentDescriptor::toString(void)
{
    OD_LOG_OBJENTER(); //####
    YarpString result(prefixFields("C", "c"));

    result += suffixFields(getDefaultValue());
    OD_LOG_OBJEXIT_s(result); //####
    return result;
} // ChannelArgumentDescriptor::toString

bool ChannelArgumentDescriptor::validate(const YarpString & value)
const
{
    OD_LOG_OBJENTER(); //####
    bool result = Endpoint::CheckEndpointName(value);
    
    if (result && _argumentReference)
    {
        *_argumentReference = value;
    }
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // ChannelArgumentDescriptor::validate

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
