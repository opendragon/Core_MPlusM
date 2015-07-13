//--------------------------------------------------------------------------------------------------
//
//  File:       m+m/m+mExtraArgumentDescriptor.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the minimal functionality required to represent a
//              placeholder for trailing command-line arguments.
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
//  Created:    2015-06-08
//
//--------------------------------------------------------------------------------------------------

#include "m+mExtraArgumentDescriptor.h"

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The definition for the minimal functionality required to represent a placeholder for
 trailing command-line arguments. */
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

ExtraArgumentDescriptor::ExtraArgumentDescriptor(const YarpString & argName,
                                                 const YarpString & argDescription) :
    inherited(argName, argDescription, kArgModeOptional)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S2s("argName = ", argName, "argDescription = ", argDescription); //####
    OD_LOG_EXIT_P(this); //####
} // ExtraArgumentDescriptor::ExtraArgumentDescriptor

ExtraArgumentDescriptor::~ExtraArgumentDescriptor(void)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_OBJEXIT(); //####
} // ExtraArgumentDescriptor::~ExtraArgumentDescriptor

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

YarpString ExtraArgumentDescriptor::getDefaultValue(void)
{
    OD_LOG_OBJENTER(); //####
    YarpString result;

    OD_LOG_OBJEXIT_s(result); //####
    return result;
} // ExtraArgumentDescriptor::getDefaultValue

YarpString ExtraArgumentDescriptor::getProcessedValue(void)
{
    OD_LOG_OBJENTER(); //####
    YarpString result;

    OD_LOG_OBJEXIT_s(result); //####
    return result;
} // ExtraArgumentDescriptor::getProcessedValue

BaseArgumentDescriptor * ExtraArgumentDescriptor::parseArgString(const YarpString & inString)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("inString = ", inString); //####
    BaseArgumentDescriptor * result = NULL;
    YarpStringVector         inVector;

    if (partitionString(inString, 3, inVector))
    {
        ArgumentMode argMode = kArgModeRequired;
        bool         okSoFar = true;
        YarpString   name(inVector[0]);
        YarpString   typeTag(inVector[1]);
        YarpString   modeString(inVector[2]);
        YarpString   defaultString(inVector[3]); // ignored
        YarpString   description(inVector[4]);

        if (typeTag != "E")
        {
            okSoFar = false;
        }
        if (okSoFar)
        {
            argMode = ModeFromString(modeString);
            okSoFar = (kArgModeUnknown != argMode);
        }
        if (okSoFar)
        {
            result = new ExtraArgumentDescriptor(name, description);
        }
    }
    OD_LOG_EXIT_P(result); //####
    return result;
} // ExtraArgumentDescriptor::parseArgString

void ExtraArgumentDescriptor::setToDefault(void)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_OBJEXIT(); //####
} // ExtraArgumentDescriptor::setToDefault

YarpString ExtraArgumentDescriptor::toString(void)
{
    OD_LOG_OBJENTER(); //####
    YarpString result(prefixFields("E"));

    result += suffixFields("");
    OD_LOG_OBJEXIT_s(result); //####
    return result;
} // ExtraArgumentDescriptor::toString

bool ExtraArgumentDescriptor::validate(const YarpString & value)
const
{
    OD_LOG_OBJENTER(); //####
    bool result = true;
    
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // ExtraArgumentDescriptor::validate

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
