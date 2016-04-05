//--------------------------------------------------------------------------------------------------
//
//  File:       m+m/m+mPortArgumentDescriptor.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the minimal functionality required to represent a port
//              number command-line argument.
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
//  Created:    2015-05-21
//
//--------------------------------------------------------------------------------------------------

#include "m+mPortArgumentDescriptor.h"

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The definition for the minimal functionality required to represent a port number
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

PortArgumentDescriptor::PortArgumentDescriptor(const YarpString & argName,
                                               const YarpString & argDescription,
                                               const ArgumentMode argMode,
                                               const int          defaultValue,
                                               const bool         isSystemPort) :
    inherited(argName, argDescription, argMode, defaultValue, true,
              isSystemPort ? 0 : MINIMUM_PORT_ALLOWED_, true, MAXIMUM_PORT_ALLOWED_),
    _isSystemPort(isSystemPort)
{
    ODL_ENTER(); //####
    ODL_S2s("argName = ", argName, "argDescription = ", argDescription); //####
    ODL_LL1("defaultValue = ", defaultValue); //####
    ODL_B1("isSystemPort = ", isSystemPort); //####
    ODL_P1("argumentReference = ", argumentReference); //####
    ODL_EXIT_P(this); //####
} // PortArgumentDescriptor::PortArgumentDescriptor

PortArgumentDescriptor::~PortArgumentDescriptor(void)
{
    ODL_OBJENTER(); //####
    ODL_OBJEXIT(); //####
} // PortArgumentDescriptor::~PortArgumentDescriptor

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

DEFINE_CLONE_(PortArgumentDescriptor)
{
    ODL_OBJENTER(); //####
    BaseArgumentDescriptor * result = new PortArgumentDescriptor(argumentName(),
                                                                 argumentDescription(),
                                                                 argumentMode(), _defaultValue,
                                                                 _isSystemPort);

    ODL_EXIT_P(result);
    return result;
} // PortArgumentDescriptor::clone

BaseArgumentDescriptor *
PortArgumentDescriptor::parseArgString(const YarpString & inString)
{
    ODL_ENTER(); //####
    ODL_S1s("inString = ", inString); //####
    BaseArgumentDescriptor * result = NULL;
    YarpStringVector         inVector;

    if (partitionString(inString, 4, inVector))
    {
        ArgumentMode argMode;
        bool         okSoFar = true;
        bool         isSystemPort = false;
        int          defaultValue;
        YarpString   name(inVector[0]);
        YarpString   typeTag(inVector[1]);
        YarpString   modeString(inVector[2]);
        YarpString   portClass(inVector[3]);
        YarpString   defaultString(inVector[4]);
        YarpString   description(inVector[5]);

        if (typeTag != "P")
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
            if (portClass == "s")
            {
                isSystemPort = true;
            }
            else if (portClass != "r")
            {
                okSoFar = false;
            }
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
        if (okSoFar)
        {
            result = new PortArgumentDescriptor(name, description, argMode, defaultValue,
                                                isSystemPort);
        }
    }
    ODL_EXIT_P(result); //####
    return result;
} // PortArgumentDescriptor::parseArgString

DEFINE_TOSTRING_(PortArgumentDescriptor)
{
    ODL_OBJENTER(); //####
    YarpString result(prefixFields("P"));

    result += _parameterSeparator + (_isSystemPort ? "s" : "r") + suffixFields(getDefaultValue());
    ODL_OBJEXIT_s(result); //####
    return result;
} // PortArgumentDescriptor::toString

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
