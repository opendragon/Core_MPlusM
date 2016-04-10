//--------------------------------------------------------------------------------------------------
//
//  File:       m+m/m+mAddressArgumentDescriptor.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the minimal functionality required to represent an IP
//              address command-line argument.
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
//  Created:    2015-05-17
//
//--------------------------------------------------------------------------------------------------

#include "m+mAddressArgumentDescriptor.h"

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The definition for the minimal functionality required to represent an IP address
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

AddressArgumentDescriptor::AddressArgumentDescriptor(const YarpString & argName,
                                                     const YarpString & argDescription,
                                                     const ArgumentMode argMode,
                                                     const YarpString & defaultValue,
                                                     struct in_addr *   addrBuff) :
    inherited(argName, argDescription, argMode, defaultValue), _addrBuff(addrBuff)
{
    ODL_ENTER(); //####
    ODL_S3s("argName = ", argName, "argDescription = ", argDescription, "defaultValue = ", //####
               defaultValue); //####
    ODL_P1("addrBuff = ", addrBuff); //####
    if (_defaultValue == SELF_ADDRESS_NAME_)
    {
        _defaultValue = SELF_ADDRESS_IPADDR_;
    }
    ODL_EXIT_P(this); //####
} // AddressArgumentDescriptor::AddressArgumentDescriptor

AddressArgumentDescriptor::~AddressArgumentDescriptor(void)
{
    ODL_OBJENTER(); //####
    ODL_OBJEXIT(); //####
} // AddressArgumentDescriptor::~AddressArgumentDescriptor

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

BaseArgumentDescriptor *
AddressArgumentDescriptor::clone(void)
{
    ODL_OBJENTER(); //####
    BaseArgumentDescriptor * result = new AddressArgumentDescriptor(argumentName(),
                                                                    argumentDescription(),
                                                                    argumentMode(),
                                                                    getDefaultValue());

    ODL_EXIT_P(result);
    return result;
} // AddressArgumentDescriptor::clone

BaseArgumentDescriptor *
AddressArgumentDescriptor::parseArgString(const YarpString & inString)
{
    ODL_ENTER(); //####
    ODL_S1s("inString = ", inString); //####
    BaseArgumentDescriptor * result = NULL;
    YarpStringVector         inVector;

    if (partitionString(inString, 3, inVector))
    {
        ArgumentMode argMode = kArgModeRequired;
        bool         okSoFar = true;
        YarpString   name(inVector[0]);
        YarpString   typeTag(inVector[1]);
        YarpString   modeString(inVector[2]);
        YarpString   defaultString(inVector[3]);
        YarpString   description(inVector[4]);

        if (typeTag != "A")
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
            struct in_addr addrBuff;

            if (defaultString == SELF_ADDRESS_NAME_)
            {
                defaultString = SELF_ADDRESS_IPADDR_;
            }
#if MAC_OR_LINUX_
            okSoFar = (0 < inet_pton(AF_INET, defaultString.c_str(), &addrBuff));
#else // ! MAC_OR_LINUX_
            okSoFar = (0 < InetPton(AF_INET, defaultString.c_str(), &addrBuff));
#endif // ! MAC_OR_LINUX_
        }
        if (okSoFar)
        {
            result = new AddressArgumentDescriptor(name, description, argMode, defaultString);
        }
    }
    ODL_EXIT_P(result); //####
    return result;
} // AddressArgumentDescriptor::parseArgString

YarpString
AddressArgumentDescriptor::toString(void)
{
    ODL_OBJENTER(); //####
    YarpString result(prefixFields("A"));

    result += suffixFields(getDefaultValue());
    ODL_OBJEXIT_s(result); //####
    return result;
} // AddressArgumentDescriptor::toString

bool
AddressArgumentDescriptor::validate(const YarpString & value)
{
    ODL_OBJENTER(); //####
    YarpString testValue;

    if (value == SELF_ADDRESS_NAME_)
    {
        testValue = SELF_ADDRESS_IPADDR_;
    }
    else
    {
        testValue = value;
    }
    if (_addrBuff)
    {
#if MAC_OR_LINUX_
        _valid = (0 < inet_pton(AF_INET, testValue.c_str(), _addrBuff));
#else // ! MAC_OR_LINUX_
        _valid = (0 < InetPton(AF_INET, testValue.c_str(), _addrBuff));
#endif // ! MAC_OR_LINUX_
        ODL_B1("_valid <- ", _valid); //####
    }
    else
    {
        struct in_addr addrBuff;

#if MAC_OR_LINUX_
        _valid = (0 < inet_pton(AF_INET, testValue.c_str(), &addrBuff));
#else // ! MAC_OR_LINUX_
        _valid = (0 < InetPton(AF_INET, testValue.c_str(), &addrBuff));
#endif // ! MAC_OR_LINUX_
        ODL_B1("_valid <- ", _valid); //####
    }
    if (_valid)
    {
        _currentValue = testValue;
        ODL_S1s("_currentValue <- ", _currentValue); //####
    }
    ODL_OBJEXIT_B(_valid); //####
    return _valid;
} // AddressArgumentDescriptor::validate

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
