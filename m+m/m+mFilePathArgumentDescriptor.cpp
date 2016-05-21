//--------------------------------------------------------------------------------------------------
//
//  File:       m+m/m+mFilePathArgumentDescriptor.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the minimal functionality required to represent a
//              filepath-type command-line argument.
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

#include "m+mFilePathArgumentDescriptor.hpp"

#include <m+m/m+mUtilities.hpp>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if (! MAC_OR_LINUX_)
# include <io.h>
#endif // ! MAC_OR_LINUX_

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The definition for the minimal functionality required to represent a filepath-type
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

/*! @brief Check if a file path is acceptable.
 @param[in] thePath The file path to be checked.
 @param[in] forOutput @c true if the file is to be written to and @c false otherwise.
 @param[in] emptyIsOK @c true if the file path can be empty and @c false otherwise.
 @returns @c true if the file path is acceptable and @c false otherwise. */
static bool
checkFilePath(const char * thePath,
              const bool   forOutput,
              const bool   emptyIsOK)
{
    ODL_ENTER(); //####
    ODL_S1("thePath = ", thePath); //####
    ODL_B1("forOutput = ", forOutput); //####
    bool okSoFar;

    if (forOutput)
    {
        YarpString dirPath(thePath);
        size_t     lastDelim = dirPath.rfind(kDirectorySeparator[0]);

        if (YarpString::npos == lastDelim)
        {
#if MAC_OR_LINUX_
            okSoFar = (0 == access("..", W_OK));
#else // ! MAC_OR_LINUX_
            okSoFar = (0 == _access("..", 2));
#endif // ! MAC_OR_LINUX_
        }
        else
        {
            dirPath = dirPath.substr(0, lastDelim);
#if MAC_OR_LINUX_
            okSoFar = (0 == access(dirPath.c_str(), W_OK));
#else // ! MAC_OR_LINUX_
            okSoFar = (0 == _access(dirPath.c_str(), 2));
#endif // ! MAC_OR_LINUX_
        }
    }
    else if (0 < strlen(thePath))
    {
        // The file must exist and be readable.
#if MAC_OR_LINUX_
        okSoFar = (0 == access(thePath, R_OK));
#else // ! MAC_OR_LINUX_
        okSoFar = (0 == _access(thePath, 4));
#endif // ! MAC_OR_LINUX_
    }
    else
    {
        okSoFar = emptyIsOK;
    }
    ODL_EXIT_B(okSoFar); //####
    return okSoFar;
} // checkFilePath

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

FilePathArgumentDescriptor::FilePathArgumentDescriptor(const YarpString & argName,
                                                       const YarpString & argDescription,
                                                       const ArgumentMode argMode,
                                                       const YarpString & pathPrefix,
                                                       const YarpString & pathSuffix,
                                                       const bool         forOutput,
                                                       const bool         useRandomPath) :
    inherited(argName, argDescription, argMode, pathPrefix),
    _pathPrefix(pathPrefix), _pathSuffix(pathSuffix), _defaultSet(false), _forOutput(forOutput),
    _useRandomPath(useRandomPath)
{
    ODL_ENTER(); //####
    ODL_S4s("argName = ", argName, "argDescription = ", argDescription, "pathPrefix = ", //####
            pathPrefix, "pathSuffix = ", pathSuffix); //####
    ODL_B2("forOutput = ", forOutput, "useRandomPath = ", useRandomPath); //####
    getDefaultValue();
    ODL_EXIT_P(this); //####
} // FilePathArgumentDescriptor::FilePathArgumentDescriptor

FilePathArgumentDescriptor::~FilePathArgumentDescriptor(void)
{
    ODL_OBJENTER(); //####
    ODL_OBJEXIT(); //####
} // FilePathArgumentDescriptor::~FilePathArgumentDescriptor

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

BaseArgumentDescriptor *
FilePathArgumentDescriptor::clone(void)
{
    ODL_OBJENTER(); //####
    BaseArgumentDescriptor * result = new FilePathArgumentDescriptor(argumentName(),
                                                                     argumentDescription(),
                                                                     argumentMode(), _pathPrefix,
                                                                     _pathSuffix, _forOutput,
                                                                     _useRandomPath);

    ODL_EXIT_P(result);
    return result;
} // FilePathArgumentDescriptor::clone

YarpString
FilePathArgumentDescriptor::getDefaultValue(void)
{
    ODL_OBJENTER(); //####
    _defaultValue = _pathPrefix;
    ODL_S1s("_defaultValue <- ", _defaultValue); //####
    if (_useRandomPath)
    {
        _defaultValue += Utilities::GetRandomHexString();
        ODL_S1s("_defaultValue <- ", _defaultValue); //####
    }
    _defaultValue += _pathSuffix;
    ODL_S1s("_defaultValue <- ", _defaultValue); //####
    _defaultSet = true;
    ODL_B1("_defaultSet <- ", _defaultSet); //####
    ODL_OBJEXIT_s(_defaultValue); //####
    return _defaultValue;
} // FilePathArgumentDescriptor::getDefaultValue

BaseArgumentDescriptor *
FilePathArgumentDescriptor::parseArgString(const YarpString & inString)
{
    ODL_ENTER(); //####
    ODL_S1s("inString = ", inString); //####
    BaseArgumentDescriptor * result = NULL;
    YarpStringVector         inVector;

    if (partitionString(inString, 6, inVector))
    {
        ArgumentMode argMode;
        bool         forOutput = false;
        bool         okSoFar = true;
        bool         usesRandom = false;
        YarpString   name(inVector[0]);
        YarpString   typeTag(inVector[1]);
        YarpString   modeString(inVector[2]);
        YarpString   direction(inVector[3]);
        YarpString   suffixValue(inVector[4]);
        YarpString   randomFlag(inVector[5]);
        YarpString   defaultString(inVector[6]);
        YarpString   description(inVector[7]);

        if (typeTag != "F")
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
            if (direction == "o")
            {
                forOutput = true;
            }
            else if (direction != "i")
            {
                okSoFar = false;
            }
        }
        if (okSoFar)
        {
            if (randomFlag == "1")
            {
                usesRandom = true;
            }
            else if (randomFlag != "0")
            {
                okSoFar = false;
            }
        }
        if (okSoFar)
        {
            YarpString tempString(defaultString);

            if (usesRandom)
            {
                tempString += Utilities::GetRandomHexString();
            }
            tempString += suffixValue;
            okSoFar = checkFilePath(tempString.c_str(), forOutput, ! (argMode & kArgModeOptional));
        }
        if (okSoFar)
        {
            result = new FilePathArgumentDescriptor(name, description, argMode, defaultString,
                                                    suffixValue, forOutput, usesRandom);
        }
    }
    ODL_EXIT_P(result); //####
    return result;
} // FilePathArgumentDescriptor::parseArgString

void
FilePathArgumentDescriptor::setToDefaultValue(void)
{
    ODL_OBJENTER(); //####
    if (! _defaultSet)
    {
        getDefaultValue();
    }
    _currentValue = _defaultValue;
    ODL_S1s("_currentValue <- ", _currentValue); //####
    ODL_OBJEXIT(); //####
} // FilePathArgumentDescriptor::setToDefaultValue

YarpString
FilePathArgumentDescriptor::toString(void)
{
    ODL_OBJENTER(); //####
    YarpString oldDefault(_defaultValue);
    YarpString result(prefixFields("F"));

    // Temporarily change the default value to the prefix value, as that's how we pass the path
    // prefix to the outside world.
    result += _parameterSeparator + (_forOutput ? "o" : "i") + _parameterSeparator + _pathSuffix +
                _parameterSeparator + (_useRandomPath ? "1" : "0") + suffixFields(_pathPrefix);
    ODL_OBJEXIT_s(result); //####
    return result;
} // FilePathArgumentDescriptor::toString

bool
FilePathArgumentDescriptor::validate(const YarpString & value)
{
    ODL_OBJENTER(); //####
    _valid = checkFilePath(value.c_str(), _forOutput, false);
    ODL_B1("_valid <- ", _valid); //####
    if (_valid)
    {
        _currentValue = value;
        ODL_S1s("_currentValue <- ", _currentValue); //####
    }
    ODL_OBJEXIT_B(_valid); //####
    return _valid;
} // FilePathArgumentDescriptor::validate

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
