//--------------------------------------------------------------------------------------------------
//
//  File:       mpm/M+MMatchFieldName.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for a pattern matcher for field names.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2014 by HPlus Technologies Ltd. and Simon Fraser University.
//
//              All rights reserved. Redistribution and use in source and binary forms, with or
//              without modification, are permitted provided that the following conditions are met:
//                * Redistributions of source code must retain the above copyright notice, this list
//                  of conditions and the following disclaimer.
//                * Redistributions in binary form must reproduce the above copyright notice, this
//                  list of conditions and the following disclaimer in the documentation and/or
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
//  Created:    2014-03-07
//
//--------------------------------------------------------------------------------------------------

#include <mpm/M+MMatchFieldName.h>
#include <mpm/M+MBaseNameValidator.h>
#include <mpm/M+MMatchValueList.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for a pattern matcher for field names. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Parser;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief The colon character, which is an optional field name terminating character. */
static const char kColon = ':';

/*! @brief The exclamation mark character, which is a field name terminating character which
 signifies negation. */
static const char kExclamationMark = '!';

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

MatchFieldName * MatchFieldName::CreateMatcher(const yarp::os::ConstString & inString,
                                               const size_t                  inLength,
                                               const size_t                  startPos,
                                               size_t &                      endPos,
                                               BaseNameValidator *           validator)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("inString = ", inString); //####
    OD_LOG_LL2("inLength = ", inLength, "startPos = ", startPos); //####
    MatchFieldName * result = NULL;
    
    try
    {
        size_t workPos = SkipWhitespace(inString, inLength, startPos);
        
        if (workPos < inLength)
        {
            // Remember where we began.
            char   scanChar = kColon;
            size_t startSubPos = workPos;
            
            for ( ; workPos < inLength; ++workPos)
            {
                scanChar = inString[workPos];
                if (isspace(scanChar) || (kColon == scanChar) || (kExclamationMark == scanChar))
                {
                    break;
                }
                
            }
            if (startSubPos < workPos)
            {
                // We have at least one character in the name.
                size_t nameEndPos = workPos;
                
                if (isspace(scanChar))
                {
                    workPos = SkipWhitespace(inString, inLength, workPos);
                    if (workPos < inLength)
                    {
                        scanChar = inString[workPos];
                    }
                }
                if ((kColon == scanChar) || (kExclamationMark == scanChar))
                {
                    // The name is followed by a separator.
                    if (workPos < inLength)
                    {
                        yarp::os::ConstString tempString(inString.substr(startSubPos,
                                                                         nameEndPos - startSubPos));
                        
                        if (validator)
                        {
                            // If we have a non-empty substring, we need to check if the field is a
                            // known name.
#if MAC_OR_LINUX_
                            char * tempAsChars = strdup(tempString.c_str());
#else // ! MAC_OR_LINUX_
                            char * tempAsChars = _strdup(tempString.c_str());
#endif // ! MAC_OR_LINUX_
                            
                            // Convert the copy of the string to lower-case:
                            for (size_t ii = 0, len = strlen(tempAsChars); ii < len; ++ii)
                            {
                                tempAsChars[ii] = static_cast<char>(tolower(tempAsChars[ii]));
                            }
                            if (validator->checkName(tempAsChars))
                            {
                                result = new MatchFieldName(tempAsChars,
                                                            kExclamationMark == scanChar);
                            }
                            free(tempAsChars);
                        }
                        else
                        {
                            result = new MatchFieldName(tempString, kExclamationMark == scanChar);
                        }
                        if (result)
                        {
                            endPos = workPos + 1;
                        }
                    }
                    else
                    {
                        OD_LOG("! (workPos < inLength)"); //####
                    }
                }
                else
                {
                    OD_LOG("! ((kColon == scanChar) || (kExclamationMark == scanChar))"); //####
                }
            }
            else
            {
                OD_LOG("! (startSubPos < workPos)"); //####
            }
        }
        else
        {
            OD_LOG("! (workPos < inLength)"); //####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_EXIT_P(result); //####
    return result;
} // MatchFieldName::CreateMatcher

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

MatchFieldName::MatchFieldName(const yarp::os::ConstString & inString,
                               const bool                    negationSeen) :
    inherited(), _matchingString(inString), _isNegated(negationSeen)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("inString = ", inString); //####
    OD_LOG_B1("negationSeen = ", negationSeen); //####
    OD_LOG_EXIT_P(this); //####
} // MatchFieldName::MatchFieldName

MatchFieldName::~MatchFieldName(void)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_OBJEXIT(); //####
} // MatchFieldName::~MatchFieldName

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
