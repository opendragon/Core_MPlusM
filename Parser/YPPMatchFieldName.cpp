//--------------------------------------------------------------------------------------
//
//  File:       YPPMatchFieldName.cpp
//
//  Project:    YarpPlusPlus
//
//  Contains:   The class definition for a pattern matcher for field names.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2014 by OpenDragon.
//
//              All rights reserved. Redistribution and use in source and binary forms,
//              with or without modification, are permitted provided that the following
//              conditions are met:
//                * Redistributions of source code must retain the above copyright
//                  notice, this list of conditions and the following disclaimer.
//                * Redistributions in binary form must reproduce the above copyright
//                  notice, this list of conditions and the following disclaimer in the
//                  documentation and/or other materials provided with the
//                  distribution.
//                * Neither the name of the copyright holders nor the names of its
//                  contributors may be used to endorse or promote products derived
//                  from this software without specific prior written permission.
//
//              THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
//              "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
//              LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
//              PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
//              OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
//              SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
//              LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
//              DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
//              THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
//              (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
//              OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
//  Created:    2014-03-07
//
//--------------------------------------------------------------------------------------

#include "YPPMatchFieldName.h"
//#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPMatchValueList.h"
#include <cctype>
#include <cstdlib>
#include <cstring>

using namespace YarpPlusPlusParser;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief The colon character, which is an optional field name terminating character. */
static const char kColon = ':';

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

MatchFieldName * MatchFieldName::CreateMatcher(const yarp::os::ConstString & inString,
                                               const int                     inLength,
                                               const int                     startPos,
                                               int &                         endPos,
                                               FieldNameValidator            validator)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("inString = ", inString.c_str());//####
    OD_SYSLOG_LL2("inLength = ", inLength, "startPos = ", startPos);//####
    int              workPos = SkipWhitespace(inString, inLength, startPos);
    MatchFieldName * result = NULL;
    
    if (workPos < inLength)
    {
        // Remember where we began.
        char listStart = MatchValueList::ListInitiatorCharacter();
        char scanChar = inString[workPos];
        int  startSubPos = workPos;
        
        for (++workPos; workPos < inLength; ++workPos)
        {
            scanChar = inString[workPos];
            if (isspace(scanChar) || (kColon == scanChar) || (listStart == scanChar))
            {
                break;
            }
            
        }
        if (workPos < inLength)
        {
            // Either we stopped with a blank, a colon, a list beginning or the end of the string.
            if (0 < (workPos - startSubPos))
            {
                yarp::os::ConstString tempString(inString.substr(startSubPos, workPos - startSubPos));

                if (validator)
                {
                    // If we have a non-empty substring, we need to check if the field is a known name.
                    char * tempAsChars = strdup(tempString.c_str());
                    
                    // Convert the copy of the string to lower-case:
                    for (size_t ii = 0, len = strlen(tempAsChars); ii < len; ++ii)
                    {
                        tempAsChars[ii] = static_cast<char>(tolower(tempAsChars[ii]));
                    }
                    if (validator(tempAsChars, NULL, NULL))
                    {
                        result = new MatchFieldName(tempAsChars);
                    }
                    free(tempAsChars);
                }
                else
                {
                    result = new MatchFieldName(tempString);
                }
            }
        }
        if (result)
        {
            endPos = ((kColon == scanChar) ? 1 : 0) + workPos;
        }        
    }
    OD_SYSLOG_EXIT_P(result);//####
    return result;
} // MatchFieldName::CreateMatcher

#if defined(__APPLE__)
# pragma mark Constructors and destructors
#endif // defined(__APPLE__)

MatchFieldName::MatchFieldName(const yarp::os::ConstString & inString) :
        inherited(), _matchingString(inString)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("inString = ", inString.c_str());//####
    OD_SYSLOG_EXIT_P(this);//####
} // MatchFieldName::MatchFieldName

MatchFieldName::~MatchFieldName(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // MatchFieldName::~MatchFieldName

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
