//
//  YPPMatchFieldName.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-03-07.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPMatchFieldName.h"
#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include <cctype>
#include <cstdlib>
#include <cstring>

using namespace YarpPlusPlusParser;

#if defined(__APPLE__)
# pragma mark Private structures and constants
#endif // defined(__APPLE__)

/*! @brief The colon character, which is an optional field name terminating character. */
static const char kColon = ':';

/*! @brief the valid field names that may be used. Note that the strings are all lower-case for comparison purposes. */
static const char * kFieldNames[] =
{
    "description",
    "input",
    "keyword",
    "output",
    "portname",
    "request",
    "version"
};

/*! @brief The number of valid field names. */
static const size_t kFieldNamesCount = (sizeof(kFieldNames) / sizeof(*kFieldNames));

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

MatchFieldName * MatchFieldName::createMatcher(const yarp::os::ConstString & inString,
                                               const int                     startPos,
                                               int &                         endPos)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("inString = ", inString.c_str());//####
    OD_SYSLOG_LL1("startPos = ", startPos);//####
    char             scanChar = '\0';
    int              workPos = startPos;
    int              length = inString.length();
    MatchFieldName * result = NULL;
    
    // Skip whitespace
    for ( ; workPos < length; ++workPos)
    {
        scanChar = inString[workPos];
        if (! isspace(scanChar))
        {
            break;
        }
        
    }
    if (workPos < length)
    {
        // Remember where we began.
        int startSubPos = workPos;
        
        for (++workPos; workPos < length; ++workPos)
        {
            scanChar = inString[workPos];
            if (isspace(scanChar))
            {
                break;
            }
            
            if (kColon == scanChar)
            {
                break;
            }
       
        }
        if (workPos < length)
        {
            // Either we stopped with a blank, a colon or the end of the string.
            if (0 < (workPos - startSubPos))
            {
                // If we have a non-empty substring, we just need to check if the field is a know name.
                bool                  isOK = false;
                yarp::os::ConstString tempString((inString.substr(startSubPos, workPos - startSubPos)));
                char *                tempAsChars = strdup(tempString.c_str());
                
                // Convert the copy of the string to lower-case:
                for (size_t ii = 0, len = strlen(tempAsChars); ii < len; ++ii)
                {
                    tempAsChars[ii] = static_cast<char>(tolower(tempAsChars[ii]));
                }
                for (size_t ii = 0; ii < kFieldNamesCount; ++ii)
                {
                    if (! strcmp(tempAsChars, kFieldNames[ii]))
                    {
                        isOK = true;
                        break;
                    }
                    
                }
                if (isOK)
                {
                    result = new MatchFieldName(tempString);
                }
                free(tempAsChars);
            }
        }
        if (result)
        {
            endPos = ((kColon == scanChar) ? 1 : 0) + workPos;
        }        
    }
    OD_SYSLOG_EXIT_P(result);//####
    return result;
} // MatchFieldName::createMatcher

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
