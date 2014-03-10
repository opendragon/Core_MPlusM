//
//  YPPMatchValue.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-03-07.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPMatchValue.h"
#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPMatchValueList.h"
#include <cctype>

using namespace YarpPlusPlusParser;

#if defined(__APPLE__)
# pragma mark Private structures and constants
#endif // defined(__APPLE__)

/*! @brief The asterisk character, which is one of our wildcard characters. */
static const char kAsterisk = '*';
/*! @brief The double quote character. */
static const char kDoubleQuote = '"';
/*! @brief The 'escape' character - a backslash. */
static const char kEscapeCharacter = '\\';
/*! @brief The percent character, which is an SQL pattern character. */
static const char kPercent = '%';
/*! @brief The question mark character, which is one of our wildcard characters. */
static const char kQuestionMark = '?';
/*! @brief The single quote character. */
static const char kSingleQuote = '\'';
/*! @brief The underscore character, which is an SQL pattern character. */
static const char kUnderscore = '_';

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

MatchValue * MatchValue::createMatcher(const yarp::os::ConstString & inString,
                                       const int                     inLength,
                                       const int                     startPos,
                                       int &                         endPos)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("inString = ", inString.c_str());//####
    OD_SYSLOG_LL2("inLength = ", inLength, "startPos = ", startPos);//####
    int          workPos = skipWhitespace(inString, inLength, startPos);
    MatchValue * result = NULL;
    
    if (workPos < inLength)
    {
        OD_SYSLOG("(workPos < inLength)");//####
        // Remember where we began.
        char delimiter;
        char listInitiator = MatchValueList::listInitiatorCharacter();
        char listSeparator = MatchValueList::listSeparatorCharacter();
        char listTerminator = MatchValueList::listTerminatorCharacter();
        char scanChar = inString[workPos];
        int  startSubPos = workPos;
        
        // If we have a quote character, scan for the matching character. If we have an illegal starting character,
        // reject the string.
        if ((kDoubleQuote == scanChar) || (kSingleQuote == scanChar))
        {
            OD_SYSLOG("(workPos < inLength)");//####
            delimiter = scanChar;
            ++startSubPos;
        }
        else if (listInitiator == scanChar)
        {
            OD_SYSLOG("(workPos < inLength)");//####
            workPos = inLength;
            delimiter = '\1';
        }
        else
        {
            OD_SYSLOG("(workPos < inLength)");//####
            delimiter = '\0';
        }
        for (++workPos; workPos < inLength; ++workPos)
        {
            scanChar = inString[workPos];
            if (delimiter)
            {
                if (delimiter == scanChar)
                {
                    break;
                }
                
            }
            else if (isspace(scanChar) || (listSeparator == scanChar) || (listTerminator == scanChar))
            {
                break;
            }
            
        }
        // If we have a delimiter, then we must match before the end of the input string. If we don't have a delimiter,
        // we can match the rest of the input string.
        if (workPos < (inLength + (delimiter ? 0 : 1)))
        {
            OD_SYSLOG("(workPos < (inLength + (delimiter ? 0 : 1)))");//####
            // Either we stopped with a blank or the end of the string, or we saw a matching delimiter before the end.
            if (0 < (workPos - startSubPos))
            {
                // If we have a non-empty substring, we have success.
                result = new MatchValue(inString.substr(startSubPos, workPos - startSubPos));
            }
        }
        if (result)
        {
            endPos = (delimiter ? 1 : 0) + workPos;
        }        
    }
    OD_SYSLOG_EXIT_P(result);//####
    return result;
} // MatchValue::createMatcher

char MatchValue::escapeCharacter(void)
{
    return kEscapeCharacter;
} // MatchValue::escapeCharacter

#if defined(__APPLE__)
# pragma mark Constructors and destructors
#endif // defined(__APPLE__)

MatchValue::MatchValue(const yarp::os::ConstString & inString) :
        inherited(), _matchingString(inString), _hasWildcards(false), _needsEscaping(false)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("inString = ", inString.c_str());//####
    // Mark if the string will need escaping.
    for (int ii = 0, len = inString.length(); ii < len; ++ii)
    {
        char walker = inString[ii];
        
        // If there are SQL special characters present, flag this as needing to be escaped.
        if ((kUnderscore == walker) || (kPercent == walker) || (kEscapeCharacter == walker))
        {
            _needsEscaping = true;
        }
        else if ((kAsterisk == walker) || (kQuestionMark == walker))
        {
            // If there are wildcard characters present, flag this.
            _hasWildcards = true;
        }
    }
    OD_SYSLOG_EXIT_P(this);//####
} // MatchValue::MatchValue

MatchValue::~MatchValue(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // MatchValue::~MatchValue

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

yarp::os::ConstString MatchValue::asSQLString(void)
const
{
    OD_SYSLOG_ENTER();//####
    yarp::os::ConstString converted;
    
    if (_hasWildcards || _needsEscaping)
    {
        OD_SYSLOG("(_hasWildcards || _needsEscaping)");//####
        for (int ii = 0, len = _matchingString.length(); ii < len; ++ii)
        {
            char walker = _matchingString[ii];
            
            // If there are SQL special characters present, escape them.
            if ((kUnderscore == walker) || (kPercent == walker) || (kEscapeCharacter == walker))
            {
                converted += kEscapeCharacter;
                converted += walker;
            }
            else if (kAsterisk == walker)
            {
                // Substitute the corresponding SQL wildcard character.
                converted += kPercent;
            }
            else if (kQuestionMark == walker)
            {
                // Substitute the corresponding SQL wildcard character.
                converted += kUnderscore;
            }
            else
            {
                converted += walker;
            }
        }
    }
    else
    {
        converted = _matchingString;
    }
    OD_SYSLOG_EXIT_S(converted.c_str());//####
    return converted;
} // MatchValue::asSQLString

yarp::os::ConstString MatchValue::asString(void)
const
{
    bool                  sawDoubleQuote = false;
    bool                  sawSingleQuote = false;
    bool                  sawWhitespace = false;
    yarp::os::ConstString converted;
    int                   len = _matchingString.length();

    // First, check if there are blanks or quotes in the string:
    for (int ii = 0; ii < len; ++ii)
    {
        char walker = _matchingString[ii];
        
        if (isspace(walker))
        {
            sawWhitespace = true;
        }
        else if (kDoubleQuote == walker)
        {
            sawDoubleQuote = true;
        }
        else if (kSingleQuote == walker)
        {
            sawSingleQuote = true;
        }
    }
    if (sawWhitespace || sawDoubleQuote || sawSingleQuote)
    {
        if (sawDoubleQuote && sawSingleQuote)
        {
            // If both quotes are present, use double quotes and escape any double quotes that we find.
            converted += kDoubleQuote;
            for (int ii = 0; ii < len; ++ii)
            {
                char walker = _matchingString[ii];
                
                if (kDoubleQuote == walker)
                {
                    converted += kEscapeCharacter;
                }
                converted += walker;
            }
            converted += kDoubleQuote;
        }
        else if (sawDoubleQuote)
        {
            // If only one type of quote is present, use the other quote.
            converted += kSingleQuote;
            converted += _matchingString;
            converted += kSingleQuote;
        }
        else if (sawSingleQuote)
        {
            // If only one type of quote is present, use the other quote.
            converted += kDoubleQuote;
            converted += _matchingString;
            converted += kDoubleQuote;
        }
        else
        {
            // If no quotes are present, just use double quotes.
            converted += kDoubleQuote;
            converted += _matchingString;
            converted += kDoubleQuote;
        }
    }
    else
    {
        // If neither blanks nor quotes are in the string, just return the string.
        converted = _matchingString;
    }
    return converted;
} // MatchValue::asString

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
