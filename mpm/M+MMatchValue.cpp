//--------------------------------------------------------------------------------------------------
//
//  File:       mpm/M+MMatchValue.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for a pattern matcher for simple values.
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

#include <mpm/M+MMatchValue.h>
#include <mpm/M+MMatchConstraint.h>
#include <mpm/M+MMatchExpression.h>
#include <mpm/M+MMatchValueList.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The class definition for a pattern matcher for simple values. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Parser;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
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

MatchValue * MatchValue::CreateMatcher(const yarp::os::ConstString & inString,
                                       const size_t                  inLength,
                                       const size_t                  startPos,
                                       size_t &                      endPos)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("inString = ", inString); //####
    OD_LOG_LL2("inLength = ", inLength, "startPos = ", startPos); //####
    MatchValue * result = nullptr;
    
    try
    {
        size_t workPos = SkipWhitespace(inString, inLength, startPos);
        
        if (workPos < inLength)
        {
            // Remember where we began.
            bool                  escapeNextChar = false;
            char                  delimiter;
            char                  scanChar = inString[workPos];
            const char            constraintSeparator =
                                                    MatchConstraint::ConstraintSeparatorCharacter();
            const char            expressionSeparator =
                                                    MatchExpression::ExpressionSeparatorCharacter();
            const char            listInitiator = MatchValueList::ListInitiatorCharacter();
            const char            listSeparator = MatchValueList::ListSeparatorCharacter();
            const char            listTerminator = MatchValueList::ListTerminatorCharacter();
            yarp::os::ConstString assembled;
            size_t                startSubPos = workPos;
            
            // If we have a quote character, scan for the matching character. If we have an illegal
            // starting character, reject the string.
            if ((kDoubleQuote == scanChar) || (kSingleQuote == scanChar))
            {
                // A delimited string.
                delimiter = scanChar;
                ++startSubPos;
            }
            else if (listInitiator == scanChar)
            {
                // We've seen the start of a list - this is not a singular value!
                workPos = inLength;
                delimiter = '\1';
            }
            else if (kEscapeCharacter == scanChar)
            {
                // The first character needed to be escaped.
                delimiter = '\0';
                escapeNextChar = true;
            }
            else
            {
                // A normal character.
                delimiter = '\0';
                assembled += scanChar;
            }
            for (++workPos; workPos < inLength; ++workPos)
            {
                scanChar = inString[workPos];
                if (escapeNextChar)
                {
                    escapeNextChar = false;
                    // If the escaped character is one that will still need to be escaped when
                    // converted to SQL, retain the escape character.
                    if ((kEscapeCharacter == scanChar) || (kAsterisk == scanChar) ||
                        (kQuestionMark == scanChar))
                    {
                        assembled += kEscapeCharacter;
                    }
                }
                else if (kEscapeCharacter == scanChar)
                {
                    escapeNextChar = true;
                    continue;
                }
                else if (delimiter)
                {
                    if (delimiter == scanChar)
                    {
                        break;
                    }
                }
                else if (isspace(scanChar) || (listSeparator == scanChar) ||
                         (listTerminator == scanChar) ||
                         (constraintSeparator == scanChar) || (expressionSeparator == scanChar))
                {
                    break;
                }
                assembled += scanChar;
            }
            OD_LOG_S1s("assembled = ", assembled); //####
            // If we have a delimiter, then we must match before the end of the input string. If we
            // don't have a delimiter, we can match the rest of the input string.
            if (workPos < (inLength + (delimiter ? 0 : 1)))
            {
                // Either we stopped with a blank or the end of the string, or we saw a matching
                // delimiter before the end.
                if (0 < (workPos - startSubPos))
                {
                    // If we have a non-empty substring, we have success.
                    result = new MatchValue(assembled);
                }
                else
                {
                    OD_LOG("! (0 < (workPos - startSubPos))"); //####
                }
            }
            else
            {
                OD_LOG("! (workPos < (inLength + (delimiter ? 0 : 1)))"); //####
            }
            if (result)
            {
                endPos = (delimiter ? 1 : 0) + workPos;
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
} // MatchValue::CreateMatcher

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

MatchValue::MatchValue(const yarp::os::ConstString & inString) :
    inherited(), _matchingString(inString), _hasSingleQuotes(false), _hasWildcards(false),
    _needsEscaping(false)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("inString = ", inString); //####
    bool   escapeNextChar = false;
    size_t len = inString.length();

    // Check if we have unescaped wildcards.
    for (size_t ii = 0; len > ii; ++ii)
    {
        char walker = inString[ii];
        
        if (! escapeNextChar)
        {
            if (kEscapeCharacter == walker)
            {
                escapeNextChar = true;
            }
            else if ((kAsterisk == walker) || (kQuestionMark == walker))
            {
                // If there are wildcard characters present, flag this.
                _hasWildcards = true;
                OD_LOG_B1("_hasWildcards <- ", _hasWildcards); //####
            }
            else if (kSingleQuote == walker)
            {
                // If there are single quote characters present, flag this.
                _hasSingleQuotes = true;
                OD_LOG_B1("_hasSingleQuotes <- ", _hasSingleQuotes); //####
            }
        }
    }
    // Check for SQL characters that will need to be escaped, if not already escaped.
    if (_hasWildcards)
    {
        escapeNextChar = false;
        // Mark if the string will need escaping.
        for (size_t ii = 0; len > ii; ++ii)
        {
            char walker = inString[ii];
            
            if (! escapeNextChar)
            {
                if (kEscapeCharacter == walker)
                {
                    escapeNextChar = true;
                }
                else
                {
                    // If there are SQL special characters present, flag this as needing to be
                    // escaped.
                    if ((kUnderscore == walker) || (kPercent == walker))
                    {
                        _needsEscaping = true;
                        OD_LOG_B1("_needsEscaping <- ", _needsEscaping); //####
                    }
                }
            }
        }
    }
    OD_LOG_EXIT_P(this); //####
} // MatchValue::MatchValue

MatchValue::~MatchValue(void)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_OBJEXIT(); //####
} // MatchValue::~MatchValue

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

yarp::os::ConstString MatchValue::asSQLString(void)
const
{
    OD_LOG_OBJENTER(); //####
    yarp::os::ConstString converted;
    
    try
    {
        bool escapeNextChar = false;
        
        converted += kSingleQuote;
        if (_hasSingleQuotes || _hasWildcards || _needsEscaping)
        {
            OD_LOG("(_hasSingleQuotes || _hasWildcards || _needsEscaping)"); //####
            bool wasEscaped = false;
            
            for (size_t ii = 0, len = _matchingString.length(); ii < len; ++ii)
            {
                char walker = _matchingString[ii];
                
                // If there are SQL special characters present, escape them.
                if (escapeNextChar)
                {
                    if ((kUnderscore == walker) || (kPercent == walker) || (kSingleQuote == walker))
                    {
                        if (_hasWildcards)
                        {
                            wasEscaped = true;
                            converted += kEscapeCharacter;
                        }
                        else if (kSingleQuote == walker)
                        {
                            converted += kSingleQuote;
                        }
                        converted += walker;
                    }
                    else
                    {
                        converted += walker;
                    }
                }
                else if (kEscapeCharacter == walker)
                {
                    escapeNextChar = true;
                }
                else if ((kUnderscore == walker) || (kPercent == walker))
                {
                    if (_hasWildcards)
                    {
                        wasEscaped = true;
                        converted += kEscapeCharacter;
                    }
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
                else if (kSingleQuote == walker)
                {
                    converted += kSingleQuote;
                    converted += walker;
                }
                else
                {
                    converted += walker;
                }
            }
            if (wasEscaped)
            {
                converted += kSingleQuote;
                converted += " ESCAPE ";
                converted += kSingleQuote;
                converted += kEscapeCharacter;
            }
        }
        else
        {
            // Remove any unneeded escape characters.
            for (size_t ii = 0, len = _matchingString.length(); ii < len; ++ii)
            {
                char walker = _matchingString[ii];
                
                if (kEscapeCharacter != walker)
                {
                    converted += walker;
                }
            }
        }
        converted += kSingleQuote;
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_S(converted.c_str()); //####
    return converted;
} // MatchValue::asSQLString

yarp::os::ConstString MatchValue::asString(void)
const
{
    yarp::os::ConstString converted;
    
    try
    {
        bool   sawDoubleQuote = false;
        bool   sawSingleQuote = false;
        bool   sawWhitespace = false;
        size_t len = _matchingString.length();
        
        // First, check if there are blanks or quotes in the string:
        for (size_t ii = 0; ii < len; ++ii)
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
                // If both quotes are present, use double quotes and escape any double quotes that
                // we find.
                converted += kDoubleQuote;
                for (size_t ii = 0; ii < len; ++ii)
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
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    return converted;
} // MatchValue::asString

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
