//--------------------------------------------------------------------------------------
//
//  File:       YPPMatchValueList.cpp
//
//  Project:    YarpPlusPlus
//
//  Contains:   The class definition for a pattern matcher for lists of simple values.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2014 by HPlus Technologies Ltd. and Simon Fraser University.
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

#include "YPPMatchValueList.h"
//#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPMatchValue.h"
#include <cctype>

using namespace YarpPlusPlusParser;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief The character used to separate value list elements. */
static const char kComma = ',';
/*! @brief The character used to end a value list. */
static const char kRoundCloseBracket = ')';
/*! @brief The character used to start a value list. */
static const char kRoundOpenBracket = '(';

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

MatchValueList * MatchValueList::CreateMatcher(const yarp::os::ConstString & inString,
                                               const int                     inLength,
                                               const int                     startPos,
                                               int &                         endPos)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("inString = ", inString.c_str());//####
    OD_SYSLOG_LL2("inLength = ", inLength, "startPos = ", startPos);
    MatchValueList * result = NULL;
    
    try
    {
        int workPos = SkipWhitespace(inString, inLength, startPos);

        if (workPos < inLength)
        {
            if (kRoundOpenBracket == inString[workPos])
            {
                // We potentially have a value list.
                bool done = false;
                bool okSoFar = true;
                
                result = new MatchValueList();
                for (++workPos; okSoFar && (! done); )
                {
                    int          nextElementPos;
                    MatchValue * element = MatchValue::CreateMatcher(inString, inLength, workPos, nextElementPos);
                    
                    if (element)
                    {
                        // Skip over any trailing whitespace, to find if the value list is complete or more coming.
                        workPos = SkipWhitespace(inString, inLength, nextElementPos);
                        if (workPos < inLength)
                        {
                            char scanChar = inString[workPos];
                            
                            if (kRoundCloseBracket == scanChar)
                            {
                                // We've seen the end of the value list.
                                result->_values.push_back(element);
                                ++workPos;
                                done = true;
                            }
                            else if (kComma == scanChar)
                            {
                                // We've got more elements to go.
                                result->_values.push_back(element);
                                ++workPos;
                            }
                            else
                            {
                                OD_SYSLOG("! (kComma == scanChar)");//####
                                // Something unexpected has appeared.
                                okSoFar = false;
                            }
                        }
                        else
                        {
                            OD_SYSLOG("! (workPos < inLength)");//####
                            // We've gone past the end of the string without seeing a terminator or a separator.
                            okSoFar = false;
                        }
                    }
                    else
                    {
                        OD_SYSLOG("! (element)");//####
                                                 // We have a malformed value list.
                        okSoFar = false;
                    }
                }
                if (okSoFar)
                {
                    endPos = workPos;
                }
                else
                {
                    delete result;
                    result = NULL;
                }
            }
            else
            {
                OD_SYSLOG("! (kRoundOpenBracket == inString[workPos])");//####
            }
        }
        else
        {
            OD_SYSLOG("! (0 < (workPos < inLength))");//####
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT_P(result);//####
    return result;
} // MatchValueList::CreateMatcher

char MatchValueList::ListInitiatorCharacter(void)
{
    return kRoundOpenBracket;
} // MatchValueList::ListInitiatorCharacter

char MatchValueList::ListSeparatorCharacter(void)
{
    return kComma;
} // MatchValueList::ListSeparatorCharacter

char MatchValueList::ListTerminatorCharacter(void)
{
    return kRoundCloseBracket;
} // MatchValueList::ListTerminatorCharacter

#if defined(__APPLE__)
# pragma mark Constructors and destructors
#endif // defined(__APPLE__)

MatchValueList::MatchValueList(void) :
        inherited(), _values()
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT_P(this);//####
} // MatchValueList::MatchValueList

MatchValueList::~MatchValueList(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P1("this = ", this);//####
    empty();
    OD_SYSLOG_EXIT();//####
} // MatchValueList::~MatchValueList

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

yarp::os::ConstString MatchValueList::asSQLString(const char * fieldName)
const
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P1("this = ", this);//####
    OD_SYSLOG_S1("fieldName = ", fieldName);//####
    yarp::os::ConstString result;
    
    try
    {
        if (0 < _values.size())
        {
            bool simpleForm = true;
            int  nonWild = 0;
            
            // Check if none of the values contain wildcards.
            for (MatchValueListSize ii = 0, maxI = _values.size(); ii < maxI; ++ii)
            {
                MatchValue * element = _values[ii];
                
                if (element->hasWildcardCharacters())
                {
                    simpleForm = false;
                }
                else
                {
                    ++nonWild;
                }
            }
            if (simpleForm)
            {
                result += fieldName;
                if (1 == _values.size())
                {
                    result += " = ";
                    result += _values[0]->asSQLString();
                }
                else
                {
                    result += " IN (";
                    for (MatchValueListSize ii = 0, maxI = _values.size(); ii < maxI; ++ii)
                    {
                        MatchValue * element = _values[ii];
                        
                        if (ii)
                        {
                            result += kComma;
                            result += " ";
                        }
                        result += element->asSQLString();
                    }
                    result += ")";
                }
            }
            else
            {
                if (nonWild)
                {
                    result += kRoundOpenBracket;
                }
                result += fieldName;
                if (1 == nonWild)
                {
                    // Find the single non-wildcard value.
                    for (MatchValueListSize ii = 0, maxI = _values.size(); ii < maxI; ++ii)
                    {
                        MatchValue * element = _values[ii];
                        
                        if (! element->hasWildcardCharacters())
                        {
                            result += " = ";
                            result += _values[ii]->asSQLString();
                            break;
                        }
                        
                    }
                }
                else if (nonWild)
                {
                    // Gather the non-wildcard values together.
                    result += " IN (";
                    for (MatchValueListSize ii = 0, maxI = _values.size(), jj = 0; ii < maxI; ++ii)
                    {
                        MatchValue * element = _values[ii];
                        
                        if (! element->hasWildcardCharacters())
                        {
                            if (jj)
                            {
                                result += kComma;
                                result += " ";
                            }
                            result += element->asSQLString();
                            ++jj;
                        }
                    }
                    result += ")";
                }
                // Add the wildcard values
                if (nonWild)
                {
                    result += " OR ";
                    result += fieldName;
                }
                for (MatchValueListSize ii = 0, maxI = _values.size(), jj = 0; ii < maxI; ++ii)
                {
                    MatchValue * element = _values[ii];
                    
                    if (element->hasWildcardCharacters())
                    {
                        if (jj)
                        {
                            result += " OR ";
                            result += fieldName;
                        }
                        result += " LIKE ";
                        result += element->asSQLString();
                        ++jj;
                    }
                }
                if (nonWild)
                {
                    result += kRoundCloseBracket;
                }
            }
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT_S(result.c_str());//####
    return result;
} // MatchValueList::asSQLString

const yarp::os::ConstString MatchValueList::asString(void)
const
{
    yarp::os::ConstString result;
    
    try
    {
        result += kRoundOpenBracket;
        for (MatchValueListSize ii = 0, maxI = _values.size(); ii < maxI; ++ii)
        {
            MatchValue * element = _values[ii];
            
            if (ii)
            {
                result += kComma;
                result += " ";
            }
            result += element->asString();
        }
        result += kRoundCloseBracket;
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    return result;
} // MatchValueList::asString

int MatchValueList::count(void)
const
{
    return static_cast<int>(_values.size());
} // MatchValueList::count

const MatchValue * MatchValueList::element(const int index)
const
{
    MatchValue * result = NULL;
    
    try
    {
        if ((index >= 0) && (index < static_cast<int>(_values.size())))
        {
            result = _values[static_cast<MatchValueListSize>(index)];
        }
        else
        {
            OD_SYSLOG("! ((index >= 0) && (index < static_cast<int>(_values.size())))");//####
            result = NULL;
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    return result;
} // MatchValueList::element

void MatchValueList::empty(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P1("this = ", this);//####
    try
    {
        for (MatchValueListSize ii = 0, maxI = _values.size(); ii < maxI; ++ii)
        {
            MatchValue * element = _values[ii];
            
            delete element;
        }
        _values.clear();
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT();//####
} // MatchValueList::empty

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
