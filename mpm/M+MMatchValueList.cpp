//--------------------------------------------------------------------------------------------------
//
//  File:       mpm/M+MMatchValueList.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for a pattern matcher for lists of simple values.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2014 by H Plus Technologies Ltd. and Simon Fraser University.
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
//  Created:    2014-03-07
//
//--------------------------------------------------------------------------------------------------

#include "M+MMatchValueList.h"

#include <mpm/M+MMatchValue.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for a pattern matcher for lists of simple values. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Parser;

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
# pragma mark Global constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

MatchValueList * MatchValueList::CreateMatcher(const YarpString & inString,
                                               const size_t       inLength,
                                               const size_t       startPos,
                                               size_t &           endPos)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("inString = ", inString); //####
    OD_LOG_LL2("inLength = ", inLength, "startPos = ", startPos);
    MatchValueList * result = NULL;
    
    try
    {
        size_t workPos = SkipWhitespace(inString, inLength, startPos);
        
        if (workPos < inLength)
        {
            if (kRoundOpenBracket == inString[workPos])
            {
                // We potentially have a value list.
                bool done = false;
                bool okSoFar = true;
                
                result = new MatchValueList;
                for (++workPos; okSoFar && (! done); )
                {
                    size_t       nextElementPos;
                    MatchValue * element = MatchValue::CreateMatcher(inString, inLength, workPos,
                                                                     nextElementPos);
                    
                    if (element)
                    {
                        // Skip over any trailing whitespace, to find if the value list is complete
                        // or more coming.
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
                                OD_LOG("! (kComma == scanChar)"); //####
                                // Something unexpected has appeared.
                                okSoFar = false;
                            }
                        }
                        else
                        {
                            OD_LOG("! (workPos < inLength)"); //####
                            // We've gone past the end of the string without seeing a terminator or
                            // a separator.
                            okSoFar = false;
                        }
                    }
                    else
                    {
                        OD_LOG("! (element)"); //####
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
                OD_LOG("! (kRoundOpenBracket == inString[workPos])"); //####
            }
        }
        else
        {
            OD_LOG("! (0 < (workPos < inLength))"); //####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_EXIT_P(result); //####
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
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

MatchValueList::MatchValueList(void) :
inherited(), _values()
{
    OD_LOG_ENTER(); //####
    OD_LOG_EXIT_P(this); //####
} // MatchValueList::MatchValueList

MatchValueList::~MatchValueList(void)
{
    OD_LOG_OBJENTER(); //####
    empty();
    OD_LOG_OBJEXIT(); //####
} // MatchValueList::~MatchValueList

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

YarpString MatchValueList::asSQLString(const char * fieldName,
                                       const bool   negated)
const
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_S1("fieldName = ", fieldName); //####
    YarpString result;
    
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
                    result += (negated ? " != " : " = ");
                    result += _values[0]->asSQLString();
                }
                else
                {
                    result += (negated ? " NOT IN (" : " IN (");
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
                            result += (negated ? " != " : " = ");
                            result += _values[ii]->asSQLString();
                            break;
                        }
                        
                    }
                }
                else if (nonWild)
                {
                    // Gather the non-wildcard values together.
                    result += (negated ? " NOT IN (" : " IN (");
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
                    result += (negated ? " AND " : " OR ");
                    result += fieldName;
                }
                for (MatchValueListSize ii = 0, maxI = _values.size(), jj = 0; ii < maxI; ++ii)
                {
                    MatchValue * element = _values[ii];
                    
                    if (element->hasWildcardCharacters())
                    {
                        if (jj)
                        {
                            result += (negated ? " AND " : " OR ");
                            result += fieldName;
                        }
                        result += (negated ? " NOT LIKE " : " LIKE ");
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
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_S(result.c_str()); //####
    return result;
} // MatchValueList::asSQLString

const YarpString MatchValueList::asString(void)
const
{
    YarpString result;
    
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
        OD_LOG("Exception caught"); //####
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
            OD_LOG("! ((index >= 0) && (index < static_cast<int>(_values.size())))"); //####
            result = NULL;
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    return result;
} // MatchValueList::element

void MatchValueList::empty(void)
{
    OD_LOG_OBJENTER(); //####
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
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT(); //####
} // MatchValueList::empty

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
