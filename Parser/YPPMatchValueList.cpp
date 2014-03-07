//
//  YPPMatchValueList.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-03-07.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPMatchValueList.h"
#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPMatchValue.h"
#include <cctype>

using namespace YarpPlusPlusParser;

#if defined(__APPLE__)
# pragma mark Private structures and constants
#endif // defined(__APPLE__)

/*! @brief The character used to separate list elements. */
static const char kComma = ',';
/*! @brief The character used to end a list. */
static const char kRoundCloseBracket = ')';
/*! @brief The character used to start a list. */
static const char kRoundOpenBracket = '(';

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

MatchValueList * MatchValueList::createMatcher(const yarp::os::ConstString & inString,
                                               const int                     startPos,
                                               int &                         endPos)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("inString = ", inString.c_str());//####
    OD_SYSLOG_LL1("startPos = ", startPos);
    char             scanChar = '\0';
    int              workPos = startPos;
    int              length = inString.length();
    MatchValueList * result = NULL;
    
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
        if (kRoundOpenBracket == scanChar)
        {
            // We potentially have a list.
            bool okSoFar = true;
            bool done = false;
            
            result = new MatchValueList();
            for (++workPos; okSoFar && (! done); )
            {
                int          nextElementPos;
                MatchValue * element = MatchValue::createMatcher(inString, true, workPos, nextElementPos);
                
                if (element)
                {
                    // Skip over any trailing whitespace, to find if the list is complete or more coming.
                    for (workPos = nextElementPos; workPos < length; ++workPos)
                    {
                        scanChar = inString[workPos];
                        if (! isspace(scanChar))
                        {
                            break;
                        }
                        
                    }
                    if (workPos < length)
                    {
                        if (kRoundCloseBracket == scanChar)
                        {
                            // We've seen the end of the list.
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
                            // Something unexpected has appeared.
                            okSoFar = false;
                        }
                    }
                    else
                    {
                        // We've gone past the end of the string without seeing a terminator or a separator.
                        okSoFar = false;
                    }
                }
                else
                {
                    // We have a malformed list.
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
    }
    OD_SYSLOG_EXIT_P(result);//####
    return result;
} // MatchValueList::createMatcher

char MatchValueList::listInitiatorCharacter(void)
{
    return kRoundOpenBracket;
} // MatchValueList::listInitiatorCharacter

char MatchValueList::listSeparatorCharacter(void)
{
    return kComma;
} // MatchValueList::listSeparatorCharacter

char MatchValueList::listTerminatorCharacter(void)
{
    return kRoundCloseBracket;
} // MatchValueList::listTerminatorCharacter

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
    empty();
    OD_SYSLOG_EXIT();//####
} // MatchValueList::~MatchValueList

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

const yarp::os::ConstString MatchValueList::asString(void)
const
{
    yarp::os::ConstString result;
    
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
    MatchValue * result;
    
    if ((index >= 0) && (index < static_cast<int>(_values.size())))
    {
        result = _values[static_cast<MatchValueListSize>(index)];
    }
    else
    {
        result = NULL;
    }
    return result;
} // MatchValueList::element

void MatchValueList::empty(void)
{
    OD_SYSLOG_ENTER();//####
    for (MatchValueListSize ii = 0, maxI = _values.size(); ii < maxI; ++ii)
    {
        MatchValue * element = _values[ii];
        
        delete element;
    }
    _values.clear();
    OD_SYSLOG_EXIT();//####
} // MatchValueList::empty

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
