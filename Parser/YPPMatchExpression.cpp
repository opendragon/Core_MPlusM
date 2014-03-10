//
//  YPPMatchExpression.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-03-10.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPMatchExpression.h"
//#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPMatchConstraint.h"

using namespace YarpPlusPlusParser;

#if defined(__APPLE__)
# pragma mark Private structures and constants
#endif // defined(__APPLE__)

/*! @brief The character used to separate expression list elements. */
static const char kComma = ',';

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

MatchExpression * MatchExpression::createMatcher(const yarp::os::ConstString & inString,
                                                 const int                     inLength,
                                                 const int                     startPos,
                                                 int &                         endPos,
                                                 FieldNameValidator            validator)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("inString = ", inString.c_str());//####
    OD_SYSLOG_LL2("inLength = ", inLength, "startPos = ", startPos);
    int               workPos = skipWhitespace(inString, inLength, startPos);
    MatchExpression * result = NULL;
    
    if (workPos < inLength)
    {
        // We potentially have a constraint list.
        bool done = false;
        bool okSoFar = true;
        
        result = new MatchExpression();
        for ( ; okSoFar && (! done); )
        {
            int               nextElementPos;
            MatchConstraint * element = MatchConstraint::createMatcher(inString, inLength, workPos, nextElementPos,
                                                                       validator);
            
            if (element)
            {
                // Skip over any trailing whitespace, to find if the constraint list is complete or more coming.
                workPos = skipWhitespace(inString, inLength, nextElementPos);
                if (workPos < inLength)
                {
                    char scanChar = inString[workPos];

                    if (kComma == scanChar)
                    {
                        // We've got more elements to go.
                        result->_constraints.push_back(element);
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
                    // This is the last element.
                    result->_constraints.push_back(element);
                    done = true;
                }
            }
            else
            {
                // We have a malformed constraint list.
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
    OD_SYSLOG_EXIT_P(result);//####
    return result;
} // MatchExpression::createMatcher

char MatchExpression::expressionSeparatorCharacter(void)
{
    return kComma;
} // MatchExpression::expressionSeparatorCharacter

#if defined(__APPLE__)
# pragma mark Constructors and destructors
#endif // defined(__APPLE__)

MatchExpression::MatchExpression(void) :
        inherited(), _constraints()
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT_P(this);//####
} // MatchExpression::MatchExpression

MatchExpression::~MatchExpression(void)
{
    OD_SYSLOG_ENTER();//####
    empty();
    OD_SYSLOG_EXIT();//####
} // MatchExpression::~MatchExpression

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

yarp::os::ConstString MatchExpression::asString(void)
const
{
    yarp::os::ConstString result;
    
    for (MatchExpressionListSize ii = 0, maxI = _constraints.size(); ii < maxI; ++ii)
    {
        MatchConstraint * element = _constraints[ii];
        
        if (ii)
        {
            result += kComma;
            result += " ";
        }
        result += element->asString();
    }
    return result;
} // MatchExpression::asString

int MatchExpression::count(void)
const
{
    return static_cast<int>(_constraints.size());
} // MatchExpression::count

const MatchConstraint * MatchExpression::element(const int index)
const
{
    MatchConstraint * result;
    
    if ((index >= 0) && (index < static_cast<int>(_constraints.size())))
    {
        result = _constraints[static_cast<MatchExpressionListSize>(index)];
    }
    else
    {
        result = NULL;
    }
    return result;
} // MatchExpression::element

void MatchExpression::empty(void)
{
    OD_SYSLOG_ENTER();//####
    for (MatchExpressionListSize ii = 0, maxI = _constraints.size(); ii < maxI; ++ii)
    {
        MatchConstraint * element = _constraints[ii];
        
        delete element;
    }
    _constraints.clear();
    OD_SYSLOG_EXIT();//####
} // MatchExpression::empty

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
