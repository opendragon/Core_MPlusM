//--------------------------------------------------------------------------------------
//
//  File:       YPPMatchExpression.cpp
//
//  Project:    YarpPlusPlus
//
//  Contains:   The class definition for a pattern matcher for expressions.
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
//  Created:    2014-03-10
//
//--------------------------------------------------------------------------------------

#include "YPPMatchExpression.h"
//#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPMatchConstraint.h"

using namespace YarpPlusPlusParser;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief The character used to separate expression list elements. */
static const char kComma = ',';

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

MatchExpression * MatchExpression::CreateMatcher(const yarp::os::ConstString & inString,
                                                 const int                     inLength,
                                                 const int                     startPos,
                                                 int &                         endPos,
                                                 FieldNameValidator            validator)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("inString = ", inString.c_str());//####
    OD_SYSLOG_LL2("inLength = ", inLength, "startPos = ", startPos);
    MatchExpression * result = NULL;
    
    try
    {
        int workPos = SkipWhitespace(inString, inLength, startPos);

        if (workPos < inLength)
        {
            // We potentially have a constraint list.
            bool done = false;
            bool okSoFar = true;
            
            result = new MatchExpression();
            for ( ; okSoFar && (! done); )
            {
                int               nextElementPos;
                MatchConstraint * element = MatchConstraint::CreateMatcher(inString, inLength, workPos, nextElementPos,
                                                                           validator);
                
                if (element)
                {
                    // Skip over any trailing whitespace, to find if the constraint list is complete or more coming.
                    workPos = SkipWhitespace(inString, inLength, nextElementPos);
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
                            OD_SYSLOG("! (kComma == scanChar)");//####
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
                    OD_SYSLOG("! (element)");//####
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
        else
        {
            OD_SYSLOG("! (workPos < inLength)");//####
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT_P(result);//####
    return result;
} // MatchExpression::CreateMatcher

char MatchExpression::ExpressionSeparatorCharacter(void)
{
    return kComma;
} // MatchExpression::ExpressionSeparatorCharacter

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

yarp::os::ConstString MatchExpression::asSQLString(const char * prefixString)
const
{
    OD_SYSLOG_ENTER();//####
    yarp::os::ConstString result;
    
    try
    {
        for (MatchExpressionListSize ii = 0, maxI = _constraints.size(); ii < maxI; ++ii)
        {
            MatchConstraint * element = _constraints[ii];
            
            if (ii)
            {
                result += " UNION ";
            }
            if (prefixString)
            {
                result += prefixString;
            }
            result += element->asSQLString();
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT_S(result.c_str());//####
    return result;
} // MatchExpression::asSQLString

yarp::os::ConstString MatchExpression::asString(void)
const
{
    yarp::os::ConstString result;
    
    try
    {
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
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
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
    MatchConstraint * result = NULL;
    
    try
    {
        if ((index >= 0) && (index < static_cast<int>(_constraints.size())))
        {
            result = _constraints[static_cast<MatchExpressionListSize>(index)];
        }
        else
        {
            OD_SYSLOG("! ((index >= 0) && (index < static_cast<int>(_constraints.size())))");//####
            result = NULL;
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    return result;
} // MatchExpression::element

void MatchExpression::empty(void)
{
    OD_SYSLOG_ENTER();//####
    try
    {
        for (MatchExpressionListSize ii = 0, maxI = _constraints.size(); ii < maxI; ++ii)
        {
            MatchConstraint * element = _constraints[ii];
            
            delete element;
        }
        _constraints.clear();
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT();//####
} // MatchExpression::empty

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
