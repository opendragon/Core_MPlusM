//
//  YPPMatchFieldWithValues.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-03-07.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPMatchFieldWithValues.h"
#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPMatchFieldName.h"
#include "YPPMatchValue.h"
#include "YPPMatchValueList.h"
//#include <cctype>
//#include <cstdlib>
//#include <cstring>

using namespace YarpPlusPlusParser;

#if defined(__APPLE__)
# pragma mark Private structures and constants
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

MatchFieldWithValues * MatchFieldWithValues::createMatcher(const yarp::os::ConstString & inString,
                                                           const int                     startPos,
                                                           int &                         endPos)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("inString = ", inString.c_str());//####
    OD_SYSLOG_LL1("startPos = ", startPos);//####
//    char                   scanChar = '\0';
    int                    workPos = startPos;
//    int                    length = inString.length();
    MatchFieldWithValues * result = NULL;
    MatchFieldName *       fieldName = MatchFieldName::createMatcher(inString, startPos, workPos);

    if (fieldName)
    {
        int              nextPos;
        MatchValue *     asSingle = NULL;
        MatchValueList * asList = NULL;
        
        // Check for a list of values first
        asList = MatchValueList::createMatcher(inString, workPos, nextPos);
        if (! asList)
        {
            // If it isn't a list of values, check for a single value
            asSingle = MatchValue::createMatcher(inString, false, workPos, nextPos);
        }
        if (asList || asSingle)
        {
            result = new MatchFieldWithValues(fieldName, asSingle, asList);
            endPos = nextPos;
        }
    }
    OD_SYSLOG_EXIT_P(result);//####
    return result;
} // MatchFieldWithValues::createMatcher

#if defined(__APPLE__)
# pragma mark Constructors and destructors
#endif // defined(__APPLE__)

MatchFieldWithValues::MatchFieldWithValues(MatchFieldName * fieldName,
                                           MatchValue *     asSingle,
                                           MatchValueList * asList) :
        inherited(), _fieldName(fieldName), _singleValue(asSingle), _values(asList)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT_P(this);//####
} // MatchFieldWithValues::MatchFieldWithValues

MatchFieldWithValues::~MatchFieldWithValues(void)
{
    OD_SYSLOG_ENTER();//####
    delete _fieldName;
    delete _singleValue;
    delete _values;
    OD_SYSLOG_EXIT();//####
} // MatchFieldWithValues::~MatchFieldWithValues

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

yarp::os::ConstString MatchFieldWithValues::asString(void)
const
{
    yarp::os::ConstString result;
    
    result += _fieldName->asString();
    result += ": ";
    if (_singleValue)
    {
        result += _singleValue->asString();
    }
    else if (_values)
    {
        result += _values->asString();
    }
    return result;
} // MatchFieldWithValues::asString

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
