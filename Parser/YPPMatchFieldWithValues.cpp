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

MatchFieldWithValues * MatchFieldWithValues::CreateMatcher(const yarp::os::ConstString & inString,
                                                           const int                     inLength,
                                                           const int                     startPos,
                                                           int &                         endPos,
                                                           FieldNameValidator            validator)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("inString = ", inString.c_str());//####
    OD_SYSLOG_LL2("inLength = ", inLength, "startPos = ", startPos);//####
    int                    workPos = startPos;
    MatchFieldWithValues * result = NULL;
    MatchFieldName *       fieldName = MatchFieldName::CreateMatcher(inString, inLength, startPos, workPos, validator);

    if (fieldName)
    {
        workPos = SkipWhitespace(inString, inLength, workPos);
        if (workPos < inLength)
        {
            int              nextPos;
            MatchValue *     asSingle = NULL;
            MatchValueList * asList = NULL;
            
            if (MatchValueList::ListInitiatorCharacter() == inString[workPos])
            {
                asList = MatchValueList::CreateMatcher(inString, inLength, workPos, nextPos);
            }
            else
            {
                asSingle = MatchValue::CreateMatcher(inString, inLength, workPos, nextPos);
            }
            if (asList || asSingle)
            {
                result = new MatchFieldWithValues(validator, fieldName, asSingle, asList);
                endPos = nextPos;
            }
            else
            {
                delete fieldName;
            }
        }
        else
        {
            delete fieldName;
        }
    }
    OD_SYSLOG_EXIT_P(result);//####
    return result;
} // MatchFieldWithValues::CreateMatcher

#if defined(__APPLE__)
# pragma mark Constructors and destructors
#endif // defined(__APPLE__)

MatchFieldWithValues::MatchFieldWithValues(FieldNameValidator validator,
                                           MatchFieldName *   fieldName,
                                           MatchValue *       asSingle,
                                           MatchValueList *   asList) :
        inherited(), _validator(validator), _fieldName(fieldName), _singleValue(asSingle), _values(asList)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P3("fieldName = ", fieldName, "asSingle = ", asSingle, "asList = ", asList);//####
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

yarp::os::ConstString MatchFieldWithValues::asSQLString(void)
const
{
    OD_SYSLOG_ENTER();//####
    yarp::os::ConstString field(_fieldName->asString());
    const char *          prefixString = NULL;
    const char *          suffixString = NULL;
    const char *          trueName;
    yarp::os::ConstString converted;

    if (_validator)
    {
        trueName = _validator(field.c_str(), &prefixString, &suffixString);
    }
    else
    {
        trueName = field.c_str();
    }
    OD_SYSLOG_S1("trueName <- ", trueName);//####
    if (_singleValue)
    {
        if (prefixString)
        {
            converted += prefixString;
        }
        converted += trueName;
        if (_singleValue->hasWildcardCharacters())
        {
            converted += " LIKE ";
        }
        else
        {
            converted += " = ";
        }
        converted += _singleValue->asSQLString();
        if (suffixString)
        {
            converted += suffixString;
        }
    }
    else if (_values)
    {
        if (prefixString)
        {
            converted += prefixString;
        }
        converted += _values->asSQLString(trueName);
        if (suffixString)
        {
            converted += suffixString;
        }
    }
    OD_SYSLOG_EXIT_S(converted.c_str());//####
    return converted;
} // MatchFieldWithValues::asSQLString

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
