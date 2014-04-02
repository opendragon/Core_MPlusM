//--------------------------------------------------------------------------------------
//
//  File:       MoMeMatchFieldWithValues.cpp
//
//  Project:    MoAndMe
//
//  Contains:   The class definition for a pattern matcher for field/value pairs.
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

#include "MoMeMatchFieldWithValues.h"
#include "MoMeBaseNameValidator.h"
#include "MoMeMatchFieldName.h"
#include "MoMeMatchValue.h"
#include "MoMeMatchValueList.h"

//#include "ODEnableLogging.h"
#include "ODLogging.h"

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The class definition for a pattern matcher for field/value pairs. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MoAndMe::Parser;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
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
                                                           BaseNameValidator *           validator)
{
    OD_LOG_ENTER();//####
    OD_LOG_S1("inString = ", inString.c_str());//####
    OD_LOG_LL2("inLength = ", inLength, "startPos = ", startPos);//####
    MatchFieldWithValues * result = NULL;

    try
    {
        int              workPos = startPos;
        MatchFieldName * fieldName = MatchFieldName::CreateMatcher(inString, inLength, startPos, workPos, validator);
        
        if (fieldName)
        {
            workPos = SkipWhitespace(inString, inLength, workPos);
            if (workPos < inLength)
            {
                int nextPos;
                
                if (MatchValueList::ListInitiatorCharacter() == inString[workPos])
                {
                    MatchValueList * asList = MatchValueList::CreateMatcher(inString, inLength, workPos, nextPos);
                    
                    if (asList)
                    {
                        result = new MatchFieldWithValues(validator, fieldName, asList);
                    }
                    else
                    {
                        OD_LOG("! (asList)");//####
                    }
                }
                else
                {
                    MatchValue * asSingle = MatchValue::CreateMatcher(inString, inLength, workPos, nextPos);
                    
                    if (asSingle)
                    {
                        result = new MatchFieldWithValues(validator, fieldName, asSingle);
                    }
                    else
                    {
                        OD_LOG("! (asSingle)");//####
                    }
                }
                if (result)
                {
                    endPos = nextPos;
                }
                else
                {
                    delete fieldName;
                }
            }
            else
            {
                OD_LOG("! (workPos < inLength)");//####
                delete fieldName;
            }
        }
        else
        {
            OD_LOG("! (fieldName)");//####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_EXIT_P(result);//####
    return result;
} // MatchFieldWithValues::CreateMatcher

#if defined(__APPLE__)
# pragma mark Constructors and destructors
#endif // defined(__APPLE__)

MatchFieldWithValues::MatchFieldWithValues(BaseNameValidator * validator,
                                           MatchFieldName *    fieldName,
                                           MatchValue *        asSingle) :
        inherited(), _validator(validator), _fieldName(fieldName), _singleValue(asSingle), _values(NULL)
{
    OD_LOG_ENTER();//####
    OD_LOG_P3("validator = ", validator, "fieldName = ", fieldName, "asSingle = ", asSingle);//####
    OD_LOG_EXIT_P(this);//####
} // MatchFieldWithValues::MatchFieldWithValues

MatchFieldWithValues::MatchFieldWithValues(BaseNameValidator * validator,
                                           MatchFieldName *    fieldName,
                                           MatchValueList *    asList) :
        inherited(), _validator(validator), _fieldName(fieldName), _singleValue(NULL), _values(asList)
{
    OD_LOG_ENTER();//####
    OD_LOG_P3("validator = ", validator, "fieldName = ", fieldName, "asList = ", asList);//####
    OD_LOG_EXIT_P(this);//####
} // MatchFieldWithValues::MatchFieldWithValues

MatchFieldWithValues::~MatchFieldWithValues(void)
{
    OD_LOG_OBJENTER();//####
    delete _fieldName;
    delete _singleValue;
    delete _values;
    OD_LOG_OBJEXIT();//####
} // MatchFieldWithValues::~MatchFieldWithValues

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

yarp::os::ConstString MatchFieldWithValues::asSQLString(void)
const
{
    OD_LOG_OBJENTER();//####
    yarp::os::ConstString converted;

    try
    {
        yarp::os::ConstString field(_fieldName->asString());
        const char *          prefixString = NULL;
        const char *          suffixString = NULL;
        const char *          trueName;
        
        if (_validator)
        {
            trueName = _validator->getPrefixAndSuffix(field.c_str(), prefixString, suffixString);
        }
        else
        {
            trueName = field.c_str();
        }
        OD_LOG_S1("trueName <- ", trueName);//####
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
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT_S(converted.c_str());//####
    return converted;
} // MatchFieldWithValues::asSQLString

yarp::os::ConstString MatchFieldWithValues::asString(void)
const
{
    yarp::os::ConstString result;
    
    try
    {
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
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    return result;
} // MatchFieldWithValues::asString

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
