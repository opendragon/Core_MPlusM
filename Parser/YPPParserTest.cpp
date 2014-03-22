//--------------------------------------------------------------------------------------
//
//  File:       YPPParserTest.cpp
//
//  Project:    YarpPlusPlus
//
//  Contains:   The test driver for the unit tests of the Yarp++ parser.
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

//#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPMatchConstraint.h"
#include "YPPMatchExpression.h"
#include "YPPMatchFieldName.h"
#include "YPPMatchFieldWithValues.h"
#include "YPPMatchValue.h"
#include "YPPMatchValueList.h"
#include <iostream>
#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wc++11-extensions"
# pragma clang diagnostic ignored "-Wdocumentation"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# pragma clang diagnostic ignored "-Wpadded"
# pragma clang diagnostic ignored "-Wshadow"
# pragma clang diagnostic ignored "-Wunused-parameter"
# pragma clang diagnostic ignored "-Wweak-vtables"
#endif // defined(__APPLE__)
#include <yarp/os/all.h>
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace YarpPlusPlusParser;
using std::cout;
using std::cerr;
using std::endl;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief the valid field names that may be used. Note that the strings are all lower-case for comparison purposes. */
static const char * kFieldNames[] =
{
    "description",
    "details",
    "input",
    "keyword",
    "name",
    "output",
    "portname",
    "request",
    "version"
};

/*! @brief The number of valid field names. */
static const size_t kFieldNamesCount = (sizeof(kFieldNames) / sizeof(*kFieldNames));

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

/*! @brief Check a candidate field name against the list of legal field names.
 @param aString The string to be checked.
 @param prefixString If non-@c NULL, a pointer to the string to be used in the SQL prefix for this field.
 @param suffixString If non-@c NULL, a pointer to the string to be used in the SQL suffix for this field.
 @returns The actual field name to be used or @c NULL if the field name was unmatched. */
static const char * fieldNameValidator(const char *  aString,
                                       const char ** prefixString,
                                       const char ** suffixString)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("aString = ", aString);//####
    const char * result = NULL;
    
    try
    {
        for (size_t ii = 0; ii < kFieldNamesCount; ++ii)
        {
            if (! strcmp(aString, kFieldNames[ii]))
            {
                result = kFieldNames[ii];
                break;
            }
            
        }
        if (prefixString)
        {
            *prefixString = NULL;
        }
        if (suffixString)
        {
            *suffixString = NULL;
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT_P(result);//####
    return result;
} // fieldNameValidator

#if defined(__APPLE__)
# pragma mark *** Test Case 01 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param expected @c true if the test is expected to succeed, and @c false otherwise.
 @param inString The string to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doCase01(const bool expected,
                    char *     inString) // create value matcher
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_B1("expected = ", expected);//####
    OD_SYSLOG_S1("inString = ", inString);//####
    int result = 1;
    
    try
    {
        int          endPos;
        int          len = static_cast<int>(strlen(inString));
        MatchValue * didMatch = MatchValue::CreateMatcher(inString, len, 0, endPos);
        
        if ((NULL != didMatch) == expected)
        {
            result = 0;
        }
        if (didMatch)
        {
            OD_SYSLOG_S2("didMatch->asString = ", didMatch->asString(), "didMatch->asSQLString = ",//####
                         didMatch->asSQLString().c_str());//####
            cout << didMatch->asSQLString().c_str() << endl;
            delete didMatch;
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT_L(result);//####
    return result;
} // doCase01

/*! @brief Perform a test case.
 @param expected @c true if the test is expected to succeed, and @c false otherwise.
 @param inString The string to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doCase02(const bool expected,
                    char *     inString) // create value matcher
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_B1("expected = ", expected);//####
    OD_SYSLOG_S1("inString = ", inString);//####
    int result = 1;
    
    try
    {
        int              endPos;
        int              len = static_cast<int>(strlen(inString));
        MatchValueList * didMatch = MatchValueList::CreateMatcher(inString, len, 0, endPos);

        if ((NULL != didMatch) == expected)
        {
            result = 0;
        }
        if (didMatch)
        {
            OD_SYSLOG_S2("didMatch->asString = ", didMatch->asString().c_str(), "didMatch->asSQLString = ",//####
                         didMatch->asSQLString("field"));//####
            cout << didMatch->asSQLString("field").c_str() << endl;
            delete didMatch;
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT_L(result);//####
    return result;
} // doCase02

/*! @brief Perform a test case.
 @param expected @c true if the test is expected to succeed, and @c false otherwise.
 @param inString The string to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doCase03(const bool expected,
                    char *     inString) // create value matcher
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_B1("expected = ", expected);//####
    OD_SYSLOG_S1("inString = ", inString);//####
    int result = 1;
    
    try
    {
        int              endPos;
        int              len = static_cast<int>(strlen(inString));
        MatchFieldName * didMatch = MatchFieldName::CreateMatcher(inString, len, 0, endPos, fieldNameValidator);

        if ((NULL != didMatch) == expected)
        {
            result = 0;
        }
        if (didMatch)
        {
            OD_SYSLOG_S2("didMatch->asString = ", didMatch->asString().c_str(), "didMatch->asSQLString = ",//####
                         didMatch->asSQLString());//####
            cout << didMatch->asSQLString().c_str() << endl;
            delete didMatch;
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT_L(result);//####
    return result;
} // doCase03

/*! @brief Perform a test case.
 @param expected @c true if the test is expected to succeed, and @c false otherwise.
 @param inString The string to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doCase04(const bool expected,
                    char *     inString) // create value matcher
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_B1("expected = ", expected);//####
    OD_SYSLOG_S1("inString = ", inString);//####
    int result = 1;
    
    try
    {
        int                    endPos;
        int                    len = static_cast<int>(strlen(inString));
        MatchFieldWithValues * didMatch = MatchFieldWithValues::CreateMatcher(inString, len, 0, endPos,
                                                                              fieldNameValidator);

        if ((NULL != didMatch) == expected)
        {
            result = 0;
        }
        if (didMatch)
        {
            OD_SYSLOG_S2("didMatch->asString = ", didMatch->asString(), "didMatch->asSQLString = ",//####
                         didMatch->asSQLString().c_str());//####
            cout << didMatch->asSQLString().c_str() << endl;
            delete didMatch;
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT_L(result);//####
    return result;
} // doCase04

/*! @brief Perform a test case.
 @param expected @c true if the test is expected to succeed, and @c false otherwise.
 @param inString The string to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doCase05(const bool expected,
                    char *     inString) // create value matcher
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_B1("expected = ", expected);//####
    OD_SYSLOG_S1("inString = ", inString);//####
    int result = 1;
    
    try
    {
        int               endPos;
        int               len = static_cast<int>(strlen(inString));
        MatchConstraint * didMatch = MatchConstraint::CreateMatcher(inString, len, 0, endPos, fieldNameValidator);

        if ((NULL != didMatch) == expected)
        {
            result = 0;
        }
        if (didMatch)
        {
            OD_SYSLOG_S2("didMatch->asString = ", didMatch->asString().c_str(), "didMatch->asSQLString = ",//####
                         didMatch->asSQLString());//####
            cout << didMatch->asSQLString().c_str() << endl;
            delete didMatch;
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT_L(result);//####
    return result;
} // doCase05

/*! @brief Perform a test case.
 @param expected @c true if the test is expected to succeed, and @c false otherwise.
 @param inString The string to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doCase06(const bool expected,
                    char *     inString) // create value matcher
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_B1("expected = ", expected);//####
    OD_SYSLOG_S1("inString = ", inString);//####
    int result = 1;
    
    try
    {
        int               endPos;
        int               len = static_cast<int>(strlen(inString));
        MatchExpression * didMatch = MatchExpression::CreateMatcher(inString, len, 0, endPos, fieldNameValidator);

        if ((NULL != didMatch) == expected)
        {
            result = 0;
        }
        if (didMatch)
        {
            OD_SYSLOG_S2("didMatch->asString = ", didMatch->asString().c_str(), "didMatch->asSQLString = ",//####
                         didMatch->asSQLString("SELECT "));//####
            cout << didMatch->asSQLString("SELECT ").c_str() << endl;
            delete didMatch;
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT_L(result);//####
    return result;
} // doCase06

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

/*! @brief The entry point for unit tests.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the unit tests.
 @returns @c 0 on a successful test and @c 1 on failure. */
int main(int     argc,
         char ** argv)
{
    OD_SYSLOG_INIT(*argv, kODSyslogOptionIncludeProcessID | kODSyslogOptionIncludeThreadID |//####
                   kODSyslogOptionEnableThreadSupport | kODSyslogOptionWriteToStderr);//####
    OD_SYSLOG_ENTER();//####
    int result = 1;
    
    try
    {
        if (2 < --argc)
        {
            int  selector = atoi(argv[1]);
            bool expected = (('t' == *argv[2]) || ('T' == *argv[2]));
            
            OD_SYSLOG_LL1("selector <- ", selector);//####
            OD_SYSLOG_B1("expected <- ", expected);//####
            switch (selector)
            {
                case 1:
                    result = doCase01(expected, *(argv + 3));
                    break;
                    
                case 2:
                    result = doCase02(expected, *(argv + 3));
                    break;
                    
                case 3:
                    result = doCase03(expected, *(argv + 3));
                    break;
                    
                case 4:
                    result = doCase04(expected, *(argv + 3));
                    break;
                    
                case 5:
                    result = doCase05(expected, *(argv + 3));
                    break;
                    
                case 6:
                    result = doCase06(expected, *(argv + 3));
                    break;
                    
                default:
                    break;
                    
            }
        }
        else
        {
            OD_SYSLOG("! (2 < --argc)");//####
        }
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT_L(result);//####
    return result;
} // main
