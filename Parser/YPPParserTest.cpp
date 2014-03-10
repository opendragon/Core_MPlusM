//
//  YPPParserTest.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-03-07.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPMatchFieldName.h"
#include "YPPMatchFieldWithValues.h"
#include "YPPMatchValue.h"
#include "YPPMatchValueList.h"
#include <ace/Version.h>
#include <iostream>
#include <yarp/conf/version.h>
#include <yarp/os/all.h>

using namespace YarpPlusPlusParser;
using std::cout;
using std::cerr;
using std::endl;

#if defined(__APPLE__)
# pragma mark Private structures and constants
#endif // defined(__APPLE__)

/*! @brief the valid field names that may be used. Note that the strings are all lower-case for comparison purposes. */
static const char * kFieldNames[] =
{
    "description",
    "input",
    "keyword",
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
 @returns @c true if the field name is recognized and @c false otherwise. */
static bool fieldNameValidator(const char * aString)
{
    bool result = false;
    
    for (size_t ii = 0; ii < kFieldNamesCount; ++ii)
    {
        if (! strcmp(aString, kFieldNames[ii]))
        {
            result = true;
            break;
        }
        
    }
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
    int          endPos;
    int          len = static_cast<int>(strlen(inString));
    MatchValue * didMatch = MatchValue::createMatcher(inString, len, 0, endPos);
    int          result = ((expected == (NULL != didMatch)) ? 0 : 1);

    if (didMatch)
    {
        OD_SYSLOG_S1("didMatch->asSQLString = ", didMatch->asSQLString().c_str());//####
        delete didMatch;
    }
    OD_SYSLOG_EXIT_LL(result);//####
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
    int              endPos;
    int              len = static_cast<int>(strlen(inString));
    MatchValueList * didMatch = MatchValueList::createMatcher(inString, len, 0, endPos);
    int              result = ((expected == (NULL != didMatch)) ? 0 : 1);
    
    if (didMatch)
    {
        OD_SYSLOG_S1("didMatch->asString = ", didMatch->asString().c_str());//####
        delete didMatch;
    }
    OD_SYSLOG_EXIT_LL(result);//####
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
    int              endPos;
    int              len = static_cast<int>(strlen(inString));
    MatchFieldName * didMatch = MatchFieldName::createMatcher(inString, len, 0, endPos, fieldNameValidator);
    int              result = ((expected == (NULL != didMatch)) ? 0 : 1);
    
    if (didMatch)
    {
        OD_SYSLOG_S1("didMatch->asString = ", didMatch->asString().c_str());//####
        delete didMatch;
    }
    OD_SYSLOG_EXIT_LL(result);//####
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
    int                    endPos;
    int                    len = static_cast<int>(strlen(inString));
    MatchFieldWithValues * didMatch = MatchFieldWithValues::createMatcher(inString, len, 0, endPos, fieldNameValidator);
    int                    result = ((expected == (NULL != didMatch)) ? 0 : 1);
    
    if (didMatch)
    {
        OD_SYSLOG_S1("didMatch->asString = ", didMatch->asString().c_str());//####
        delete didMatch;
    }
    OD_SYSLOG_EXIT_LL(result);//####
    return result;
} // doCase04

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

/*! @brief The entry point for unit tests.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the unit tests.
 @returns @c 0 on a successful test and @c 1 on failure. */
int main(int      argc,
         char * * argv)
{
    OD_SYSLOG_INIT(*argv, kODSyslogOptionIncludeProcessID | kODSyslogOptionIncludeThreadID |//####
                   kODSyslogOptionEnableThreadSupport);//####
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S3("YARP Version = ", YARP_VERSION_STRING, "YARP++ Version = ", YPP_VERSION, "ACE Version = ",//####
                 ACE_VERSION);//####
    int result;
    
    if (--argc > 2)
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

            default:
                result = 1;
                break;
                
        }
    }
    else
    {
        result = 1;
    }
    OD_SYSLOG_EXIT_L(result);//####
    return result;
} // main
