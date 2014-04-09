//--------------------------------------------------------------------------------------
//
//  File:       MoMeParserTest.cpp
//
//  Project:    MoAndMe
//
//  Contains:   The test driver for the unit tests of the MoAndMe parser.
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

#include "MoMeMatchConstraint.h"
#include "MoMeMatchExpression.h"
#include "MoMeMatchFieldName.h"
#include "MoMeMatchFieldWithValues.h"
#include "MoMeMatchValue.h"
#include "MoMeMatchValueList.h"
#include "MoMeTestNameValidator.h"

//#include "ODEnableLogging.h"
#include "ODLogging.h"

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

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The test driver for the unit tests of the MoAndMe parser. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MoAndMe::Test;
using std::cout;
using std::cerr;
using std::endl;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark *** Test Case 01 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param expected @c true if the test is expected to succeed, and @c false otherwise.
 @param inString The string to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doTestParseValue(const bool   expected,
                            const char * inString) // create value matcher
{
    OD_LOG_ENTER();//####
    OD_LOG_B1("expected = ", expected);//####
    OD_LOG_S1("inString = ", inString);//####
    int result = 1;
    
    try
    {
        size_t                        endPos;
        size_t                        len = strlen(inString);
        MoAndMe::Parser::MatchValue * didMatch = MoAndMe::Parser::MatchValue::CreateMatcher(inString, len, 0, endPos);
        
        if ((NULL != didMatch) == expected)
        {
            result = 0;
        }
        else
        {
            OD_LOG("! ((NULL != didMatch) == expected)");//####
        }
        if (didMatch)
        {
            OD_LOG_S2("didMatch->asString = ", didMatch->asString(), "didMatch->asSQLString = ",//####
                      didMatch->asSQLString().c_str());//####
            cout << didMatch->asSQLString().c_str() << endl;
            delete didMatch;
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_EXIT_L(result);//####
    return result;
} // doTestParseValue

#if defined(__APPLE__)
# pragma mark *** Test Case 02 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param expected @c true if the test is expected to succeed, and @c false otherwise.
 @param inString The string to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doTestParseValueList(const bool   expected,
                                const char * inString) // create value matcher
{
    OD_LOG_ENTER();//####
    OD_LOG_B1("expected = ", expected);//####
    OD_LOG_S1("inString = ", inString);//####
    int result = 1;
    
    try
    {
        size_t                            endPos;
        size_t                            len = strlen(inString);
        MoAndMe::Parser::MatchValueList * didMatch = MoAndMe::Parser::MatchValueList::CreateMatcher(inString, len, 0,
                                                                                                    endPos);

        if ((NULL != didMatch) == expected)
        {
            result = 0;
        }
        else
        {
            OD_LOG("! ((NULL != didMatch) == expected)");//####
        }
        if (didMatch)
        {
            OD_LOG_S2("didMatch->asString = ", didMatch->asString().c_str(), "didMatch->asSQLString = ",//####
                      didMatch->asSQLString("field"));//####
            cout << didMatch->asSQLString("field").c_str() << endl;
            delete didMatch;
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_EXIT_L(result);//####
    return result;
} // doTestParseValueList

#if defined(__APPLE__)
# pragma mark *** Test Case 03 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param expected @c true if the test is expected to succeed, and @c false otherwise.
 @param inString The string to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doTestParseFieldName(const bool   expected,
                                const char * inString) // create value matcher
{
    OD_LOG_ENTER();//####
    OD_LOG_B1("expected = ", expected);//####
    OD_LOG_S1("inString = ", inString);//####
    int result = 1;
    
    try
    {
        size_t                            endPos;
        size_t                            len = strlen(inString);
        TestNameValidator *               validator = new TestNameValidator;
        MoAndMe::Parser::MatchFieldName * didMatch = MoAndMe::Parser::MatchFieldName::CreateMatcher(inString, len, 0,
                                                                                                    endPos, validator);

        if ((NULL != didMatch) == expected)
        {
            result = 0;
        }
        else
        {
            OD_LOG("! ((NULL != didMatch) == expected)");//####
        }
        if (didMatch)
        {
            OD_LOG_S2("didMatch->asString = ", didMatch->asString().c_str(), "didMatch->asSQLString = ",//####
                      didMatch->asSQLString());//####
            cout << didMatch->asSQLString().c_str() << endl;
            delete didMatch;
        }
        delete validator;
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_EXIT_L(result);//####
    return result;
} // doTestParseFieldName

#if defined(__APPLE__)
# pragma mark *** Test Case 04 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param expected @c true if the test is expected to succeed, and @c false otherwise.
 @param inString The string to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doTestParseFieldWithValues(const bool   expected,
                                      const char * inString) // create value matcher
{
    OD_LOG_ENTER();//####
    OD_LOG_B1("expected = ", expected);//####
    OD_LOG_S1("inString = ", inString);//####
    int result = 1;
    
    try
    {
        size_t                                  endPos;
        size_t                                  len = strlen(inString);
        TestNameValidator *                     validator = new TestNameValidator;
        MoAndMe::Parser::MatchFieldWithValues * didMatch =
                                                    MoAndMe::Parser::MatchFieldWithValues::CreateMatcher(inString, len,
                                                                                                         0, endPos,
                                                                                                         validator);

        if ((NULL != didMatch) == expected)
        {
            result = 0;
        }
        else
        {
            OD_LOG("! ((NULL != didMatch) == expected)");//####
        }
        if (didMatch)
        {
            OD_LOG_S2("didMatch->asString = ", didMatch->asString(), "didMatch->asSQLString = ",//####
                      didMatch->asSQLString().c_str());//####
            cout << didMatch->asSQLString().c_str() << endl;
            delete didMatch;
        }
        delete validator;
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_EXIT_L(result);//####
    return result;
} // doTestParseFieldWithValues

#if defined(__APPLE__)
# pragma mark *** Test Case 05 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param expected @c true if the test is expected to succeed, and @c false otherwise.
 @param inString The string to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doTestParseConstraintList(const bool   expected,
                                     const char * inString) // create value matcher
{
    OD_LOG_ENTER();//####
    OD_LOG_B1("expected = ", expected);//####
    OD_LOG_S1("inString = ", inString);//####
    int result = 1;
    
    try
    {
        size_t                             endPos;
        size_t                             len = strlen(inString);
        TestNameValidator *                validator = new TestNameValidator;
        MoAndMe::Parser::MatchConstraint * didMatch = MoAndMe::Parser::MatchConstraint::CreateMatcher(inString, len, 0,
                                                                                                      endPos, validator);

        if ((NULL != didMatch) == expected)
        {
            result = 0;
        }
        else
        {
            OD_LOG("! ((NULL != didMatch) == expected)");//####
        }
        if (didMatch)
        {
            OD_LOG_S2("didMatch->asString = ", didMatch->asString().c_str(), "didMatch->asSQLString = ",//####
                      didMatch->asSQLString());//####
            cout << didMatch->asSQLString().c_str() << endl;
            delete didMatch;
        }
        delete validator;
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_EXIT_L(result);//####
    return result;
} // doTestParseConstraintList

#if defined(__APPLE__)
# pragma mark *** Test Case 06 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param expected @c true if the test is expected to succeed, and @c false otherwise.
 @param inString The string to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doTestParseExpression(const bool   expected,
                                 const char * inString) // create value matcher
{
    OD_LOG_ENTER();//####
    OD_LOG_B1("expected = ", expected);//####
    OD_LOG_S1("inString = ", inString);//####
    int result = 1;
    
    try
    {
        size_t                             endPos;
        size_t                             len = strlen(inString);
        TestNameValidator *                validator = new TestNameValidator;
        MoAndMe::Parser::MatchExpression * didMatch = MoAndMe::Parser::MatchExpression::CreateMatcher(inString, len, 0,
                                                                                                      endPos,
                                                                                                      validator);

        if ((NULL != didMatch) == expected)
        {
            result = 0;
        }
        else
        {
            OD_LOG("! ((NULL != didMatch) == expected)");//####
        }
        if (didMatch)
        {
            OD_LOG_S2("didMatch->asString = ", didMatch->asString().c_str(), "didMatch->asSQLString = ",//####
                      didMatch->asSQLString("SELECT "));//####
            cout << didMatch->asSQLString("SELECT ").c_str() << endl;
            delete didMatch;
        }
        delete validator;
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_EXIT_L(result);//####
    return result;
} // doTestParseExpression

/*! @brief The signal handler to catch requests to stop the service.
 @param signal The signal being handled. */
static void catchSignal(int signal)
{
    OD_LOG_ENTER();//####
    OD_LOG_LL1("signal = ", signal);//####
    cerr << "Exiting due to signal " << signal << " = " << MoAndMe::NameOfSignal(signal) << endl;
    OD_LOG_EXIT_EXIT(1);//####
    yarp::os::exit(1);
} // catchSignal

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
    OD_LOG_INIT(*argv, kODLoggingOptionIncludeProcessID | kODLoggingOptionIncludeThreadID |//####
                kODLoggingOptionEnableThreadSupport | kODLoggingOptionWriteToStderr);//####
    OD_LOG_ENTER();//####
    int result = 1;
    
    try
    {
        if (2 < --argc)
        {
            int  selector = atoi(argv[1]);
            bool expected = (('t' == *argv[2]) || ('T' == *argv[2]));
            
            MoAndMe::Common::SetSignalHandlers(catchSignal);
            OD_LOG_LL1("selector <- ", selector);//####
            OD_LOG_B1("expected <- ", expected);//####
            switch (selector)
            {
                case 1:
                    result = doTestParseValue(expected, *(argv + 3));
                    break;
                    
                case 2:
                    result = doTestParseValueList(expected, *(argv + 3));
                    break;
                    
                case 3:
                    result = doTestParseFieldName(expected, *(argv + 3));
                    break;
                    
                case 4:
                    result = doTestParseFieldWithValues(expected, *(argv + 3));
                    break;
                    
                case 5:
                    result = doTestParseConstraintList(expected, *(argv + 3));
                    break;
                    
                case 6:
                    result = doTestParseExpression(expected, *(argv + 3));
                    break;
                    
                default:
                    break;
                    
            }
            if (result)
            {
                OD_LOG_LL1("%%%%%%% unit test failure = ", result);//####
            }
        }
        else
        {
            OD_LOG("! (2 < --argc)");//####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_EXIT_L(result);//####
    return result;
} // main
