//--------------------------------------------------------------------------------------------------
//
//  File:       ParserTest/m+mParserTest.cpp
//
//  Project:    m+m
//
//  Contains:   The test driver for the unit tests of the m+m parser.
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

#include "m+mTestNameValidator.h"

#include <m+m/m+mMatchConstraint.h>
#include <m+m/m+mMatchExpression.h>
#include <m+m/m+mMatchFieldName.h>
#include <m+m/m+mMatchFieldWithValues.h>
#include <m+m/m+mMatchValue.h>
#include <m+m/m+mMatchValueList.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The test driver for the unit tests of the m+m parser. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Parser;
using namespace MplusM::Test;
using std::cerr;
using std::cout;
using std::endl;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global constants and variables
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
static int
doTestParseValue(const bool   expected,
                 const char * inString) // create value matcher
{
    ODL_ENTER(); //####
    ODL_B1("expected = ", expected); //####
    ODL_S1("inString = ", inString); //####
    int result = 1;

    try
    {
        size_t               endPos;
        size_t               len = strlen(inString);
        Parser::MatchValue * didMatch = Parser::MatchValue::CreateMatcher(inString, len, 0, endPos);

        if ((NULL != didMatch) == expected)
        {
            result = 0;
        }
        else
        {
            ODL_LOG("! ((NULL != didMatch) == expected)"); //####
        }
        if (didMatch)
        {
            ODL_S2s("didMatch->asString = ", didMatch->asString(), //####
                    "didMatch->asSQLString = ", didMatch->asSQLString()); //####
            cout << didMatch->asSQLString().c_str() << endl;
            delete didMatch;
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_EXIT_L(result); //####
    return result;
} // doTestParseValue

#if defined(__APPLE__)
# pragma mark *** Test Case 02 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param expected @c true if the test is expected to succeed, and @c false otherwise.
 @param inString The string to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int
doTestParseValueList(const bool   expected,
                     const char * inString) // create value matcher
{
    ODL_ENTER(); //####
    ODL_B1("expected = ", expected); //####
    ODL_S1("inString = ", inString); //####
    int result = 1;

    try
    {
        size_t                   endPos;
        size_t                   len = strlen(inString);
        Parser::MatchValueList * didMatch = Parser::MatchValueList::CreateMatcher(inString, len, 0,
                                                                                  endPos);

        if ((NULL != didMatch) == expected)
        {
            result = 0;
        }
        else
        {
            ODL_LOG("! ((NULL != didMatch) == expected)"); //####
        }
        if (didMatch)
        {
            ODL_S2s("didMatch->asString = ", didMatch->asString(), //####
                    "didMatch->asSQLString = ", didMatch->asSQLString("field", false)); //####
            cout << didMatch->asSQLString("field", false).c_str() << endl;
            delete didMatch;
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_EXIT_L(result); //####
    return result;
} // doTestParseValueList

#if defined(__APPLE__)
# pragma mark *** Test Case 03 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param expected @c true if the test is expected to succeed, and @c false otherwise.
 @param inString The string to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int
doTestParseFieldName(const bool   expected,
                     const char * inString) // create value matcher
{
    ODL_ENTER(); //####
    ODL_B1("expected = ", expected); //####
    ODL_S1("inString = ", inString); //####
    int result = 1;

    try
    {
        size_t                   endPos;
        size_t                   len = strlen(inString);
        TestNameValidator *      validator = new TestNameValidator;
        Parser::MatchFieldName * didMatch = Parser::MatchFieldName::CreateMatcher(inString, len, 0,
                                                                                  endPos,
                                                                                  validator);

        if ((NULL != didMatch) == expected)
        {
            result = 0;
        }
        else
        {
            ODL_LOG("! ((NULL != didMatch) == expected)"); //####
        }
        if (didMatch)
        {
            ODL_S2s("didMatch->asString = ", didMatch->asString(), //####
                    "didMatch->asSQLString = ", didMatch->asSQLString()); //####
            cout << didMatch->asSQLString().c_str() << endl;
            delete didMatch;
        }
        delete validator;
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_EXIT_L(result); //####
    return result;
} // doTestParseFieldName

#if defined(__APPLE__)
# pragma mark *** Test Case 04 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param expected @c true if the test is expected to succeed, and @c false otherwise.
 @param inString The string to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int
doTestParseFieldWithValues(const bool   expected,
                           const char * inString) // create value matcher
{
    ODL_ENTER(); //####
    ODL_B1("expected = ", expected); //####
    ODL_S1("inString = ", inString); //####
    int result = 1;

    try
    {
        size_t                         endPos;
        size_t                         len = strlen(inString);
        TestNameValidator *            validator = new TestNameValidator;
        Parser::MatchFieldWithValues * didMatch =
                                            Parser::MatchFieldWithValues::CreateMatcher(inString,
                                                                                        len, 0,
                                                                                        endPos,
                                                                                        validator);

        if ((NULL != didMatch) == expected)
        {
            result = 0;
        }
        else
        {
            ODL_LOG("! ((NULL != didMatch) == expected)"); //####
        }
        if (didMatch)
        {
            ODL_S2s("didMatch->asString = ", didMatch->asString(), //####
                    "didMatch->asSQLString = ", didMatch->asSQLString()); //####
            cout << didMatch->asSQLString().c_str() << endl;
            delete didMatch;
        }
        delete validator;
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_EXIT_L(result); //####
    return result;
} // doTestParseFieldWithValues

#if defined(__APPLE__)
# pragma mark *** Test Case 05 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param expected @c true if the test is expected to succeed, and @c false otherwise.
 @param inString The string to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int
doTestParseConstraintList(const bool   expected,
                          const char * inString) // create value matcher
{
    ODL_ENTER(); //####
    ODL_B1("expected = ", expected); //####
    ODL_S1("inString = ", inString); //####
    int result = 1;

    try
    {
        size_t                    endPos;
        size_t                    len = strlen(inString);
        TestNameValidator *       validator = new TestNameValidator;
        Parser::MatchConstraint * didMatch = Parser::MatchConstraint::CreateMatcher(inString, len,
                                                                                    0, endPos,
                                                                                    validator);

        if ((NULL != didMatch) == expected)
        {
            result = 0;
        }
        else
        {
            ODL_LOG("! ((NULL != didMatch) == expected)"); //####
        }
        if (didMatch)
        {
            ODL_S2s("didMatch->asString = ", didMatch->asString(), //####
                    "didMatch->asSQLString = ", didMatch->asSQLString()); //####
            cout << didMatch->asSQLString().c_str() << endl;
            delete didMatch;
        }
        delete validator;
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_EXIT_L(result); //####
    return result;
} // doTestParseConstraintList

#if defined(__APPLE__)
# pragma mark *** Test Case 06 ***
#endif // defined(__APPLE__)

/*! @brief Perform a test case.
 @param expected @c true if the test is expected to succeed, and @c false otherwise.
 @param inString The string to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int
doTestParseExpression(const bool   expected,
                      const char * inString) // create value matcher
{
    ODL_ENTER(); //####
    ODL_B1("expected = ", expected); //####
    ODL_S1("inString = ", inString); //####
    int result = 1;

    try
    {
        size_t                    endPos;
        size_t                    len = strlen(inString);
        TestNameValidator *       validator = new TestNameValidator;
        Parser::MatchExpression * didMatch = Parser::MatchExpression::CreateMatcher(inString, len,
                                                                                    0, endPos,
                                                                                    validator);

        if ((NULL != didMatch) == expected)
        {
            result = 0;
        }
        else
        {
            ODL_LOG("! ((NULL != didMatch) == expected)"); //####
        }
        if (didMatch)
        {
            ODL_S2s("didMatch->asString = ", didMatch->asString(), //####
                    "didMatch->asSQLString = ", didMatch->asSQLString("SELECT ")); //####
            cout << didMatch->asSQLString("SELECT ").c_str() << endl;
            delete didMatch;
        }
        delete validator;
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_EXIT_L(result); //####
    return result;
} // doTestParseExpression

/*! @brief The signal handler to catch requests to stop the service.
 @param signal The signal being handled. */
static void
catchSignal(int signal)
{
    ODL_ENTER(); //####
    ODL_LL1("signal = ", signal); //####
    std::stringstream buff;
    YarpString        message("Exiting due to signal ");

    buff << signal;
    message += buff.str();
    message += " = ";
    message += MplusM::NameOfSignal(signal);
    MpM_ERROR_(message.c_str());
    ODL_EXIT_EXIT(1); //####
    yarp::os::exit(1);
} // catchSignal

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

/*! @brief The entry point for unit tests of the m+m Parser classes.

 The first argument is the test number, the second argument is either 't' or 'f', to indicate if the
 test is expected to succeed or fail, respectivelly, and the third argument is the string to be
 parsed. Output depends on the test being run.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the unit tests.
 @returns @c 0 on a successful test and @c 1 on failure. */
int
main(int      argc,
     char * * argv)
{
    ODL_INIT(*argv, kODLoggingOptionIncludeProcessID | kODLoggingOptionIncludeThreadID | //####
             kODLoggingOptionEnableThreadSupport | kODLoggingOptionWriteToStderr); //####
    ODL_ENTER(); //####
    int result = 1;

    try
    {
        if (2 < --argc)
        {
            const char * startPtr = argv[1];
            char *       endPtr;
            int          selector = strtol(startPtr, &endPtr, 10);

            ODL_LL1("selector <- ", selector); //####
            if ((startPtr != endPtr) && (! *endPtr) && (0 < selector))
            {
                bool expected = (('t' == *argv[2]) || ('T' == *argv[2]));

                SetSignalHandlers(catchSignal);
                ODL_B1("expected <- ", expected); //####
                switch (selector)
                {
                    case 1 :
                        result = doTestParseValue(expected, *(argv + 3));
                        break;

                    case 2 :
                        result = doTestParseValueList(expected, *(argv + 3));
                        break;

                    case 3 :
                        result = doTestParseFieldName(expected, *(argv + 3));
                        break;

                    case 4 :
                        result = doTestParseFieldWithValues(expected, *(argv + 3));
                        break;

                    case 5 :
                        result = doTestParseConstraintList(expected, *(argv + 3));
                        break;

                    case 6 :
                        result = doTestParseExpression(expected, *(argv + 3));
                        break;

                    default :
                        break;

                }
                if (result)
                {
                    ODL_LL1("%%%%%%% unit test failure = ", result); //####
                }
            }
        }
        else
        {
            ODL_LOG("! (2 < --argc)"); //####
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_EXIT_L(result); //####
    return result;
} // main
