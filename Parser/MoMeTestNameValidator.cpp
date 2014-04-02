//--------------------------------------------------------------------------------------
//
//  File:       MoMeTestNameValidator.cpp
//
//  Project:    MoAndMe
//
//  Contains:   The class definition for the minimal functionality required for a MoAndMe
//              field name matcher.
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
//  Created:    2014-03-24
//
//--------------------------------------------------------------------------------------

#include "MoMeTestNameValidator.h"

//#include "ODEnableLogging.h"
#include "ODLogging.h"

#include <cstring>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The class definition for a field name validator used by the unit tests. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MoAndMe::Test;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief the valid field names that may be used. Note that the strings are all lower-case for comparison purposes. */
static const char * kFieldNames[] =
{
    "channelname",
    "description",
    "details",
    "input",
    "keyword",
    "name",
    "output",
    "request",
    "version"
}; // kFieldNames

/*! @brief The number of valid field names. */
static const size_t kFieldNamesCount = (sizeof(kFieldNames) / sizeof(*kFieldNames));

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and destructors
#endif // defined(__APPLE__)

TestNameValidator::TestNameValidator(void) :
        inherited()
{
    OD_LOG_ENTER();//####
    OD_LOG_EXIT_P(this);//####
} // TestNameValidator::TestNameValidator

TestNameValidator::~TestNameValidator(void)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_OBJEXIT();//####
} // TestNameValidator::~TestNameValidator

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

bool TestNameValidator::checkName(const char * aString)
{
    OD_LOG_ENTER();//####
    OD_LOG_S1("aString = ", aString);//####
    bool result = false;
    
    try
    {
        for (size_t ii = 0; ii < kFieldNamesCount; ++ii)
        {
            if (! strcmp(aString, kFieldNames[ii]))
            {
                result = true;
                break;
            }
            
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_EXIT_B(result);//####
    return result;
} // TestNameValidator::checkName

const char * TestNameValidator::getPrefixAndSuffix(const char *  aString,
                                                   const char *& prefixString,
                                                   const char *& suffixString)
{
    OD_LOG_ENTER();//####
    OD_LOG_S1("aString = ", aString);//####
    OD_LOG_P2("prefixString = ", &prefixString, "suffixString = ", &suffixString);//####
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
        prefixString = NULL;
        suffixString = NULL;
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_EXIT_P(result);//####
    return result;
} // TestNameValidator::getPrefixAndSuffix

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
