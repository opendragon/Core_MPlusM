//--------------------------------------------------------------------------------------------------
//
//  File:       M+MColumnNameValidator.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for the functionality required for an M+M field name matcher
//              for the Registry Service.
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
//  Created:    2014-03-24
//
//--------------------------------------------------------------------------------------------------

#include "M+MColumnNameValidator.h"

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the functionality required for an M+M field name matcher for the
 Registry Service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Registry;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief The prefix to be used when generating SQL for a 'description' request. */
#define DESCRIPTION_PREFIX_ T_( \
        CHANNELNAME_C_ " IN (SELECT DISTINCT " CHANNELNAME_C_ " FROM " SERVICES_T_ " WHERE ")

/*! @brief The suffix to be used when generating SQL for a 'description' request. */
#define DESCRIPTION_SUFFIX_ T_(")")

/*! @brief The prefix to be used when generating SQL for a 'keyword' request. */
#define KEYWORD_PREFIX_     T_( \
        "KEY IN (SELECT DISTINCT " REQUESTS_ID_C_ " FROM " REQUESTSKEYWORDS_T_ " WHERE ")

/*! @brief The suffix to be used when generating SQL for a 'keyword' request. */
#define KEYWORD_SUFFIX_     T_(")")

/*! @brief The prefix to be used when generating SQL for a 'name' request. */
#define NAME_PREFIX_        T_( \
        CHANNELNAME_C_ " IN (SELECT DISTINCT " CHANNELNAME_C_ " FROM " SERVICES_T_ " WHERE ")

/*! @brief The suffix to be used when generating SQL for a 'name' request. */
#define NAME_SUFFIX_        T_(")")

/*! @brief The prefix to be used when generating SQL for a 'tag' request. */
#define TAG_PREFIX_ T_( \
CHANNELNAME_C_ " IN (SELECT DISTINCT " CHANNELNAME_C_ " FROM " SERVICES_T_ " WHERE ")

/*! @brief The suffix to be used when generating SQL for a 'description' request. */
#define TAG_SUFFIX_ T_(")")

/*! @brief the valid field names that may be used. Note that the strings are all lower-case for
 comparison purposes. */
static const char * kColumnNames[] =
{
    // Name to match        Name to use             Prefix to be used    Suffix to be used
    DESCRIPTION_C_,         DESCRIPTION_C_,         DESCRIPTION_PREFIX_, DESCRIPTION_SUFFIX_,
    DETAILS_C_,             DETAILS_C_,             NULL,                NULL,
    EXECUTABLE_C_,          EXECUTABLE_C_,          NULL,                NULL,
    INPUT_C_,               INPUT_C_,               NULL,                NULL,
    KEYWORD_C_,             KEYWORDS_ID_C_,         KEYWORD_PREFIX_,     KEYWORD_SUFFIX_,
    NAME_C_,                NAME_C_,                NAME_PREFIX_,        NAME_SUFFIX_,
    OUTPUT_C_,              OUTPUT_C_,              NULL,                NULL,
    CHANNELNAME_C_,         CHANNELNAME_C_,         NULL,                NULL,
    REQUEST_C_,             REQUEST_C_,             NULL,                NULL,
    REQUESTSDESCRIPTION_C_, REQUESTSDESCRIPTION_C_, DESCRIPTION_PREFIX_, DESCRIPTION_SUFFIX_,
    TAG_C_,                 TAG_C_,                 TAG_PREFIX_,         TAG_SUFFIX_,
    VERSION_C_,             VERSION_C_,             NULL,                NULL
}; // kColumnNames

/*! @brief The number of valid field names. */
static const size_t kColumnNamesCount = (sizeof(kColumnNames) / sizeof(*kColumnNames));

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

ColumnNameValidator::ColumnNameValidator(void) :
    inherited()
{
    OD_LOG_ENTER(); //####
    OD_LOG_EXIT_P(this); //####
} // ColumnNameValidator::ColumnNameValidator

ColumnNameValidator::~ColumnNameValidator(void)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_OBJEXIT(); //####
} // ColumnNameValidator::~ColumnNameValidator

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

bool ColumnNameValidator::checkName(const char * aString)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1("aString = ", aString); //####
    bool result = false;
    
    try
    {
        for (size_t ii = 0; ii < kColumnNamesCount; ii += 4)
        {
            if (! strcmp(aString, kColumnNames[ii]))
            {
                result = true;
                break;
            }
            
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // ColumnNameValidator::checkName

const char * ColumnNameValidator::getPrefixAndSuffix(const char *   aString,
                                                     const char * & prefixString,
                                                     const char * & suffixString)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1("aString = ", aString); //####
    OD_LOG_P2("prefixString = ", &prefixString, "suffixString = ", &suffixString); //####
    const char * result = NULL;
    
    try
    {
        const char * resultPrefix = NULL;
        const char * resultSuffix = NULL;
        
        for (size_t ii = 0; ii < kColumnNamesCount; ii += 4)
        {
            if (! strcmp(aString, kColumnNames[ii]))
            {
                result = kColumnNames[ii + 1];
                resultPrefix = kColumnNames[ii + 2];
                resultSuffix = kColumnNames[ii + 3];
                break;
            }
            
        }
        prefixString = resultPrefix;
        suffixString = resultSuffix;
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_EXIT_P(result); //####
    return result;
} // ColumnNameValidator::getPrefixAndSuffix

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
