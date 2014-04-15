//--------------------------------------------------------------------------------------
//
//  File:       MoMeRegistryService.cpp
//
//  Project:    MPlusM
//
//  Contains:   The class definition for the Service Registry M+M service.
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
//  Created:    2014-02-06
//
//--------------------------------------------------------------------------------------

#include "MoMeRegistryService.h"
#include "MoMeClientChannel.h"
#include "MoMeColumnNameValidator.h"
#include "MoMeMatchExpression.h"
#include "MoMeMatchRequestHandler.h"
#include "MoMeRegisterRequestHandler.h"
#include "MoMeRequests.h"
#include "MoMeServiceRequest.h"
#include "MoMeServiceResponse.h"
#include "MoMeUnregisterRequestHandler.h"

//#include "ODEnableLogging.h"
#include "ODLogging.h"

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wc++11-long-long"
#endif // defined(__APPLE__)
#include "sqlite3.h"
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)
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
#include <yarp/os/Network.h>
#include <yarp/os/Time.h>
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The class definition for the Service Registry M+M service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM::Registry;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

//#define USE_TEST_DATABASE /* Use an on-disk database rather than in-memory. */

/*! @brief The name of the index for the 'channelName' column of the 'requests' table. */
#define REQUESTS_CHANNELNAME_I_         "Requests_channelname_idx"
/*! @brief The name of the index for the 'requests' column of the 'requests' table. */
#define REQUESTS_REQUEST_I_             "Requests_request_idx"
/*! @brief The name of the index for the 'keywords_id' column of the 'requests-keywords' table. */
#define REQUESTSKEYWORDS_KEYWORDS_ID_I_ "RequestsKeywords_Keywords_id_idx"
/*! @brief The name of the index for the 'requests_id' column of the 'requests-keywords' table. */
#define REQUESTSKEYWORDS_REQUESTS_ID_I_ "RequestsKeywords_Requests_id_idx"
/*! @brief The name of the index for the 'name' column of the 'services' table. */
#define SERVICES_NAME_I_                "Services_name_idx"

/*! @brief The command to initiate an SQL transaction. */
static const char * kBeginTransaction = "BEGIN TRANSACTION";
/*! @brief The command to successfully complete an SQL transaction. */
static const char * kEndTransaction = "END TRANSACTION";
/*! @brief The command to undo an SQL transaction. */
static const char * kRollbackTransaction = "ROLLBACK TRANSACTION";

namespace MplusM
{
    namespace Registry
    {
        /*! @brief A function that provides bindings for parameters in an SQL statement.
         @param statement The prepared statement that is to be updated.
         @param stuff The source of data that is to be bound.
         @returns The SQLite error from the bind operation. */
        typedef int (*BindFunction)
            (sqlite3_stmt * statement,
             const void *   stuff);
        
        /*! @brief The data needed to add a request-keyword entry into the database. */
        struct RequestKeywordData
        {
            /*! @brief The name of the request. */
            yarp::os::ConstString _request;
            /*! @brief The service channel for the request. */
            yarp::os::ConstString _channel;
            /*! @brief A keyword for the request. */
            yarp::os::ConstString _key;
        }; // RequestKeywordData
        
        /*! @brief The data needed to add a service entry into the database. */
        struct ServiceData
        {
            /*! @brief The service channel for the service. */
            yarp::os::ConstString _channel;
            /*! @brief The name for the service. */
            yarp::os::ConstString _name;
            /*! @brief The description of the service. */
            yarp::os::ConstString _description;
        }; // ServiceData
        
    } // Registry
    
} // MplusM

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(OD_ENABLE_LOGGING)
/*! @brief Provide a symbolic name for an SQL status value.
 @param sqlRes The status value to be checked.
 @returns A string representing the symbolic name for the status value. */
static const char * mapStatusToStringForSQL(const int sqlRes)
{
    const char * result;
    
    switch (sqlRes)
    {
        case SQLITE_OK:
            result = "SQLITE_OK";
            break;
            
        case SQLITE_ERROR:
            result = "SQLITE_ERROR";
            break;
            
        case SQLITE_INTERNAL:
            result = "SQLITE_INTERNAL";
            break;
            
        case SQLITE_PERM:
            result = "SQLITE_PERM";
            break;
            
        case SQLITE_ABORT:
            result = "SQLITE_ABORT";
            break;
            
        case SQLITE_BUSY:
            result = "SQLITE_BUSY";
            break;
            
        case SQLITE_LOCKED:
            result = "SQLITE_LOCKED";
            break;
            
        case SQLITE_NOMEM:
            result = "SQLITE_NOMEM";
            break;
            
        case SQLITE_READONLY:
            result = "SQLITE_READONLY";
            break;
            
        case SQLITE_INTERRUPT:
            result = "SQLITE_INTERRUPT";
            break;
            
        case SQLITE_IOERR:
            result = "SQLITE_IOERR";
            break;
            
        case SQLITE_CORRUPT:
            result = "SQLITE_CORRUPT";
            break;
            
        case SQLITE_NOTFOUND:
            result = "SQLITE_NOTFOUND";
            break;
            
        case SQLITE_FULL:
            result = "SQLITE_FULL";
            break;
            
        case SQLITE_CANTOPEN:
            result = "SQLITE_CANTOPEN";
            break;
            
        case SQLITE_PROTOCOL:
            result = "SQLITE_PROTOCOL";
            break;
            
        case SQLITE_EMPTY:
            result = "SQLITE_EMPTY";
            break;
            
        case SQLITE_SCHEMA:
            result = "SQLITE_SCHEMA";
            break;
            
        case SQLITE_TOOBIG:
            result = "SQLITE_TOOBIG";
            break;
            
        case SQLITE_CONSTRAINT:
            result = "SQLITE_CONSTRAINT";
            break;
            
        case SQLITE_MISMATCH:
            result = "SQLITE_MISMATCH";
            break;
            
        case SQLITE_MISUSE:
            result = "SQLITE_MISUSE";
            break;
            
        case SQLITE_NOLFS:
            result = "SQLITE_NOLFS";
            break;
            
        case SQLITE_AUTH:
            result = "SQLITE_AUTH";
            break;
            
        case SQLITE_FORMAT:
            result = "SQLITE_FORMAT";
            break;
            
        case SQLITE_RANGE:
            result = "SQLITE_RANGE";
            break;
            
        case SQLITE_NOTADB:
            result = "SQLITE_NOTADB";
            break;
            
        case SQLITE_NOTICE:
            result = "SQLITE_NOTICE";
            break;
            
        case SQLITE_WARNING:
            result = "SQLITE_WARNING";
            break;
            
        case SQLITE_ROW:
            result = "SQLITE_ROW";
            break;
            
        case SQLITE_DONE:
            result = "SQLITE_DONE";
            break;
            
        default:
            result = "<Unknown>";
            break;
            
    }
    return result;
} // mapStatusToStringForSQL
#endif // defined(OD_ENABLE_LOGGING)

/*! @brief Perform a simple operation on the database.
 @param database The database to be modified.
 @param sqlStatement The operation to be performed.
 @param doBinds A function that will fill in any parameters in the statement.
 @param data The custom information used with the binding function.
 @returns @c true if the operation was successfully performed and @c false otherwise. */
static bool performSQLstatementWithNoResults(sqlite3 *    database,
                                             const char * sqlStatement,
                                             BindFunction doBinds = NULL,
                                             const void * data = NULL)
{
    OD_LOG_ENTER();//####
    OD_LOG_P2("database = ", database, "data = ", data);//####
    OD_LOG_S1("sqlStatement = ", sqlStatement);//####
    bool okSoFar = true;
    
    try
    {
        if (database)
        {
            sqlite3_stmt * prepared = NULL;
            int            sqlRes = sqlite3_prepare_v2(database, sqlStatement, static_cast<int>(strlen(sqlStatement)),
                                                       &prepared, NULL);
            
            OD_LOG_LL1("sqlRes <- ", sqlRes);//####
            OD_LOG_S1("sqlRes <- ", mapStatusToStringForSQL(sqlRes));//####
            if ((SQLITE_OK == sqlRes) && prepared)
            {
                if (doBinds)
                {
                    sqlRes = doBinds(prepared, data);
                    OD_LOG_LL1("sqlRes <- ", sqlRes);//####
                    OD_LOG_S1("sqlRes <- ", mapStatusToStringForSQL(sqlRes));//####
                    okSoFar = (SQLITE_OK == sqlRes);
                }
                if (okSoFar)
                {
                    do
                    {
                        sqlRes = sqlite3_step(prepared);
                        OD_LOG_LL1("sqlRes <- ", sqlRes);//####
                        OD_LOG_S1("sqlRes <- ", mapStatusToStringForSQL(sqlRes));//####
                        if (SQLITE_BUSY == sqlRes)
                        {
                            yarp::os::Time::delay(ONE_SECOND_DELAY / 10.0);
                        }
                    }
                    while (SQLITE_BUSY == sqlRes);
                    if (SQLITE_DONE != sqlRes)
                    {
                        OD_LOG("(SQLITE_DONE != sqlRes)");//####
                        okSoFar = false;
                    }
                }
                sqlite3_finalize(prepared);
            }
            else
            {
                OD_LOG("! ((SQLITE_OK == sqlRes) && prepared)");//####
                okSoFar = false;
            }
        }
        else
        {
            OD_LOG("! (database)");//####
            okSoFar = false;
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_EXIT_B(okSoFar);//####
    return okSoFar;
} // performSQLstatementWithNoResults

/*! @brief Perform an operation that can return multiple rows of results.
 @param database The database to be modified.
 @param resultList The list to be filled in with the values from the column of interest.
 @param sqlStatement The operation to be performed.
 @param columnOfInterest The column containing the value of interest.
 @param doBinds A function that will fill in any parameters in the statement.
 @param data The custom information used with the binding function.
 @returns @c true if the operation was successfully performed and @c false otherwise. */
static bool performSQLstatementWithResults(sqlite3 *                 database,
                                           MplusM::Common::Package & resultList,
                                           const char *              sqlStatement,
                                           const int                 columnOfInterest = 0,
                                           BindFunction              doBinds = NULL,
                                           const void *              data = NULL)
{
    OD_LOG_ENTER();//####
    OD_LOG_P3("database = ", database, "resultList = ", &resultList, "data = ", data);//####
    OD_LOG_LL1("columnOfInterest = ", columnOfInterest);//####
    OD_LOG_S1("sqlStatement = ", sqlStatement);//####
    bool okSoFar = true;
    
    try
    {
        if (database && (0 <= columnOfInterest))
        {
            sqlite3_stmt * prepared = NULL;
            int            sqlRes = sqlite3_prepare_v2(database, sqlStatement, static_cast<int>(strlen(sqlStatement)),
                                                       &prepared, NULL);
            
            OD_LOG_LL1("sqlRes <- ", sqlRes);//####
            OD_LOG_S1("sqlRes <- ", mapStatusToStringForSQL(sqlRes));//####
            if ((SQLITE_OK == sqlRes) && prepared)
            {
                if (doBinds)
                {
                    sqlRes = doBinds(prepared, data);
                    OD_LOG_LL1("sqlRes <- ", sqlRes);//####
                    OD_LOG_S1("sqlRes <- ", mapStatusToStringForSQL(sqlRes));//####
                    okSoFar = (SQLITE_OK == sqlRes);
                }
                if (okSoFar)
                {
                    for (sqlRes = SQLITE_ROW; SQLITE_ROW == sqlRes; )
                    {
                        do
                        {
                            sqlRes = sqlite3_step(prepared);
                            OD_LOG_LL1("sqlRes <- ", sqlRes);//####
                            OD_LOG_S1("sqlRes <- ", mapStatusToStringForSQL(sqlRes));//####
                            if (SQLITE_BUSY == sqlRes)
                            {
                                yarp::os::Time::delay(ONE_SECOND_DELAY / 10.0);
                            }
                        }
                        while (SQLITE_BUSY == sqlRes);
                        if (SQLITE_ROW == sqlRes)
                        {
                            // Gather the column data...
                            int colCount = sqlite3_column_count(prepared);
                            
                            OD_LOG_LL1("colCount <- ", colCount);//####
                            if ((0 < colCount) && (columnOfInterest < colCount))
                            {
                                const char * value = reinterpret_cast<const char *>(sqlite3_column_text(prepared,
                                                                                                    columnOfInterest));
                                
                                OD_LOG_S1("value <- ", value);//####
                                if (value)
                                {
                                    resultList.addString(value);
                                }
                            }
                        }
                    }
                    if (SQLITE_DONE != sqlRes)
                    {
                        OD_LOG("(SQLITE_DONE != sqlRes)");//####
                        okSoFar = false;
                    }
                }
                sqlite3_finalize(prepared);
            }
            else
            {
                OD_LOG("! ((SQLITE_OK == sqlRes) && prepared)");//####
                okSoFar = false;
            }
        }
        else
        {
            OD_LOG("! (database && (0 <= columnOfInterest))");//####
            okSoFar = false;
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
   
    OD_LOG_EXIT_B(okSoFar);//####
    return okSoFar;
} // performSQLstatementWithResults

/*! @brief Construct the tables needed in the database.
 @param database The database to be modified.
 @returns @c true if the tables were successfully built and @c false otherwise. */
static bool constructTables(sqlite3 * database)
{
    OD_LOG_ENTER();//####
    OD_LOG_P1("database = ", database);//####
    bool okSoFar = true;
    
    try
    {
        if (database)
        {
            if (performSQLstatementWithNoResults(database, kBeginTransaction))
            {
                static const char * tableSQL[] =
                {
#if defined(USE_TEST_DATABASE)
                    T_("DROP INDEX IF EXISTS " REQUESTS_REQUEST_I_),
                    T_("DROP INDEX IF EXISTS " REQUESTS_CHANNELNAME_I_),
                    T_("DROP INDEX IF EXISTS " REQUESTSKEYWORDS_KEYWORDS_ID_I_),
                    T_("DROP INDEX IF EXISTS " REQUESTSKEYWORDS_REQUESTS_ID_I_),
                    T_("DROP INDEX IF EXISTS " SERVICES_NAME_I_),
                    T_("DROP TABLE IF EXISTS " REQUESTSKEYWORDS_T_),
                    T_("DROP TABLE IF EXISTS " REQUESTS_T_),
                    T_("DROP TABLE IF EXISTS " KEYWORDS_T_),
                    T_("DROP TABLE IF EXISTS " SERVICES_T_),
#endif // defined(USE_TEST_DATABASE)
                    T_("CREATE TABLE IF NOT EXISTS " SERVICES_T_ "( " CHANNELNAME_C_
                       " Text NOT NULL DEFAULT _ PRIMARY KEY ON CONFLICT REPLACE, " NAME_C_
                       " Text NOT NULL DEFAULT _, " DESCRIPTION_C_ " Text NOT NULL DEFAULT _)"),
                    T_("CREATE INDEX IF NOT EXISTS " SERVICES_NAME_I_ " ON " SERVICES_T_ "(" NAME_C_ ")"),
                    T_("CREATE TABLE IF NOT EXISTS " KEYWORDS_T_ "( " KEYWORD_C_
                       " Text NOT NULL DEFAULT _ PRIMARY KEY ON CONFLICT IGNORE)"),
                    T_("CREATE TABLE IF NOT EXISTS " REQUESTS_T_ "( " CHANNELNAME_C_
                       " Text NOT NULL DEFAULT _ REFERENCES " SERVICES_T_ "(" CHANNELNAME_C_ "), " REQUEST_C_
                       " Text NOT NULL DEFAULT _, " INPUT_C_ " Text, " OUTPUT_C_ " Text, " VERSION_C_ " Text, "
                       DETAILS_C_ " Text, " KEY_C_ " Integer PRIMARY KEY)"),
                    T_("CREATE INDEX IF NOT EXISTS " REQUESTS_REQUEST_I_ " ON " REQUESTS_T_ "(" REQUEST_C_ ")"),
                    T_("CREATE INDEX IF NOT EXISTS " REQUESTS_CHANNELNAME_I_ " ON " REQUESTS_T_ "(" CHANNELNAME_C_ ")"),
                    T_("CREATE TABLE " REQUESTSKEYWORDS_T_ "( " KEYWORDS_ID_C_ " Text REFERENCES " KEYWORDS_T_ "("
                       KEYWORD_C_ "), " REQUESTS_ID_C_ " Integer REFERENCES " REQUESTS_T_ "(" KEY_C_ "))"),
                    T_("CREATE INDEX IF NOT EXISTS " REQUESTSKEYWORDS_KEYWORDS_ID_I_ " ON " REQUESTSKEYWORDS_T_ "("
                       KEYWORDS_ID_C_ ")"),
                    T_("CREATE INDEX IF NOT EXISTS " REQUESTSKEYWORDS_REQUESTS_ID_I_ " ON " REQUESTSKEYWORDS_T_ "("
                       REQUESTS_ID_C_ ")")
                };
                int numTables = (sizeof(tableSQL) / sizeof(*tableSQL));
                
                okSoFar = true;
                for (int ii = 0; okSoFar && (ii < numTables); ++ii)
                {
                    okSoFar = performSQLstatementWithNoResults(database, tableSQL[ii]);
                }
                if (okSoFar)
                {
                    okSoFar = performSQLstatementWithNoResults(database, kEndTransaction);
                }
                else
                {
                    performSQLstatementWithNoResults(database, kRollbackTransaction);
                }
            }
        }
        else
        {
            OD_LOG("! (database)");//####
            okSoFar = false;
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_EXIT_B(okSoFar);//####
    return okSoFar;
} // constructTables

/*! @brief Bind the values that are to be inserted into the Keywords table.
 @param statement The prepared statement that is to be updated.
 @param stuff The source of data that is to be bound.
 @returns The SQLite error from the bind operation. */
static int setupInsertForKeywords(sqlite3_stmt * statement,
                                  const void *   stuff)
{
    OD_LOG_ENTER();//####
    OD_LOG_P2("statement = ", statement, "stuff = ", stuff);//####
    int result = SQLITE_MISUSE;
    
    try
    {
        int keywordIndex = sqlite3_bind_parameter_index(statement, "@" KEYWORD_C_);

        if (0 < keywordIndex)
        {
            const char * nameString = static_cast<const char *>(stuff);
            
            OD_LOG_S1("nameString <- ", nameString);//####
            result = sqlite3_bind_text(statement, keywordIndex, nameString, static_cast<int>(strlen(nameString)),
                                       SQLITE_TRANSIENT);
            if (SQLITE_OK != result)
            {
                OD_LOG_S1("error description: ", sqlite3_errstr(result));//####
            }
        }
        else
        {
            OD_LOG("! (0 < keywordIndex)");//####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_EXIT_LL(result);
    return result;
} // setupInsertForKeywords

/*! @brief Bind the values that are to be inserted into the Requests table.
 @param statement The prepared statement that is to be updated.
 @param stuff The source of data that is to be bound.
 @returns The SQLite error from the bind operation. */
static int setupInsertForRequests(sqlite3_stmt * statement,
                                  const void *   stuff)
{
    OD_LOG_ENTER();//####
    OD_LOG_P2("statement = ", statement, "stuff = ", stuff);//####
    int result = SQLITE_MISUSE;

    try
    {
        int channelNameIndex = sqlite3_bind_parameter_index(statement, "@" CHANNELNAME_C_);
        int detailsIndex = sqlite3_bind_parameter_index(statement, "@" DETAILS_C_);
        int inputIndex = sqlite3_bind_parameter_index(statement, "@" INPUT_C_);
        int outputIndex = sqlite3_bind_parameter_index(statement, "@" OUTPUT_C_);
        int requestIndex = sqlite3_bind_parameter_index(statement, "@" REQUEST_C_);
        int versionIndex = sqlite3_bind_parameter_index(statement, "@" VERSION_C_);
        
        if ((0 < channelNameIndex) && (0 < detailsIndex) && (0 < inputIndex) && (0 < outputIndex) &&
            (0 < requestIndex) && (0 < versionIndex))
        {
            const RequestDescription * descriptor = static_cast<const RequestDescription *>(stuff);
            const char *               channelName = descriptor->_channel.c_str();
            
            OD_LOG_S1("channelName <- ", channelName);//####
            result = sqlite3_bind_text(statement, channelNameIndex, channelName, static_cast<int>(strlen(channelName)),
                                       SQLITE_TRANSIENT);
            if (SQLITE_OK == result)
            {
                const char * request = descriptor->_request.c_str();
                
                OD_LOG_S1("request <- ", request);//####
                result = sqlite3_bind_text(statement, requestIndex, request, static_cast<int>(strlen(request)),
                                           SQLITE_TRANSIENT);
            }
            if (SQLITE_OK == result)
            {
                const char * input = descriptor->_inputs.c_str();
                
                OD_LOG_S1("input <- ", input);//####
                result = sqlite3_bind_text(statement, inputIndex, input, static_cast<int>(strlen(input)),
                                           SQLITE_TRANSIENT);
            }
            if (SQLITE_OK == result)
            {
                const char * output = descriptor->_outputs.c_str();
                
                OD_LOG_S1("output <- ", output);//####
                result = sqlite3_bind_text(statement, outputIndex, output, static_cast<int>(strlen(output)),
                                           SQLITE_TRANSIENT);
            }
            if (SQLITE_OK == result)
            {
                const char * version = descriptor->_version.c_str();
                
                OD_LOG_S1("version <- ", version);//####
                result = sqlite3_bind_text(statement, versionIndex, version, static_cast<int>(strlen(version)),
                                           SQLITE_TRANSIENT);
            }
            if (SQLITE_OK == result)
            {
                const char * details = descriptor->_details.c_str();
                
                OD_LOG_S1("details <- ", details);//####
                result = sqlite3_bind_text(statement, detailsIndex, details, static_cast<int>(strlen(details)),
                                           SQLITE_TRANSIENT);
            }
            if (SQLITE_OK != result)
            {
                OD_LOG_S1("error description: ", sqlite3_errstr(result));//####
            }
        }
        else
        {
            OD_LOG("! ((0 < channelNameIndex) && (0 < detailsIndex) && (0 < inputIndex) && (0 < outputIndex) && "//####
                      "(0 < requestIndex) && (0 < versionIndex))");//####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_EXIT_LL(result);
    return result;
} // setupInsertForRequests

/*! @brief Bind the values that are to be inserted into the RequestsKeywords table.
 @param statement The prepared statement that is to be updated.
 @param stuff The source of data that is to be bound.
 @returns The SQLite error from the bind operation. */
static int setupInsertForRequestsKeywords(sqlite3_stmt * statement,
                                          const void *   stuff)
{
    OD_LOG_ENTER();//####
    OD_LOG_P2("statement = ", statement, "stuff = ", stuff);//####
    int result = SQLITE_MISUSE;

    try
    {
        int channelNameIndex = sqlite3_bind_parameter_index(statement, "@" CHANNELNAME_C_);
        int keywordIndex = sqlite3_bind_parameter_index(statement, "@" KEYWORD_C_);
        int requestIndex = sqlite3_bind_parameter_index(statement, "@" REQUEST_C_);
        
        if ((0 < channelNameIndex) && (0 < keywordIndex) && (0 < requestIndex))
        {
            const RequestKeywordData * descriptor = static_cast<const RequestKeywordData *>(stuff);
            const char *               keyword = descriptor->_key.c_str();
            
            OD_LOG_S1("keyword <- ", keyword);//####
            result = sqlite3_bind_text(statement, keywordIndex, keyword, static_cast<int>(strlen(keyword)),
                                       SQLITE_TRANSIENT);
            if (SQLITE_OK == result)
            {
                const char * request = descriptor->_request.c_str();
                
                OD_LOG_S1("request <- ", request);//####
                result = sqlite3_bind_text(statement, requestIndex, request, static_cast<int>(strlen(request)),
                                           SQLITE_TRANSIENT);
            }
            if (SQLITE_OK == result)
            {
                const char * channelName = descriptor->_channel.c_str();
                
                OD_LOG_S1("channelName <- ", channelName);//####
                result = sqlite3_bind_text(statement, channelNameIndex, channelName,
                                           static_cast<int>(strlen(channelName)), SQLITE_TRANSIENT);
            }
            if (SQLITE_OK != result)
            {
                OD_LOG_S1("error description: ", sqlite3_errstr(result));//####
            }
        }
        else
        {
            OD_LOG("! ((0 < channelNameIndex) && (0 < keywordIndex) && (0 < requestIndex))");//####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_EXIT_LL(result);
    return result;
} // setupInsertForRequestsKeywords

/*! @brief Bind the values that are to be inserted into the Services table.
 @param statement The prepared statement that is to be updated.
 @param stuff The source of data that is to be bound.
 @returns The SQLite error from the bind operation. */
static int setupInsertForServices(sqlite3_stmt * statement,
                                  const void *   stuff)
{
    OD_LOG_ENTER();//####
    OD_LOG_P2("statement = ", statement, "stuff = ", stuff);//####
    int result = SQLITE_MISUSE;

    try
    {
        int channelNameIndex = sqlite3_bind_parameter_index(statement, "@" CHANNELNAME_C_);
        int descriptionIndex = sqlite3_bind_parameter_index(statement, "@" DESCRIPTION_C_);
        int nameIndex = sqlite3_bind_parameter_index(statement, "@" NAME_C_);
        
        if ((0 < channelNameIndex) && (0 < descriptionIndex) && (0 < nameIndex))
        {
            const ServiceData * descriptor = static_cast<const ServiceData *>(stuff);
            const char *        channelName = descriptor->_channel.c_str();
            
            OD_LOG_S1("channelName <- ", channelName);//####
            result = sqlite3_bind_text(statement, channelNameIndex, channelName, static_cast<int>(strlen(channelName)),
                                       SQLITE_TRANSIENT);
            if (SQLITE_OK == result)
            {
                const char * name = descriptor->_name.c_str();
                
                OD_LOG_S1("name <- ", name);//####
                result = sqlite3_bind_text(statement, nameIndex, name, static_cast<int>(strlen(name)),
                                           SQLITE_TRANSIENT);
            }
            if (SQLITE_OK == result)
            {
                const char * description = descriptor->_description.c_str();
                
                OD_LOG_S1("description <- ", description);//####
                result = sqlite3_bind_text(statement, descriptionIndex, description,
                                           static_cast<int>(strlen(description)), SQLITE_TRANSIENT);
            }
            if (SQLITE_OK != result)
            {
                OD_LOG_S1("error description: ", sqlite3_errstr(result));//####
            }
        }
        else
        {
            OD_LOG("! ((0 < channelNameIndex) && (0 < descriptionIndex) && (0 < nameIndex))");//####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_EXIT_LL(result);
    return result;
} // setupInsertForServices

/*! @brief Bind the values that are to be removed from the Requests table.
 @param statement The prepared statement that is to be updated.
 @param stuff The source of data that is to be bound.
 @returns The SQLite error from the bind operation. */
static int setupRemoveForRequests(sqlite3_stmt * statement,
                                  const void *   stuff)
{
    OD_LOG_ENTER();//####
    OD_LOG_P2("statement = ", statement, "stuff = ", stuff);//####
    int result = SQLITE_MISUSE;

    try
    {
        int channelNameIndex = sqlite3_bind_parameter_index(statement, "@" CHANNELNAME_C_);
        
        if (0 < channelNameIndex)
        {
            const char * channelName = static_cast<const char *>(stuff);
            
            result = sqlite3_bind_text(statement, channelNameIndex, channelName, static_cast<int>(strlen(channelName)),
                                       SQLITE_TRANSIENT);
            if (SQLITE_OK != result)
            {
                OD_LOG_S1("error description: ", sqlite3_errstr(result));//####
            }
        }
        else
        {
            OD_LOG("! (0 < channelNameIndex)");//####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_EXIT_LL(result);
    return result;
} // setupRemoveForRequests

/*! @brief Bind the values that are to be removed from the RequestsKeywords table.
 @param statement The prepared statement that is to be updated.
 @param stuff The source of data that is to be bound.
 @returns The SQLite error from the bind operation. */
static int setupRemoveForRequestsKeywords(sqlite3_stmt * statement,
                                          const void *   stuff)
{
    OD_LOG_ENTER();//####
    OD_LOG_P2("statement = ", statement, "stuff = ", stuff);//####
    int result = SQLITE_MISUSE;

    try
    {
        int channelNameIndex = sqlite3_bind_parameter_index(statement, "@" CHANNELNAME_C_);
        
        if (0 < channelNameIndex)
        {
            const char * channelName = static_cast<const char *>(stuff);
            
            result = sqlite3_bind_text(statement, channelNameIndex, channelName, static_cast<int>(strlen(channelName)),
                                       SQLITE_TRANSIENT);
            if (SQLITE_OK != result)
            {
                OD_LOG_S1("error description: ", sqlite3_errstr(result));//####
            }
        }
        else
        {
            OD_LOG("! (0 < channelNameIndex)");//####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_EXIT_LL(result);
    return result;
} // setupRemoveForRequestsKeywords

/*! @brief Bind the values that are to be removed from the Services table.
 @param statement The prepared statement that is to be updated.
 @param stuff The source of data that is to be bound.
 @returns The SQLite error from the bind operation. */
static int setupRemoveForServices(sqlite3_stmt * statement,
                                  const void *   stuff)
{
    OD_LOG_ENTER();//####
    OD_LOG_P2("statement = ", statement, "stuff = ", stuff);//####
    int result = SQLITE_MISUSE;

    try
    {
        int channelNameIndex = sqlite3_bind_parameter_index(statement, "@" CHANNELNAME_C_);
        
        if (0 < channelNameIndex)
        {
            const char * channelName = static_cast<const char *>(stuff);
            
            result = sqlite3_bind_text(statement, channelNameIndex, channelName, static_cast<int>(strlen(channelName)),
                                       SQLITE_TRANSIENT);
            if (SQLITE_OK != result)
            {
                OD_LOG_S1("error description: ", sqlite3_errstr(result));//####
            }
        }
        else
        {
            OD_LOG("! (0 < channelNameIndex)");//####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_EXIT_LL(result);
    return result;
} // setupRemoveForServices

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and destructors
#endif // defined(__APPLE__)

RegistryService::RegistryService(const bool                    useInMemoryDb,
                                 const yarp::os::ConstString & serviceHostName,
                                 const yarp::os::ConstString & servicePortNumber) :
        inherited(true, MpM_REGISTRY_CANONICAL_NAME, "The Service Registry service", MpM_SERVICE_REGISTRY_CHANNEL_NAME,
                  serviceHostName, servicePortNumber), _db(NULL), _validator(new ColumnNameValidator),
        _matchHandler(NULL), _registerHandler(NULL), _unregisterHandler(NULL), _inMemory(useInMemoryDb),
        _isActive(false)
{
    OD_LOG_ENTER();//####
    OD_LOG_B1("useInMemoryDb = ", useInMemoryDb);//####
    OD_LOG_S2("serviceHostName = ", serviceHostName.c_str(), "servicePortNumber = ", servicePortNumber.c_str());//####
    attachRequestHandlers();
    OD_LOG_EXIT_P(this);//####
} // RegistryService::RegistryService

RegistryService::~RegistryService(void)
{
    OD_LOG_OBJENTER();//####
    detachRequestHandlers();
    if (_db)
    {
        sqlite3_close(_db);
    }
    delete _validator;
    OD_LOG_OBJEXIT();//####
} // RegistryService::~RegistryService

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

bool RegistryService::addRequestRecord(const Common::Package &    keywordList,
                                       const RequestDescription & description)
{
    OD_LOG_OBJENTER();//####
    bool okSoFar = false;
    
    try
    {
        if (performSQLstatementWithNoResults(_db, kBeginTransaction))
        {
            static const char * insertIntoRequests = T_("INSERT INTO " REQUESTS_T_ "(" CHANNELNAME_C_ "," REQUEST_C_ ","
                                                        INPUT_C_ "," OUTPUT_C_ "," VERSION_C_ "," DETAILS_C_
                                                        ") VALUES(@" CHANNELNAME_C_ ",@" REQUEST_C_ ",@" INPUT_C_ ",@"
                                                        OUTPUT_C_ ",@" VERSION_C_ ",@" DETAILS_C_ ")");
            
            // Add the request.
            okSoFar = performSQLstatementWithNoResults(_db, insertIntoRequests, setupInsertForRequests,
                                                       static_cast<const void *>(&description));
            if (okSoFar)
            {
                // Add the keywords.
                int                numKeywords = keywordList.size();
                RequestKeywordData reqKeyData;
                
                reqKeyData._request = description._request;
                reqKeyData._channel = description._channel;
                for (int ii = 0; okSoFar && (ii < numKeywords); ++ii)
                {
                    yarp::os::Value & aKeyword(keywordList.get(ii));
                    
                    if (aKeyword.isString())
                    {
                        static const char * insertIntoKeywords = T_("INSERT INTO " KEYWORDS_T_ "(" KEYWORD_C_
                                                                    ") VALUES(@" KEYWORD_C_ ")");
                        static const char * insertIntoRequestsKeywords = T_("INSERT INTO " REQUESTSKEYWORDS_T_ "("
                                                                            KEYWORDS_ID_C_ "," REQUESTS_ID_C_
                                                                            ") SELECT @" KEYWORD_C_ ", " KEY_C_ " FROM "
                                                                            REQUESTS_T_ " WHERE " REQUEST_C_ " = @"
                                                                            REQUEST_C_ " AND " CHANNELNAME_C_ " = @"
                                                                            CHANNELNAME_C_);
                        
                        reqKeyData._key = aKeyword.toString();
                        okSoFar = performSQLstatementWithNoResults(_db, insertIntoKeywords, setupInsertForKeywords,
                                                                   static_cast<const void *>(reqKeyData._key.c_str()));
                        if (okSoFar)
                        {
                            okSoFar = performSQLstatementWithNoResults(_db, insertIntoRequestsKeywords,
                                                                       setupInsertForRequestsKeywords, &reqKeyData);
                        }
                    }
                    else
                    {
                        OD_LOG("! (aKeyword.isString())");//####
                        okSoFar = false;
                    }
                }
            }
            if (okSoFar)
            {
                okSoFar = performSQLstatementWithNoResults(_db, kEndTransaction);
            }
            else
            {
                performSQLstatementWithNoResults(_db, kRollbackTransaction);
            }
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT_B(okSoFar);//####
    return okSoFar;
} // RegistryService::addRequestRecord

bool RegistryService::addServiceRecord(const yarp::os::ConstString & channelName,
                                       const yarp::os::ConstString & name,
                                       const yarp::os::ConstString & description)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_S2("channelName = ", channelName.c_str(), "name = ", name.c_str());//####
    bool okSoFar = false;
    
    try
    {
        if (performSQLstatementWithNoResults(_db, kBeginTransaction))
        {
            // Add the service channel name.
            static const char * insertIntoServices = T_("INSERT INTO " SERVICES_T_ "(" CHANNELNAME_C_ "," NAME_C_ ","
                                                        DESCRIPTION_C_ ") VALUES(@" CHANNELNAME_C_ ",@" NAME_C_ ",@"
                                                        DESCRIPTION_C_ ")");
            ServiceData         servData;
            
            servData._channel = channelName;
            servData._name = name;
            servData._description = description;
            okSoFar = performSQLstatementWithNoResults(_db, insertIntoServices, setupInsertForServices,
                                                       static_cast<const void *>(&servData));
            if (okSoFar)
            {
                okSoFar = performSQLstatementWithNoResults(_db, kEndTransaction);
            }
            else
            {
                performSQLstatementWithNoResults(_db, kRollbackTransaction);
            }
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT_B(okSoFar);//####
    return okSoFar;
} // RegistryService::addServiceRecord

void RegistryService::attachRequestHandlers(void)
{
    OD_LOG_OBJENTER();//####
    try
    {
        _matchHandler = new MatchRequestHandler(*this, _validator);
        _registerHandler = new RegisterRequestHandler(*this);
        _unregisterHandler = new UnregisterRequestHandler(*this);
        if (_matchHandler && _registerHandler && _unregisterHandler)
        {
            registerRequestHandler(_matchHandler);
            registerRequestHandler(_registerHandler);
            registerRequestHandler(_unregisterHandler);
        }
        else
        {
            OD_LOG("! (_matchHandler && _registerHandler && _unregisterHandler)");//####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT();//####
} // RegistryService::attachRequestHandlers

void RegistryService::detachRequestHandlers(void)
{
    OD_LOG_OBJENTER();//####
    try
    {
        if (_matchHandler)
        {
            unregisterRequestHandler(_matchHandler);
            delete _matchHandler;
            _matchHandler = NULL;
        }
        if (_registerHandler)
        {
            unregisterRequestHandler(_registerHandler);
            delete _registerHandler;
            _registerHandler = NULL;
        }
        if (_unregisterHandler)
        {
            unregisterRequestHandler(_unregisterHandler);
            delete _unregisterHandler;
            _unregisterHandler = NULL;
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT();//####
} // RegistryService::detachRequestHandlers

bool RegistryService::processMatchRequest(Parser::MatchExpression * matcher,
                                          Common::Package &         reply)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_P1("matcher = ", matcher);//####
    bool okSoFar = false;
    
    try
    {
        if (matcher)
        {
            if (performSQLstatementWithNoResults(_db, kBeginTransaction))
            {
                Common::Package &     subList = reply.addList();
                yarp::os::ConstString requestAsSQL(matcher->asSQLString("SELECT DISTINCT " CHANNELNAME_C_ " FROM "
                                                                        REQUESTS_T_ " WHERE "));
                
                OD_LOG_S1("requestAsSQL <- ", requestAsSQL.c_str());//####
                okSoFar = performSQLstatementWithResults(_db, subList, requestAsSQL.c_str());
                if (okSoFar)
                {
                    okSoFar = performSQLstatementWithNoResults(_db, kEndTransaction);
                }
                else
                {
                    performSQLstatementWithNoResults(_db, kRollbackTransaction);
                }
            }
        }
        else
        {
            OD_LOG("! (matcher)");//####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT_B(okSoFar);//####
    return okSoFar;
} // RegistryService::processMatchRequest

bool RegistryService::removeServiceRecord(const yarp::os::ConstString & serviceChannelName)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_S1("serviceChannelName = ", serviceChannelName.c_str());//####
    bool okSoFar = false;
    
    try
    {
        if (performSQLstatementWithNoResults(_db, kBeginTransaction))
        {
            // Remove the service channel requests.
            static const char * removeFromRequestsKeywords = T_("DELETE FROM " REQUESTSKEYWORDS_T_ " WHERE "
                                                                REQUESTS_ID_C_ " IN (SELECT " KEY_C_ " FROM "
                                                                REQUESTS_T_ " WHERE " CHANNELNAME_C_ " = @"
                                                                CHANNELNAME_C_ ")");

            okSoFar = performSQLstatementWithNoResults(_db, removeFromRequestsKeywords, setupRemoveForRequestsKeywords,
                                                       static_cast<const void *>(serviceChannelName.c_str()));
            if (okSoFar)
            {
                // Remove the service channel requests.
                static const char * removeFromRequests = "DELETE FROM " REQUESTS_T_ " WHERE " CHANNELNAME_C_ " = @"
                                                            CHANNELNAME_C_;

                okSoFar = performSQLstatementWithNoResults(_db, removeFromRequests, setupRemoveForRequests,
                                                           static_cast<const void *>(serviceChannelName.c_str()));
            }
            if (okSoFar)
            {
                // Remove the service channel name.
                static const char * removeFromServices = "DELETE FROM " SERVICES_T_ " WHERE " CHANNELNAME_C_ " = @"
                                                            CHANNELNAME_C_;
                
                okSoFar = performSQLstatementWithNoResults(_db, removeFromServices, setupRemoveForServices,
                                                           static_cast<const void *>(serviceChannelName.c_str()));
            }
            if (okSoFar)
            {
                okSoFar = performSQLstatementWithNoResults(_db, kEndTransaction);
            }
            else
            {
                performSQLstatementWithNoResults(_db, kRollbackTransaction);
            }
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT_B(okSoFar);//####
    return okSoFar;
} // RegistryService::removeServiceRecord

bool RegistryService::setUpDatabase(void)
{
    OD_LOG_OBJENTER();//####
    bool okSoFar = true;

    try
    {
        int  sqlRes;
        
        if (! _db)
        {
            const char * dbFileName;
            
#if defined(USE_TEST_DATABASE)
            dbFileName = "test.db";
#else // ! defined(USE_TEST_DATABASE)
            if (_inMemory)
            {
                dbFileName = ":memory:";
            }
            else
            {
                dbFileName = "";
            }
#endif // ! defined(USE_TEST_DATABASE)
            sqlRes = sqlite3_open_v2(dbFileName, &_db, SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE, NULL);
            if (SQLITE_OK != sqlRes)
            {
                OD_LOG("(SQLITE_OK != sqlRes)");//####
                okSoFar = false;
                sqlite3_close(_db);
                _db = NULL;
            }
        }
        if (_db)
        {
            okSoFar = constructTables(_db);
            if (! okSoFar)
            {
                OD_LOG("(! okSoFar)");//####
                sqlite3_close(_db);
                _db = NULL;
            }
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT_B(okSoFar);//####
    return okSoFar;
} // RegistryService::setUpDatabase

bool RegistryService::start(void)
{
    OD_LOG_OBJENTER();//####
    bool result = false;
    
    try
    {
        if ((! isActive()) && (! isStarted()))
        {
            inherited::start();
            if (isStarted() && setUpDatabase())
            {
                // Register ourselves!!!
                yarp::os::ConstString   aName(Common::GetRandomChannelName(MpM_SERVICE_REGISTRY_CHANNEL_NAME "/temp_"));
                Common::ClientChannel * newChannel = new Common::ClientChannel;
                
                if (newChannel)
                {
                    if (newChannel->openWithRetries(aName))
                    {
                        if (Common::NetworkConnectWithRetries(aName, MpM_SERVICE_REGISTRY_CHANNEL_NAME))
                        {
                            Common::Package         parameters(MpM_SERVICE_REGISTRY_CHANNEL_NAME);
                            Common::ServiceRequest  request(MpM_REGISTER_REQUEST, parameters);
                            Common::ServiceResponse response;
                            
                            if (request.send(*newChannel, &response))
                            {
                                // Check that we got a successful self-registration!
                                if (1 == response.count())
                                {
                                    yarp::os::Value theValue = response.element(0);
                                    
                                    OD_LOG_S1("theValue <- ", theValue.toString().c_str());//####
                                    if (theValue.isString())
                                    {
                                        _isActive = (theValue.toString() == MpM_OK_RESPONSE);
                                        OD_LOG_B1("_isActive <- ", _isActive);//####
                                    }
                                    else
                                    {
                                        OD_LOG("! (theValue.isString())");//####
                                    }
                                }
                                else
                                {
                                    OD_LOG("! (1 == response.count())");//####
                                    OD_LOG_S1("response = ", response.asString().c_str());//####
                                }
                            }
                            else
                            {
                                OD_LOG("! (request.send(*newChannel, &response))");//####
                            }
                        }
                        else
                        {
                            OD_LOG("! (Common::NetworkConnectWithRetries(aName, "//####
                                   "MpM_SERVICE_REGISTRY_CHANNEL_NAME))");//####
                        }
#if defined(MpM_DO_EXPLICIT_CLOSE)
                        newChannel->close();
#endif // defined(MpM_DO_EXPLICIT_CLOSE)
                    }
                    else
                    {
                        OD_LOG("! (newChannel->openWithRetries(aName))");//####
                    }
                    Common::ClientChannel::RelinquishChannel(newChannel);
                }
                else
                {
                    OD_LOG("! (newChannel)");//####
                }
            }
            else
            {
                OD_LOG("! (isStarted() && setUpDatabase())");//####
            }
        }
        result = isStarted();
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT_B(result);//####
    return result;
} // RegistryService::start

bool RegistryService::stop(void)
{
    OD_LOG_OBJENTER();//####
    bool result = false;
    
    try
    {
        result = inherited::stop();
        _isActive = false;
        OD_LOG_B1("_isActive <- ", _isActive);//####
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT_B(result);//####
    return result;
} // RegistryService::stop

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
