//--------------------------------------------------------------------------------------
//
//  File:       M+MRegistryService.cpp
//
//  Project:    M+M
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

#include "M+MRegistryService.h"
#include "M+MAssociateRequestHandler.h"
#include "M+MChannelStatusReporter.h"
#include "M+MClientChannel.h"
#include "M+MColumnNameValidator.h"
#include "M+MDisassociateRequestHandler.h"
#include "M+MGeneralChannel.h"
#include "M+MGetAssociatesRequestHandler.h"
#include "M+MMatchExpression.h"
#include "M+MMatchRequestHandler.h"
#include "M+MPingRequestHandler.h"
#include "M+MRegisterRequestHandler.h"
#include "M+MRegistryCheckThread.h"
#include "M+MRequests.h"
#include "M+MServiceRequest.h"
#include "M+MServiceResponse.h"
#include "M+MUnregisterRequestHandler.h"

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
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The class definition for the Service Registry M+M service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Parser;
using namespace MplusM::Registry;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief The named parameter for the 'associate' column. */
#define ASSOCIATE_C_                        "associate"

/*! @brief The named parameter for the 'associates_id' column. */
#define ASSOCIATES_ID_C_                    "associates_id"

/*! @brief The name of the 'Associates' table. */
#define ASSOCIATES_T_                       "Associates"

/*! @brief The name of the index for the 'channelname' column of the 'Channels' table. */
#define CHANNELS_CHANNELNAME_I_             "Channels_channelname_idx"

/*! @brief The named parameter for the 'channels_id' column. */
#define CHANNELS_ID_C_                      "channels_id"

/*! @brief The name of the 'Channels' table. */
#define CHANNELS_T_                         "Channels"

/*! @brief The name of the index for the 'associates_id' column of the 'ChannelsAssociates' table. */
#define CHANNELSASSOCIATES_ASSOCIATES_ID_I_ "ChannelsAssociates_Associates_id_idx"

/*! @brief The name of the index for the 'channels_id' column of the 'ChannelsAssociates' table. */
#define CHANNELSASSOCIATES_CHANNELS_ID_I_   "ChannelsAssociates_Channels_id_idx"

/*! @brief The name of the 'ChannelsAssociates' table. */
#define CHANNELSASSOCIATES_T_               "ChannelsAssociates"

/*! @brief The named parameter for the 'direction' column. */
#define DIRECTION_C_                        "direction"

/*! @brief The name of the index for the 'channelname' column of the 'Requests' table. */
#define REQUESTS_CHANNELNAME_I_             "Requests_channelname_idx"

/*! @brief The name of the index for the 'requests' column of the 'Requests' table. */
#define REQUESTS_REQUEST_I_                 "Requests_request_idx"

/*! @brief The name of the index for the 'keywords_id' column of the 'RequestsKeywords' table. */
#define REQUESTSKEYWORDS_KEYWORDS_ID_I_     "RequestsKeywords_Keywords_id_idx"

/*! @brief The name of the index for the 'requests_id' column of the 'RequestsKeywords' table. */
#define REQUESTSKEYWORDS_REQUESTS_ID_I_     "RequestsKeywords_Requests_id_idx"

/*! @brief The name of the secondary port for the service. */
#define SECONDARY_CHANNEL_NAME_             T_(MpM_REGISTRY_CHANNEL_NAME "/status")

/*! @brief The name of the index for the 'name' column of the 'Services' table. */
#define SERVICES_NAME_I_                    "Services_name_idx"

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
        typedef int (* BindFunction)
                        (sqlite3_stmt * statement,
                         const void *   stuff);
        
        /*! @brief The data needed to add a channel-associates entry into the database. */
        struct ChannelAssociateData
        {
            /*! @brief The primary channel. */
            yarp::os::ConstString _channel;
            
            /*! @brief The channel to be associated. */
            yarp::os::ConstString _associate;
            
            /*! @brief The direction of the associate channel. */
            int                   _direction;
            
        }; // ChannelAssociateData
        
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
            
            /*! @brief The description of the service. */
            yarp::os::ConstString _description;
            
            /*! @brief The path to the executable for the service. */
            yarp::os::ConstString _executable;
            
            /*! @brief The name for the service. */
            yarp::os::ConstString _name;
            
            /*! @brief The description of the requests for the service. */
            yarp::os::ConstString _requestsDescription;
            
        }; // ServiceData
        
    } // Registry
    
} // MplusM

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

/*! @brief Perform a simple operation on the database, ignoring constraint errors.
 @param database The database to be modified.
 @param sqlStatement The operation to be performed.
 @param doBinds A function that will fill in any parameters in the statement.
 @param data The custom information used with the binding function.
 @returns @c true if the operation was successfully performed and @c false otherwise. */
static bool performSQLstatementWithNoResultsAllowConstraint(sqlite3 *    database,
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
                    if ((SQLITE_CONSTRAINT != sqlRes) && (SQLITE_DONE != sqlRes))
                    {
                        OD_LOG("((SQLITE_CONSTRAINT != sqlRes) && (SQLITE_DONE != sqlRes))");//####
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
} // performSQLstatementWithNoResultsAllowConstraint

/*! @brief Perform an operation that can return multiple rows of results.
 @param database The database to be modified.
 @param resultList The list to be filled in with the values from the column of interest.
 @param sqlStatement The operation to be performed.
 @param columnOfInterest1 The column containing the first value of interest.
 @param columnOfInterest2 The column containing the second value of interest.
 @param doBinds A function that will fill in any parameters in the statement.
 @param data The custom information used with the binding function.
 @returns @c true if the operation was successfully performed and @c false otherwise. */
static bool performSQLstatementWithDoubleColumnResults(sqlite3 *         database,
                                                       Common::Package & resultList,
                                                       const char *      sqlStatement,
                                                       const int         columnOfInterest1 = 0,
                                                       const int         columnOfInterest2 = 0,
                                                       BindFunction      doBinds = NULL,
                                                       const void *      data = NULL)
{
    OD_LOG_ENTER();//####
    OD_LOG_P3("database = ", database, "resultList = ", &resultList, "data = ", data);//####
    OD_LOG_LL2("columnOfInterest1 = ", columnOfInterest1, "columnOfInterest2 = ", columnOfInterest2);//####
    OD_LOG_S1("sqlStatement = ", sqlStatement);//####
    bool okSoFar = true;
    
    try
    {
        if (database && (0 <= columnOfInterest1) && (0 <= columnOfInterest2))
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
                            if ((0 < colCount) && (columnOfInterest1 < colCount) && (columnOfInterest2 < colCount))
                            {
                                Common::Package & subList = resultList.addList();
                                const char *      value = reinterpret_cast<const char *>(sqlite3_column_text(prepared,
                                                                                                    columnOfInterest1));
                                
                                OD_LOG_S1("value <- ", value);//####
                                if (value)
                                {
                                    subList.addString(value);
                                }
                                value = reinterpret_cast<const char *>(sqlite3_column_text(prepared,
                                                                                           columnOfInterest2));
                                OD_LOG_S1("value <- ", value);//####
                                if (value)
                                {
                                    subList.addString(value);
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
            OD_LOG("! (database && (0 <= columnOfInterest1) && (0 <= columnOfInterest2))");//####
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
} // performSQLstatementWithDoubleColumnResults

/*! @brief Perform an operation that can return multiple rows of results.
 @param database The database to be modified.
 @param resultList The list to be filled in with the values from the column of interest.
 @param sqlStatement The operation to be performed.
 @param columnOfInterest The column containing the value of interest.
 @param doBinds A function that will fill in any parameters in the statement.
 @param data The custom information used with the binding function.
 @returns @c true if the operation was successfully performed and @c false otherwise. */
static bool performSQLstatementWithSingleColumnResults(sqlite3 *         database,
                                                       Common::Package & resultList,
                                                       const char *      sqlStatement,
                                                       const int         columnOfInterest = 0,
                                                       BindFunction      doBinds = NULL,
                                                       const void *      data = NULL)
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
} // performSQLstatementWithSingleColumnResults

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
#if defined(MpM_UseTestDatabase)
                    T_("DROP INDEX IF EXISTS " CHANNELS_CHANNELNAME_I_),
                    T_("DROP INDEX IF EXISTS " CHANNELSASSOCIATES_ASSOCIATES_ID_I_),
                    T_("DROP INDEX IF EXISTS " CHANNELSASSOCIATES_CHANNELS_ID_I_),
                    T_("DROP INDEX IF EXISTS " REQUESTS_REQUEST_I_),
                    T_("DROP INDEX IF EXISTS " REQUESTS_CHANNELNAME_I_),
                    T_("DROP INDEX IF EXISTS " REQUESTSKEYWORDS_KEYWORDS_ID_I_),
                    T_("DROP INDEX IF EXISTS " REQUESTSKEYWORDS_REQUESTS_ID_I_),
                    T_("DROP INDEX IF EXISTS " SERVICES_NAME_I_),
                    T_("DROP TABLE IF EXISTS " ASSOCIATES_T_),
                    T_("DROP TABLE IF EXISTS " CHANNELS_T_),
                    T_("DROP TABLE IF EXISTS " CHANNELSASSOCIATES_T_),
                    T_("DROP TABLE IF EXISTS " REQUESTSKEYWORDS_T_),
                    T_("DROP TABLE IF EXISTS " REQUESTS_T_),
                    T_("DROP TABLE IF EXISTS " KEYWORDS_T_),
                    T_("DROP TABLE IF EXISTS " SERVICES_T_),
#endif // defined(MpM_UseTestDatabase)
                    T_("CREATE TABLE IF NOT EXISTS " SERVICES_T_ "( " CHANNELNAME_C_
                       " Text NOT NULL DEFAULT _ PRIMARY KEY ON CONFLICT REPLACE, " NAME_C_
                       " Text NOT NULL DEFAULT _, " DESCRIPTION_C_ " Text NOT NULL DEFAULT _, " EXECUTABLE_C_
                       " Text NOT NULL DEFAULT _, " REQUESTSDESCRIPTION_C_ " Text NOT NULL DEFAULT _)"),
                    T_("CREATE INDEX IF NOT EXISTS " SERVICES_NAME_I_ " ON " SERVICES_T_ "(" NAME_C_ ")"),
                    T_("CREATE TABLE IF NOT EXISTS " KEYWORDS_T_ "( " KEYWORD_C_
                       " Text NOT NULL DEFAULT _ PRIMARY KEY ON CONFLICT IGNORE)"),
                    T_("CREATE TABLE IF NOT EXISTS " REQUESTS_T_ "( " CHANNELNAME_C_
                       " Text NOT NULL DEFAULT _ REFERENCES " SERVICES_T_ "(" CHANNELNAME_C_ "), " REQUEST_C_
                       " Text NOT NULL DEFAULT _, " INPUT_C_ " Text, " OUTPUT_C_ " Text, " VERSION_C_ " Text, "
                       DETAILS_C_ " Text, " KEY_C_ " Integer PRIMARY KEY)"),
                    T_("CREATE INDEX IF NOT EXISTS " REQUESTS_REQUEST_I_ " ON " REQUESTS_T_ "(" REQUEST_C_ ")"),
                    T_("CREATE INDEX IF NOT EXISTS " REQUESTS_CHANNELNAME_I_ " ON " REQUESTS_T_ "(" CHANNELNAME_C_ ")"),
                    T_("CREATE TABLE IF NOT EXISTS " REQUESTSKEYWORDS_T_ "( " KEYWORDS_ID_C_ " Text REFERENCES "
                       KEYWORDS_T_ "(" KEYWORD_C_ "), " REQUESTS_ID_C_ " Integer REFERENCES " REQUESTS_T_ "(" KEY_C_
                       "))"),
                    T_("CREATE INDEX IF NOT EXISTS " REQUESTSKEYWORDS_KEYWORDS_ID_I_ " ON " REQUESTSKEYWORDS_T_ "("
                       KEYWORDS_ID_C_ ")"),
                    T_("CREATE INDEX IF NOT EXISTS " REQUESTSKEYWORDS_REQUESTS_ID_I_ " ON " REQUESTSKEYWORDS_T_ "("
                       REQUESTS_ID_C_ ")"),
                    T_("CREATE TABLE IF NOT EXISTS " CHANNELS_T_ "( " CHANNELNAME_C_ " Text NOT NULL UNIQUE DEFAULT _, "
                       KEY_C_ " Integer PRIMARY KEY)"),
                    T_("CREATE INDEX IF NOT EXISTS " CHANNELS_CHANNELNAME_I_ " ON " CHANNELS_T_ "(" CHANNELNAME_C_ ")"),
                    T_("CREATE TABLE IF NOT EXISTS " ASSOCIATES_T_ "( " ASSOCIATE_C_
                       " Text NOT NULL DEFAULT _ PRIMARY KEY ON CONFLICT IGNORE)"),
                    T_("CREATE TABLE IF NOT EXISTS " CHANNELSASSOCIATES_T_ "( " CHANNELS_ID_C_ " Integer REFERENCES "
                       CHANNELS_T_ "(" KEY_C_ "), " ASSOCIATES_ID_C_ " Text REFERENCES " ASSOCIATES_T_ "(" ASSOCIATE_C_
                       "), " DIRECTION_C_ " Integer)"),
                    T_("CREATE INDEX IF NOT EXISTS " CHANNELSASSOCIATES_ASSOCIATES_ID_I_ " ON " CHANNELSASSOCIATES_T_
                       "(" ASSOCIATES_ID_C_ ")"),
                    T_("CREATE INDEX IF NOT EXISTS " CHANNELSASSOCIATES_CHANNELS_ID_I_ " ON " CHANNELSASSOCIATES_T_ "("
                       CHANNELS_ID_C_ ")")
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

/*! @brief Bind the values that are to be gathered from the Services table.
 @param statement The prepared statement that is to be updated.
 @param stuff The source of data that is to be bound.
 @returns The SQLite error from the bind operation. */
static int setupCheckChannel(sqlite3_stmt * statement,
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
            const char * channelNameString = static_cast<const char *>(stuff);
            
            OD_LOG_S1("channelNameString <- ", channelNameString);//####
            result = sqlite3_bind_text(statement, channelNameIndex, channelNameString,
                                       static_cast<int>(strlen(channelNameString)), SQLITE_TRANSIENT);
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
} // setupCheckChannel

/*! @brief Bind the values that are to be gathered from the Associates table.
 @param statement The prepared statement that is to be updated.
 @param stuff The source of data that is to be bound.
 @returns The SQLite error from the bind operation. */
static int setupCollectAssociates(sqlite3_stmt * statement,
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
            const char * channelNameString = static_cast<const char *>(stuff);
            
            OD_LOG_S1("channelNameString <- ", channelNameString);//####
            result = sqlite3_bind_text(statement, channelNameIndex, channelNameString,
                                       static_cast<int>(strlen(channelNameString)), SQLITE_TRANSIENT);
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
} // setupCollectAssociates

/*! @brief Bind the values that are to be gathered from the Channels table.
 @param statement The prepared statement that is to be updated.
 @param stuff The source of data that is to be bound.
 @returns The SQLite error from the bind operation. */
static int setupCollectPrimaries(sqlite3_stmt * statement,
                                  const void *   stuff)
{
    OD_LOG_ENTER();//####
    OD_LOG_P2("statement = ", statement, "stuff = ", stuff);//####
    int result = SQLITE_MISUSE;
    
    try
    {
        int associatesIdIndex = sqlite3_bind_parameter_index(statement, "@" ASSOCIATES_ID_C_);
        
        if (0 < associatesIdIndex)
        {
            const char * associatesIdString = static_cast<const char *>(stuff);
            
            OD_LOG_S1("associatesIdString <- ", associatesIdString);//####
            result = sqlite3_bind_text(statement, associatesIdIndex, associatesIdString,
                                       static_cast<int>(strlen(associatesIdString)), SQLITE_TRANSIENT);
            if (SQLITE_OK != result)
            {
                OD_LOG_S1("error description: ", sqlite3_errstr(result));//####
            }
        }
        else
        {
            OD_LOG("! (0 < associatesIdIndex)");//####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_EXIT_LL(result);
    return result;
} // setupCollectPrimaries

/*! @brief Bind the values that are to be inserted into the Associates table.
 @param statement The prepared statement that is to be updated.
 @param stuff The source of data that is to be bound.
 @returns The SQLite error from the bind operation. */
static int setupInsertIntoAssociates(sqlite3_stmt * statement,
                                     const void *   stuff)
{
    OD_LOG_ENTER();//####
    OD_LOG_P2("statement = ", statement, "stuff = ", stuff);//####
    int result = SQLITE_MISUSE;
    
    try
    {
        int associateIndex = sqlite3_bind_parameter_index(statement, "@" ASSOCIATE_C_);
        
        if (0 < associateIndex)
        {
            const char * associateString = static_cast<const char *>(stuff);
            
            OD_LOG_S1("associateString <- ", associateString);//####
            result = sqlite3_bind_text(statement, associateIndex, associateString,
                                       static_cast<int>(strlen(associateString)), SQLITE_TRANSIENT);
            if (SQLITE_OK != result)
            {
                OD_LOG_S1("error description: ", sqlite3_errstr(result));//####
            }
        }
        else
        {
            OD_LOG("! (0 < associateIndex)");//####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_EXIT_LL(result);
    return result;
} // setupInsertIntoAssociates

/*! @brief Bind the values that are to be inserted into the Channels table.
 @param statement The prepared statement that is to be updated.
 @param stuff The source of data that is to be bound.
 @returns The SQLite error from the bind operation. */
static int setupInsertIntoChannels(sqlite3_stmt * statement,
                                   const void *   stuff)
{
    OD_LOG_ENTER();//####
    OD_LOG_P2("statement = ", statement, "stuff = ", stuff);//####
    int result = SQLITE_MISUSE;
    
    try
    {
        int channelIndex = sqlite3_bind_parameter_index(statement, "@" CHANNELNAME_C_);
        
        if (0 < channelIndex)
        {
            const char * channelString = static_cast<const char *>(stuff);
            
            OD_LOG_S1("channelString <- ", channelString);//####
            result = sqlite3_bind_text(statement, channelIndex, channelString, static_cast<int>(strlen(channelString)),
                                       SQLITE_TRANSIENT);
            if (SQLITE_OK != result)
            {
                OD_LOG_S1("error description: ", sqlite3_errstr(result));//####
            }
        }
        else
        {
            OD_LOG("! (0 < channelIndex)");//####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_EXIT_LL(result);
    return result;
} // setupInsertIntoChannels

/*! @brief Bind the values that are to be inserted into the ChannelsAssociates table.
 @param statement The prepared statement that is to be updated.
 @param stuff The source of data that is to be bound.
 @returns The SQLite error from the bind operation. */
static int setupInsertIntoChannelsAssociates(sqlite3_stmt * statement,
                                             const void *   stuff)
{
    OD_LOG_ENTER();//####
    OD_LOG_P2("statement = ", statement, "stuff = ", stuff);//####
    int result = SQLITE_MISUSE;
    
    try
    {
        int associateIndex = sqlite3_bind_parameter_index(statement, "@" ASSOCIATE_C_);
        int channelIndex = sqlite3_bind_parameter_index(statement, "@" CHANNELNAME_C_);
        int directionIndex = sqlite3_bind_parameter_index(statement, "@" DIRECTION_C_);
        
        if ((0 < associateIndex) && (0 < channelIndex) && (0 < directionIndex))
        {
            const ChannelAssociateData * descriptor = static_cast<const ChannelAssociateData *>(stuff);
            const char *                channelName = descriptor->_channel.c_str();
            
            OD_LOG_S1("channelName <- ", channelName);//####
            result = sqlite3_bind_text(statement, channelIndex, channelName, static_cast<int>(strlen(channelName)),
                                       SQLITE_TRANSIENT);
            if (SQLITE_OK == result)
            {
                const char * associateName = descriptor->_associate.c_str();
                
                OD_LOG_S1("associateName <- ", associateName);//####
                result = sqlite3_bind_text(statement, associateIndex, associateName,
                                           static_cast<int>(strlen(associateName)), SQLITE_TRANSIENT);
            }
            if (SQLITE_OK == result)
            {
                const char * directionString = (descriptor->_direction ? "1" : "0");
                
                OD_LOG_S1("directionString <- ", directionString);//####
                result = sqlite3_bind_text(statement, directionIndex, directionString,
                                           static_cast<int>(strlen(directionString)), SQLITE_TRANSIENT);
            }
        }
        else
        {
            OD_LOG("! ((0 < associateIndex) && (0 < channelIndex) && (0 < directionIndex))");//####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_EXIT_LL(result);
    return result;
} // setupInsertIntoChannelsAssociates

/*! @brief Bind the values that are to be inserted into the Keywords table.
 @param statement The prepared statement that is to be updated.
 @param stuff The source of data that is to be bound.
 @returns The SQLite error from the bind operation. */
static int setupInsertIntoKeywords(sqlite3_stmt * statement,
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
} // setupInsertIntoKeywords

/*! @brief Bind the values that are to be inserted into the Requests table.
 @param statement The prepared statement that is to be updated.
 @param stuff The source of data that is to be bound.
 @returns The SQLite error from the bind operation. */
static int setupInsertIntoRequests(sqlite3_stmt * statement,
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
} // setupInsertIntoRequests

/*! @brief Bind the values that are to be inserted into the RequestsKeywords table.
 @param statement The prepared statement that is to be updated.
 @param stuff The source of data that is to be bound.
 @returns The SQLite error from the bind operation. */
static int setupInsertIntoRequestsKeywords(sqlite3_stmt * statement,
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
} // setupInsertIntoRequestsKeywords

/*! @brief Bind the values that are to be inserted into the Services table.
 @param statement The prepared statement that is to be updated.
 @param stuff The source of data that is to be bound.
 @returns The SQLite error from the bind operation. */
static int setupInsertIntoServices(sqlite3_stmt * statement,
                                   const void *   stuff)
{
    OD_LOG_ENTER();//####
    OD_LOG_P2("statement = ", statement, "stuff = ", stuff);//####
    int result = SQLITE_MISUSE;

    try
    {
        int channelNameIndex = sqlite3_bind_parameter_index(statement, "@" CHANNELNAME_C_);
        int descriptionIndex = sqlite3_bind_parameter_index(statement, "@" DESCRIPTION_C_);
        int executableIndex = sqlite3_bind_parameter_index(statement, "@" EXECUTABLE_C_);
        int nameIndex = sqlite3_bind_parameter_index(statement, "@" NAME_C_);
        int requestsDescriptionIndex = sqlite3_bind_parameter_index(statement, "@" REQUESTSDESCRIPTION_C_);
        
        if ((0 < channelNameIndex) && (0 < descriptionIndex) && (0 < executableIndex) && (0 < nameIndex) &&
            (0 < requestsDescriptionIndex))
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
            if (SQLITE_OK == result)
            {
                const char * executable = descriptor->_executable.c_str();
                
                OD_LOG_S1("executable <- ", executable);//####
                result = sqlite3_bind_text(statement, executableIndex, executable,
                                           static_cast<int>(strlen(executable)), SQLITE_TRANSIENT);
            }
            if (SQLITE_OK == result)
            {
                const char * requestsDescription = descriptor->_requestsDescription.c_str();
                
                OD_LOG_S1("requestsDescription <- ", requestsDescription);//####
                result = sqlite3_bind_text(statement, requestsDescriptionIndex, requestsDescription,
                                           static_cast<int>(strlen(requestsDescription)), SQLITE_TRANSIENT);
            }
            if (SQLITE_OK != result)
            {
                OD_LOG_S1("error description: ", sqlite3_errstr(result));//####
            }
        }
        else
        {
            OD_LOG("! ((0 < channelNameIndex) && (0 < descriptionIndex) && (0 < executableIndex) && (0 < nameIndex) && "
                   "(0 < requestsDescriptionIndex))");//####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_EXIT_LL(result);
    return result;
} // setupInsertIntoServices

/*! @brief Bind the values that are to be removed from the Associates table.
 @param statement The prepared statement that is to be updated.
 @param stuff The source of data that is to be bound.
 @returns The SQLite error from the bind operation. */
static int setupRemoveFromAssociates(sqlite3_stmt * statement,
                                     const void *   stuff)
{
    OD_LOG_ENTER();//####
    OD_LOG_P2("statement = ", statement, "stuff = ", stuff);//####
    int result = SQLITE_MISUSE;
    
    try
    {
        int associateIndex = sqlite3_bind_parameter_index(statement, "@" ASSOCIATE_C_);
        
        if (0 < associateIndex)
        {
            const char * anAssociate = static_cast<const char *>(stuff);
            
            result = sqlite3_bind_text(statement, associateIndex, anAssociate, static_cast<int>(strlen(anAssociate)),
                                       SQLITE_TRANSIENT);
            if (SQLITE_OK != result)
            {
                OD_LOG_S1("error description: ", sqlite3_errstr(result));//####
            }
        }
        else
        {
            OD_LOG("! (0 < associateIndex)");//####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_EXIT_LL(result);
    return result;
} // setupRemoveFromAssociates

/*! @brief Bind the values that are to be removed from the Channels table.
 @param statement The prepared statement that is to be updated.
 @param stuff The source of data that is to be bound.
 @returns The SQLite error from the bind operation. */
static int setupRemoveFromChannels(sqlite3_stmt * statement,
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
} // setupRemoveFromChannels

/*! @brief Bind the values that are to be removed from the ChannelsAssociates table.
 @param statement The prepared statement that is to be updated.
 @param stuff The source of data that is to be bound.
 @returns The SQLite error from the bind operation. */
static int setupRemoveFromChannelsAssociates(sqlite3_stmt * statement,
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
} // setupRemoveFromChannelsAssociates

/*! @brief Bind the values that are to be removed from the Requests table.
 @param statement The prepared statement that is to be updated.
 @param stuff The source of data that is to be bound.
 @returns The SQLite error from the bind operation. */
static int setupRemoveFromRequests(sqlite3_stmt * statement,
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
} // setupRemoveFromRequests

/*! @brief Bind the values that are to be removed from the RequestsKeywords table.
 @param statement The prepared statement that is to be updated.
 @param stuff The source of data that is to be bound.
 @returns The SQLite error from the bind operation. */
static int setupRemoveFromRequestsKeywords(sqlite3_stmt * statement,
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
} // setupRemoveFromRequestsKeywords

/*! @brief Bind the values that are to be removed from the Services table.
 @param statement The prepared statement that is to be updated.
 @param stuff The source of data that is to be bound.
 @returns The SQLite error from the bind operation. */
static int setupRemoveFromServices(sqlite3_stmt * statement,
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
} // setupRemoveFromServices

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and destructors
#endif // defined(__APPLE__)

RegistryService::RegistryService(const char *                  launchPath,
                                 const bool                    useInMemoryDb,
                                 const yarp::os::ConstString & servicePortNumber) :
        inherited(kServiceKindRegistry, launchPath, true, MpM_REGISTRY_CANONICAL_NAME, "The Service Registry service",
                  "associate - associate a channel with another channel\n"
                  "disassociate - remove all associations for a channel\n"
                  "getAssociates - return the associations of a channel\n"
                  "match - return the channels for services matching the criteria provided\n"
                  "ping - update the last-pinged information for a channel or record the information for a service on "
                      "the given channel\n"
                  "register - record the information for a service on the given channel\n"
                  "unregister - remove the information for a service on the given channel", MpM_REGISTRY_CHANNEL_NAME,
                  servicePortNumber), _db(NULL), _validator(new ColumnNameValidator), _associateHandler(NULL),
        _disassociateHandler(NULL), _getAssociatesHandler(NULL), _matchHandler(NULL), _pingHandler(NULL),
        _statusChannel(NULL), _registerHandler(NULL), _unregisterHandler(NULL), _checker(NULL),
        _inMemory(useInMemoryDb), _isActive(false)
{
    OD_LOG_ENTER();//####
    OD_LOG_S2("launchPath = ", launchPath, "servicePortNumber = ", servicePortNumber.c_str());//####
    OD_LOG_B1("useInMemoryDb = ", useInMemoryDb);//####
    attachRequestHandlers();
    OD_LOG_EXIT_P(this);//####
} // RegistryService::RegistryService

RegistryService::~RegistryService(void)
{
    OD_LOG_OBJENTER();//####
    detachRequestHandlers();
    _lastCheckedTime.clear();
    if (_db)
    {
        sqlite3_close(_db);
    }
    delete _validator;
    if (_statusChannel)
    {
#if defined(MpM_DoExplicitClose)
        _statusChannel->close();
#endif // defined(MpM_DoExplicitClose)
        Common::GeneralChannel::RelinquishChannel(_statusChannel);
        _statusChannel = NULL;
    }
    OD_LOG_OBJEXIT();//####
} // RegistryService::~RegistryService

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

bool RegistryService::addAssociation(const yarp::os::ConstString & primaryChannelName,
                                     const bool                    isOutput,
                                     const yarp::os::ConstString & secondaryChannelName)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_S2("primaryChannelName = ", primaryChannelName.c_str(), "secondaryChannelName = ",//####
              secondaryChannelName.c_str());//####
    OD_LOG_B1("isOutput = ", isOutput);//####
    bool okSoFar = false;
    
    try
    {
        if (performSQLstatementWithNoResults(_db, kBeginTransaction))
        {
            static const char * insertIntoAssociates = T_("INSERT INTO " ASSOCIATES_T_ "(" ASSOCIATE_C_ ") VALUES(@"
                                                          ASSOCIATE_C_ ")");
            
            okSoFar = performSQLstatementWithNoResults(_db, insertIntoAssociates, setupInsertIntoAssociates,
                                                       static_cast<const void *>(secondaryChannelName.c_str()));
            
            if (okSoFar)
            {
                static const char * insertIntoChannels = T_("INSERT INTO " CHANNELS_T_ "(" CHANNELNAME_C_ ") VALUES(@"
                                                            CHANNELNAME_C_ ")");

                okSoFar = performSQLstatementWithNoResultsAllowConstraint(_db, insertIntoChannels,
                                                                          setupInsertIntoChannels,
                                                               static_cast<const void *>(primaryChannelName.c_str()));
            }
            if (okSoFar)
            {
                ChannelAssociateData chanAssocData;
                static const char *  insertIntoChannelsAssociates = T_("INSERT INTO " CHANNELSASSOCIATES_T_ "("
                                                                       ASSOCIATES_ID_C_ "," CHANNELS_ID_C_ ","
                                                                       DIRECTION_C_ ") SELECT @" ASSOCIATE_C_ ", "
                                                                       KEY_C_ ", @" DIRECTION_C_ " FROM "
                                                                       CHANNELS_T_ " WHERE " CHANNELNAME_C_ " = @"
                                                                       CHANNELNAME_C_);
                
                chanAssocData._channel = primaryChannelName;
                chanAssocData._associate = secondaryChannelName;
                chanAssocData._direction = (isOutput ? 1 : 0);
                okSoFar = performSQLstatementWithNoResults(_db, insertIntoChannelsAssociates,
                                                           setupInsertIntoChannelsAssociates, &chanAssocData);
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
} // RegistryService::::addAssociation

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
            okSoFar = performSQLstatementWithNoResults(_db, insertIntoRequests, setupInsertIntoRequests,
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
                        okSoFar = performSQLstatementWithNoResults(_db, insertIntoKeywords, setupInsertIntoKeywords,
                                                                   static_cast<const void *>(reqKeyData._key.c_str()));
                        if (okSoFar)
                        {
                            okSoFar = performSQLstatementWithNoResults(_db, insertIntoRequestsKeywords,
                                                                       setupInsertIntoRequestsKeywords, &reqKeyData);
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
                                       const yarp::os::ConstString & description,
                                       const yarp::os::ConstString & executable,
                                       const yarp::os::ConstString & requestsDescription)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_S4("channelName = ", channelName.c_str(), "name = ", name.c_str(), "description = ",//####
              description.c_str(), "executable = ", executable.c_str());//####
    OD_LOG_S1("requestsDescription = ", requestsDescription.c_str());//####
    bool okSoFar = false;
    
    try
    {
        if (performSQLstatementWithNoResults(_db, kBeginTransaction))
        {
            // Add the service channel name.
            static const char * insertIntoServices = T_("INSERT INTO " SERVICES_T_ "(" CHANNELNAME_C_ "," NAME_C_ ","
                                                        DESCRIPTION_C_ "," EXECUTABLE_C_ "," REQUESTSDESCRIPTION_C_
                                                        ") VALUES(@" CHANNELNAME_C_ ",@" NAME_C_ ",@" DESCRIPTION_C_
                                                        ",@" EXECUTABLE_C_ ",@" REQUESTSDESCRIPTION_C_ ")");
            ServiceData         servData;
            
            servData._channel = channelName;
            servData._name = name;
            servData._description = description;
            servData._executable = executable;
            servData._requestsDescription = requestsDescription;
            okSoFar = performSQLstatementWithNoResults(_db, insertIntoServices, setupInsertIntoServices,
                                                       static_cast<const void *>(&servData));
            if (okSoFar)
            {
                okSoFar = performSQLstatementWithNoResults(_db, kEndTransaction);
            }
            else
            {
                performSQLstatementWithNoResults(_db, kRollbackTransaction);
            }
            reportStatusChange(channelName, kRegistryAddService);
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
        _associateHandler = new AssociateRequestHandler(*this);
        _disassociateHandler = new DisassociateRequestHandler(*this);
        _getAssociatesHandler = new GetAssociatesRequestHandler(*this);
        _matchHandler = new MatchRequestHandler(*this, _validator);
        _pingHandler = new PingRequestHandler(*this);
        _registerHandler = new RegisterRequestHandler(*this);
        _unregisterHandler = new UnregisterRequestHandler(*this);
        if (_associateHandler && _disassociateHandler && _getAssociatesHandler && _matchHandler && _pingHandler &&
            _registerHandler && _unregisterHandler)
        {
            registerRequestHandler(_associateHandler);
            registerRequestHandler(_disassociateHandler);
            registerRequestHandler(_getAssociatesHandler);
            registerRequestHandler(_matchHandler);
            registerRequestHandler(_pingHandler);
            registerRequestHandler(_registerHandler);
            registerRequestHandler(_unregisterHandler);
        }
        else
        {
            OD_LOG("! (_associateHandler && _disassociateHandler && _getAssociatesHandler && _matchHandler && "
                   "_pingHandler && _registerHandler && _unregisterHandler)");//####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT();//####
} // RegistryService::attachRequestHandlers

bool RegistryService::checkForExistingService(const yarp::os::ConstString & channelName)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_S1("channelName = ", channelName.c_str());//####
    bool okSoFar = false;
    
    try
    {
        if (performSQLstatementWithNoResults(_db, kBeginTransaction))
        {
            Common::Package dummy;
            const char *    checkName = T_("SELECT DISTINCT " NAME_C_ " FROM " SERVICES_T_
                                           " WHERE " CHANNELNAME_C_ " = @" CHANNELNAME_C_);

            okSoFar = performSQLstatementWithSingleColumnResults(_db, dummy, checkName, 0, setupCheckChannel,
                                                                 static_cast<const void *>(channelName.c_str()));
            if (okSoFar)
            {
                okSoFar = performSQLstatementWithNoResults(_db, kEndTransaction);
            }
            else
            {
                performSQLstatementWithNoResults(_db, kRollbackTransaction);
            }
            if (okSoFar)
            {
                okSoFar = (1 <= dummy.size());
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
} // RegistryService::checkForExistingService

void RegistryService::checkServiceTimes(void)
{
    OD_LOG_OBJENTER();//####
    double       now = yarp::os::Time::now();
    StringVector expired;
    
    // Build a list of expired services.
    _checkedTimeLock.lock();
    for (TimeMap::const_iterator walker(_lastCheckedTime.begin()); _lastCheckedTime.end() != walker; ++walker)
    {
        double check = walker->second;
        
        if (now > check)
        {
            expired.push_back(walker->first);
        }
    }
    _checkedTimeLock.unlock();
    for (StringVector::const_iterator walker(expired.begin()); expired.end() != walker; ++walker)
    {
        if (removeServiceRecord(*walker))
        {
            removeCheckedTimeForChannel(*walker);
        }
    }
    OD_LOG_OBJEXIT();//####
} // RegistryService::checkServiceTimes

void RegistryService::detachRequestHandlers(void)
{
    OD_LOG_OBJENTER();//####
    try
    {
        if (_associateHandler)
        {
            unregisterRequestHandler(_associateHandler);
            delete _associateHandler;
            _associateHandler = NULL;
        }
        if (_disassociateHandler)
        {
            unregisterRequestHandler(_disassociateHandler);
            delete _disassociateHandler;
            _disassociateHandler = NULL;
        }
        if (_getAssociatesHandler)
        {
            unregisterRequestHandler(_getAssociatesHandler);
            delete _getAssociatesHandler;
            _getAssociatesHandler = NULL;
        }
        if (_matchHandler)
        {
            unregisterRequestHandler(_matchHandler);
            delete _matchHandler;
            _matchHandler = NULL;
        }
        if (_pingHandler)
        {
            unregisterRequestHandler(_pingHandler);
            delete _pingHandler;
            _pingHandler = NULL;
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

bool RegistryService::fillInAssociates(const yarp::os::ConstString & channelName,
                                       bool &                        isPrimary,
                                       Common::StringVector &        inputs,
                                       Common::StringVector &        outputs)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_S1("channelName = ", channelName.c_str());//####
    OD_LOG_P3("isPrimary = ", isPrimary, "inputs = ", &inputs, "outputs = ", &outputs);//####
    bool okSoFar = false;
    
    try
    {
        isPrimary = false;
        inputs.clear();
        outputs.clear();
        if (performSQLstatementWithNoResults(_db, kBeginTransaction))
        {
            // First, check if the channel name is a primary:
            Common::Package dummy;
            const char *    collectPrimaries1 = T_("SELECT " KEY_C_ " FROM " CHANNELS_T_ " WHERE " CHANNELNAME_C_ " = @"
                                                   CHANNELNAME_C_);
            
            okSoFar = performSQLstatementWithSingleColumnResults(_db, dummy, collectPrimaries1, 0,
                                                                 setupCollectAssociates,
                                                                 static_cast<const void *>(channelName.c_str()));
            if (okSoFar)
            {
                isPrimary = (0 < dummy.size());
                if (isPrimary)
                {
                    // Second, gather a list of the associated channels:
                    Common::Package associatesList;
                    const char *    collectAssociates = T_("SELECT " ASSOCIATES_ID_C_ ", " DIRECTION_C_ " FROM "
                                                           CHANNELSASSOCIATES_T_ " WHERE " CHANNELS_ID_C_ " IN (SELECT "
                                                           KEY_C_ " FROM " CHANNELS_T_ " WHERE " CHANNELNAME_C_ " = @"
                                                           CHANNELNAME_C_ ")");
                    
                    okSoFar = performSQLstatementWithDoubleColumnResults(_db, associatesList, collectAssociates, 0, 1,
                                                                         setupCollectAssociates,
                                                                     static_cast<const void *>(channelName.c_str()));
                    if (okSoFar)
                    {
                        for (int ii = 0, numAssociates = associatesList.size(); okSoFar && (ii < numAssociates); ++ii)
                        {
                            yarp::os::Value & associatePair(associatesList.get(ii));
                            
                            if (associatePair.isList())
                            {
                                Common::Package * associateList = associatePair.asList();
                                
                                if (2 == associateList->size())
                                {
                                    yarp::os::Value associateName(associateList->get(0));
                                    yarp::os::Value associateDirection(associateList->get(1));
                                    
                                    if (associateName.isString() && associateDirection.isString())
                                    {
                                        yarp::os::ConstString nameAsString(associateName.toString());
                                        yarp::os::ConstString directionAsString(associateDirection.toString());
                                        
                                        if (directionAsString == "0")
                                        {
                                            inputs.push_back(nameAsString.c_str());
                                        }
                                        else
                                        {
                                            outputs.push_back(nameAsString.c_str());
                                        }
                                    }
                                    else
                                    {
                                        OD_LOG("! (associateName.isString() && associateDirection.isString())");//####
                                        okSoFar = false;
                                    }
                                }
                                else
                                {
                                    OD_LOG("! (2 == associateList->size())");//####
                                    okSoFar = false;
                                }
                            }
                        }
                    }
                }
                else
                {
                    const char * collectPrimaries2 = T_("SELECT " CHANNELNAME_C_ " FROM " CHANNELS_T_ " WHERE " KEY_C_
                                                        " IN (SELECT " CHANNELS_ID_C_ " FROM " CHANNELSASSOCIATES_T_
                                                        " WHERE " ASSOCIATES_ID_C_ " = @" ASSOCIATES_ID_C_ ")");
                    
                    okSoFar = performSQLstatementWithSingleColumnResults(_db, dummy, collectPrimaries2, 0,
                                                                         setupCollectPrimaries,
                                                                     static_cast<const void *>(channelName.c_str()));
                    if (okSoFar)
                    {
                        if (1 == dummy.size())
                        {
                            yarp::os::Value & primary(dummy.get(0));
                            
                            if (primary.isString())
                            {
                                inputs.push_back(primary.toString().c_str());
                            }
                            else
                            {
                                OD_LOG("! (primary.isString())");//####
                                okSoFar = false;
                            }
                        }
                        else
                        {
                            OD_LOG("! (1 == dummy.size())");//####
                            okSoFar = false;
                        }
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
} // RegistryService::fillInAssociates

void RegistryService::fillInSecondaryOutputChannelsList(ChannelVector & channels)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_P1("channels = ", &channels);//####
    inherited::fillInSecondaryOutputChannelsList(channels);
    if (_statusChannel)
    {
        ChannelDescription descriptor;
     
        descriptor._portName = _statusChannel->name();
        descriptor._portProtocol = _statusChannel->protocol();
        descriptor._portMode = kChannelModeTCP;
        channels.push_back(descriptor);
    }
    OD_LOG_OBJEXIT();//####
} // RegistryService::fillInSecondaryOutputChannelsList

bool RegistryService::processMatchRequest(Parser::MatchExpression * matcher,
                                          const bool                getNames,
                                          Common::Package &         reply)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_P1("matcher = ", matcher);//####
    OD_LOG_B1("getNames = ", getNames);//####
    bool okSoFar = false;
    
    try
    {
        if (matcher)
        {
            if (performSQLstatementWithNoResults(_db, kBeginTransaction))
            {
                Common::Package &     subList = reply.addList();
                const char *          sqlStart = (getNames ? T_("SELECT DISTINCT " NAME_C_ " FROM " SERVICES_T_
                                                                " WHERE " CHANNELNAME_C_ " IN (SELECT DISTINCT "
                                                                CHANNELNAME_C_ " FROM " REQUESTS_T_ " WHERE ") :
                                                  T_("SELECT DISTINCT " CHANNELNAME_C_ " FROM " REQUESTS_T_ " WHERE "));
                const char *          sqlEnd = (getNames ? T_(")") : T_(""));
                yarp::os::ConstString requestAsSQL(matcher->asSQLString(sqlStart, sqlEnd));
                
                OD_LOG_S1("requestAsSQL <- ", requestAsSQL.c_str());//####
                okSoFar = performSQLstatementWithSingleColumnResults(_db, subList, requestAsSQL.c_str());
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

bool RegistryService::removeAllAssociations(const yarp::os::ConstString & primaryChannelName)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_S1("primaryChannelName = ", primaryChannelName.c_str());//####
    bool okSoFar = false;
    
    try
    {
        if (performSQLstatementWithNoResults(_db, kBeginTransaction))
        {
            // First, gather a list of the associated channels:
            Common::Package associatesList;
            const char *    collectAssociates = T_("SELECT " ASSOCIATES_ID_C_ " FROM " CHANNELSASSOCIATES_T_ " WHERE "
                                                   CHANNELS_ID_C_ " IN (SELECT " KEY_C_ " FROM " CHANNELS_T_ " WHERE "
                                                   CHANNELNAME_C_ " = @" CHANNELNAME_C_ ")");

            okSoFar = performSQLstatementWithSingleColumnResults(_db, associatesList, collectAssociates, 0,
                                                                 setupCollectAssociates,
                                                                 static_cast<const void *>(primaryChannelName.c_str()));
            if (okSoFar)
            {
                static const char * removeFromChannelsAssociates = T_("DELETE FROM " CHANNELSASSOCIATES_T_ " WHERE "
                                                                      CHANNELS_ID_C_ " IN (SELECT " KEY_C_ " FROM "
                                                                      CHANNELS_T_ " WHERE " CHANNELNAME_C_ " = @"
                                                                      CHANNELNAME_C_ ")");
                
                okSoFar = performSQLstatementWithNoResults(_db, removeFromChannelsAssociates,
                                                           setupRemoveFromChannelsAssociates,
                                                           static_cast<const void *>(primaryChannelName.c_str()));
            }
            if (okSoFar)
            {
                for (int ii = 0, numAssociates = associatesList.size(); okSoFar && (ii < numAssociates); ++ii)
                {
                    yarp::os::Value & anAssociate(associatesList.get(ii));
                    
                    if (anAssociate.isString())
                    {
                        static const char * removeFromAssociates = T_("DELETE FROM " ASSOCIATES_T_ " WHERE "
                                                                      ASSOCIATE_C_ " = @" ASSOCIATE_C_);
                        
                        okSoFar = performSQLstatementWithNoResults(_db, removeFromAssociates, setupRemoveFromAssociates,
                                                           static_cast<const void *>(anAssociate.toString().c_str()));
                    }
                    else
                    {
                        OD_LOG("! (anAssociate.isString())");//####
                        okSoFar = false;
                    }
                }
            }
            if (okSoFar)
            {
                static const char * removeFromChannels = T_("DELETE FROM " CHANNELS_T_ " WHERE " CHANNELNAME_C_ " = @"
                                                            CHANNELNAME_C_);

                okSoFar = performSQLstatementWithNoResults(_db, removeFromChannels, setupRemoveFromChannels,
                                                           static_cast<const void *>(primaryChannelName.c_str()));
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
} // RegistryService::::removeAllAssociations

void RegistryService::removeCheckedTimeForChannel(const yarp::os::ConstString & serviceChannelName)
{
    OD_LOG_OBJENTER();//####
    _checkedTimeLock.lock();
    _lastCheckedTime.erase(serviceChannelName);
    _checkedTimeLock.unlock();
    OD_LOG_OBJEXIT();//####
} // RegistryService::removeCheckedTimeForChannel

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

            okSoFar = performSQLstatementWithNoResults(_db, removeFromRequestsKeywords, setupRemoveFromRequestsKeywords,
                                                       static_cast<const void *>(serviceChannelName.c_str()));
            if (okSoFar)
            {
                // Remove the service channel requests.
                static const char * removeFromRequests = "DELETE FROM " REQUESTS_T_ " WHERE " CHANNELNAME_C_ " = @"
                                                            CHANNELNAME_C_;

                okSoFar = performSQLstatementWithNoResults(_db, removeFromRequests, setupRemoveFromRequests,
                                                           static_cast<const void *>(serviceChannelName.c_str()));
            }
            if (okSoFar)
            {
                // Remove the service channel name.
                static const char * removeFromServices = "DELETE FROM " SERVICES_T_ " WHERE " CHANNELNAME_C_ " = @"
                                                            CHANNELNAME_C_;
                
                okSoFar = performSQLstatementWithNoResults(_db, removeFromServices, setupRemoveFromServices,
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
            reportStatusChange(serviceChannelName, kRegistryRemoveService);
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

void RegistryService::reportStatusChange(const yarp::os::ConstString & channelName,
                                         const ServiceStatus           newStatus)
{
    OD_LOG_OBJENTER();//####
    OD_LOG_S1("channelName = ", channelName.c_str());//####
    if (_statusChannel)
    {
        Common::Package message;
        
        switch (newStatus)
        {
            case kRegistryStarted:
                message.addString(MpM_REGISTRY_STATUS_STARTING);
                break;
                
            case kRegistryStopped:
                message.addString(MpM_REGISTRY_STATUS_STOPPING);
                break;
                
            case kRegistryAddService:
                message.addString(MpM_REGISTRY_STATUS_ADDING);
                message.addString(channelName);
                break;
                
            case kRegistryRemoveService:
                message.addString(MpM_REGISTRY_STATUS_REMOVING);
                message.addString(channelName);
                break;
                
            default:
                message.addString(MpM_REGISTRY_STATUS_UNKNOWN);
                break;
                
        }
        if (! _statusChannel->write(message))
        {
            OD_LOG("(! _statusChannel->write(message))");//####
#if defined(MpM_StallOnSendProblem)
            Common::Stall();
#endif // defined(MpM_StallOnSendProblem)
        }
    }
    OD_LOG_OBJEXIT();//####
} // RegistryService::reportStatusChange

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
            
#if defined(MpM_UseTestDatabase)
            dbFileName = "/tmp/test.db";
#else // ! defined(MpM_UseTestDatabase)
            if (_inMemory)
            {
                dbFileName = ":memory:";
            }
            else
            {
                dbFileName = "";
            }
#endif // ! defined(MpM_UseTestDatabase)
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

bool RegistryService::setUpStatusChannel(void)
{
    OD_LOG_OBJENTER();//####
    bool okSoFar = false;
    
    try
    {
        _statusChannel = new Common::GeneralChannel(true);
        if (_statusChannel)
        {
            yarp::os::ConstString outputName(SECONDARY_CHANNEL_NAME_);
#if defined(MpM_ReportOnConnections)
            _statusChannel->setReporter(ChannelStatusReporter::gReporter);
            _statusChannel->getReport(ChannelStatusReporter::gReporter);
#endif // defined(MpM_ReportOnConnections)
            if (_statusChannel->openWithRetries(outputName, STANDARD_WAIT_TIME))
            {
                _statusChannel->setProtocol("s");
                okSoFar = true;
            }
            else
            {
                OD_LOG("! (_statusChannel->openWithRetries(outputName, STANDARD_WAIT_TIME))");//####
            }
        }
        else
        {
            OD_LOG("! (_statusChannel)");//####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT_B(okSoFar);//####
    return okSoFar;
} // RegistryService::setUpStatusChannel

bool RegistryService::start(void)
{
    OD_LOG_OBJENTER();//####
    bool result = false;
    
    try
    {
        if ((! isActive()) && (! isStarted()))
        {
            inherited::start();
            if (isStarted() && setUpDatabase() && setUpStatusChannel())
            {
                // Register ourselves!!!
                yarp::os::ConstString   aName(Common::GetRandomChannelName(MpM_REGISTRY_CHANNEL_NAME "/_temp_"));
                Common::ClientChannel * newChannel = new Common::ClientChannel;
                
                if (newChannel)
                {
                    if (newChannel->openWithRetries(aName, STANDARD_WAIT_TIME))
                    {
                        if (Common::NetworkConnectWithRetries(aName, MpM_REGISTRY_CHANNEL_NAME, STANDARD_WAIT_TIME,
                                                              false))
                        {
                            Common::Package         parameters(MpM_REGISTRY_CHANNEL_NAME);
                            Common::ServiceRequest  request(MpM_REGISTER_REQUEST, parameters);
                            Common::ServiceResponse response;
                            
                            if (request.send(*newChannel, &response))
                            {
                                // Check that we got a successful self-registration!
                                if (MpM_EXPECTED_REGISTER_RESPONSE_SIZE == response.count())
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
                                    OD_LOG("! (MpM_EXPECTED_REGISTER_RESPONSE_SIZE == response.count())");//####
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
                                   "MpM_REGISTRY_CHANNEL_NAME, STANDARD_WAIT_TIME, false))");//####
                        }
#if defined(MpM_DoExplicitClose)
                        newChannel->close();
#endif // defined(MpM_DoExplicitClose)
                    }
                    else
                    {
                        OD_LOG("! (newChannel->openWithRetries(aName, STANDARD_WAIT_TIME))");//####
                    }
                    Common::ClientChannel::RelinquishChannel(newChannel);
                    newChannel = NULL;
                }
                else
                {
                    OD_LOG("! (newChannel)");//####
                }
            }
            else
            {
                OD_LOG("! (isStarted() && setUpDatabase() && setUpStatusChannel())");//####
            }
        }
        result = isStarted();
        if (result)
        {
            reportStatusChange("", kRegistryStarted);
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
        throw;
    }
    OD_LOG_OBJEXIT_B(result);//####
    return result;
} // RegistryService::start

void RegistryService::startChecker(void)
{
    OD_LOG_OBJENTER();//####
    if (! _checker)
    {
        _checker = new RegistryCheckThread(*this);
        OD_LOG_P1("_checker <- ", _checker);//####
        _checker->start();
    }
    OD_LOG_OBJEXIT();//####
} // RegistryService::startChecker

bool RegistryService::stop(void)
{
    OD_LOG_OBJENTER();//####
    bool result = false;
    
    try
    {
        if (_checker)
        {
            OD_LOG_P1("_checker = ", _checker);//####
            _checker->stop();
            for ( ; _checker->isRunning(); )
            {
                yarp::os::Time::delay(PING_CHECK_INTERVAL / 3.1);
            }
            delete _checker;
            _checker = NULL;
        }
        if (isActive())
        {
            reportStatusChange("", kRegistryStopped);
        }
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

void RegistryService::updateCheckedTimeForChannel(const yarp::os::ConstString & serviceChannelName)
{
    OD_LOG_OBJENTER();//####
    _checkedTimeLock.lock();
    _lastCheckedTime[serviceChannelName] = yarp::os::Time::now() + (PING_COUNT_MAX * PING_INTERVAL);
    _checkedTimeLock.unlock();
    OD_LOG_OBJEXIT();//####
} // RegistryService::updateCheckedTimeForChannel

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
