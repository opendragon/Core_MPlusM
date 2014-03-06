//
//  YPPRegistryService.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-06.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPRegistryService.h"
#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPRegisterRequestHandler.h"
#include "YPPRequests.h"
#include "YPPServiceRequest.h"
#include "YPPUnregisterRequestHandler.h"
#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wc++11-long-long"
#endif // defined(__APPLE__)
#include <sqlite3.h>
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)
#include <yarp/os/Time.h>

#if 0
#define SQLITE_OK           0   /* Successful result */
/* beginning-of-error-codes */
#define SQLITE_ERROR        1   /* SQL error or missing database */
#define SQLITE_INTERNAL     2   /* Internal logic error in SQLite */
#define SQLITE_PERM         3   /* Access permission denied */
#define SQLITE_ABORT        4   /* Callback routine requested an abort */
#define SQLITE_BUSY         5   /* The database file is locked */
#define SQLITE_LOCKED       6   /* A table in the database is locked */
#define SQLITE_NOMEM        7   /* A malloc() failed */
#define SQLITE_READONLY     8   /* Attempt to write a readonly database */
#define SQLITE_INTERRUPT    9   /* Operation terminated by sqlite3_interrupt()*/
#define SQLITE_IOERR       10   /* Some kind of disk I/O error occurred */
#define SQLITE_CORRUPT     11   /* The database disk image is malformed */
#define SQLITE_NOTFOUND    12   /* Unknown opcode in sqlite3_file_control() */
#define SQLITE_FULL        13   /* Insertion failed because database is full */
#define SQLITE_CANTOPEN    14   /* Unable to open the database file */
#define SQLITE_PROTOCOL    15   /* Database lock protocol error */
#define SQLITE_EMPTY       16   /* Database is empty */
#define SQLITE_SCHEMA      17   /* The database schema changed */
#define SQLITE_TOOBIG      18   /* String or BLOB exceeds size limit */
#define SQLITE_CONSTRAINT  19   /* Abort due to constraint violation */
#define SQLITE_MISMATCH    20   /* Data type mismatch */
#define SQLITE_MISUSE      21   /* Library used incorrectly */
#define SQLITE_NOLFS       22   /* Uses OS features not supported on host */
#define SQLITE_AUTH        23   /* Authorization denied */
#define SQLITE_FORMAT      24   /* Auxiliary database format error */
#define SQLITE_RANGE       25   /* 2nd parameter to sqlite3_bind out of range */
#define SQLITE_NOTADB      26   /* File opened that is not a database file */
#define SQLITE_ROW         100  /* sqlite3_step() has another row ready */
#define SQLITE_DONE        101  /* sqlite3_step() has finished executing */
/* end-of-error-codes */
#endif//0

using namespace YarpPlusPlus;

#if defined(__APPLE__)
# pragma mark Private structures and constants
#endif // defined(__APPLE__)

#define USE_TEST_DATABASE /* */

/*! @brief The command to initiate an SQL transaction. */
static const char * kBeginTransaction = "BEGIN TRANSACTION";
/*! @brief The command to successfully complete an SQL transaction. */
static const char * kEndTransaction = "END TRANSACTION";

/*! @brief The data needed to add a request-keyword entry into the database. */
struct ReqKeyData
{
    /*! @brief The name of the request. */
    yarp::os::ConstString _name;
    /*! @brief The service port for the request. */
    yarp::os::ConstString _port;
    /*! @brief A keyword for the request. */
    yarp::os::ConstString _key;
}; // ReqKeyData

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

typedef int (*bindFunction) (sqlite3_stmt * statement,
                             const void *   stuff);

/*! @brief Construct a table needed in the database.
 @param database The database to be modified.
 @param sqlStatement The description of the table.
 @param doBinds A function that will fill in any parameters in the statement.
 @param data The custom information used with the binding function.
 @returns @c true if the table was successfully built and @c false otherwise. */
static bool performSQLstatementWithNoResults(sqlite3 *    database,
                                             const char * sqlStatement,
                                             bindFunction doBinds = NULL,
                                             const void * data = NULL)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P2("database = ", database, "data = ", data);//####
    OD_SYSLOG_S1("sqlStatement = ", sqlStatement);//####
    bool okSoFar = true;
    
    if (database)
    {
        sqlite3_stmt * prepared = NULL;
        int            sqlRes = sqlite3_prepare_v2(database, sqlStatement, static_cast<int>(strlen(sqlStatement)),
                                                   &prepared, NULL);
        OD_SYSLOG_LL1("sqlRes <- ", sqlRes);//####
        
        if (SQLITE_OK != sqlRes)
        {
            OD_SYSLOG_S1("error description: ", sqlite3_errstr(sqlRes));//####
        }
        if ((SQLITE_OK == sqlRes) && prepared)
        {
            if (doBinds)
            {
                sqlRes = doBinds(prepared, data);
                OD_SYSLOG_LL1("sqlRes <- ", sqlRes);//####
                okSoFar = (SQLITE_OK == sqlRes);
            }
            if (okSoFar)
            {
                sqlRes = sqlite3_step(prepared);
                OD_SYSLOG_LL1("sqlRes <- ", sqlRes);//####
                if ((SQLITE_DONE != sqlRes) && (SQLITE_BUSY != sqlRes))
                {
                    OD_SYSLOG_S1("error description: ", sqlite3_errstr(sqlRes));//####
                }
                while (SQLITE_BUSY == sqlRes)
                {
                    yarp::os::Time::delay(1.0);
                    sqlRes = sqlite3_step(prepared);
                    OD_SYSLOG_LL1("sqlRes <- ", sqlRes);//####
                    if ((SQLITE_DONE != sqlRes) && (SQLITE_BUSY != sqlRes))
                    {
                        OD_SYSLOG_S1("error description: ", sqlite3_errstr(sqlRes));//####
                    }
                }
                if (SQLITE_DONE != sqlRes)
                {
                    okSoFar = false;
                }
            }
            sqlite3_finalize(prepared);
        }
        else
        {
            okSoFar = false;
        }
    }
    else
    {
        okSoFar = false;
    }
    OD_SYSLOG_EXIT_B(okSoFar);//####
    return okSoFar;
} // performSQLstatementWithNoResults

/*! @brief Construct the tables needed in the database.
 @param database The database to be modified.
 @returns @c true if the tables were successfully built and @c false otherwise. */
static bool constructTables(sqlite3 * database)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P1("database = ", database);//####
    bool okSoFar = true;
    
    if (database)
    {
        static const char * tableSQL[] =
        {
#if defined(USE_TEST_DATABASE)
//            "DROP INDEX IF EXISTS Keywords_keyword_idx",
            "DROP INDEX IF EXISTS Requests_name_idx",
            "DROP INDEX IF EXISTS Requests_portName_idx",
            "DROP INDEX IF EXISTS RequestsKeywords_Keywords_id_idx",
            "DROP INDEX IF EXISTS RequestsKeywords_Requests_id_idx",
//            "DROP INDEX IF EXISTS Services_portName_idx",
            "DROP TABLE IF EXISTS RequestsKeywords",
            "DROP TABLE IF EXISTS Requests",
            "DROP TABLE IF EXISTS Keywords",
            "DROP TABLE IF EXISTS Services",
#endif // defined(USE_TEST_DATABASE)
            "CREATE TABLE IF NOT EXISTS Services("
                " portName Text NOT NULL DEFAULT _ PRIMARY KEY ON CONFLICT REPLACE)",
//            "CREATE UNIQUE INDEX IF NOT EXISTS Services_portName_idx ON Services(portName)",
            "CREATE TABLE IF NOT EXISTS Keywords("
                " keyword Text NOT NULL DEFAULT _ PRIMARY KEY ON CONFLICT IGNORE)",
//            "CREATE UNIQUE INDEX IF NOT EXISTS Keywords_keyword_idx ON Keywords(keyword)",
            "CREATE TABLE IF NOT EXISTS Requests("
                " portName    Text NOT NULL DEFAULT _ REFERENCES Services(portName),"
                " name        Text NOT NULL DEFAULT _,"
                " input       Text,"
                " output      Text,"
                " version     Text,"
                " description Text,"
                " key         Integer PRIMARY KEY)",
            "CREATE INDEX IF NOT EXISTS Requests_name_idx ON Requests(name)",
            "CREATE INDEX IF NOT EXISTS Requests_portName_idx ON Requests(portName)",
            "CREATE TABLE RequestsKeywords("
                " Keywords_id Text REFERENCES Keywords(keyword),"
                " Requests_id Integer REFERENCES Requests(key))",
            "CREATE INDEX IF NOT EXISTS RequestsKeywords_Keywords_id_idx ON RequestsKeywords(Keywords_id)",
            "CREATE INDEX IF NOT EXISTS RequestsKeywords_Requests_id_idx ON RequestsKeywords(Requests_id)"
        };
        int numTables = (sizeof(tableSQL) / sizeof(*tableSQL));
        
        okSoFar = performSQLstatementWithNoResults(database, kBeginTransaction);
        for (int ii = 0; okSoFar && (ii < numTables); ++ii)
        {
            okSoFar = performSQLstatementWithNoResults(database, tableSQL[ii]);
        }
        if (okSoFar)
        {
            okSoFar = performSQLstatementWithNoResults(database, kEndTransaction);
        }
    }
    else
    {
        okSoFar = false;
    }
    OD_SYSLOG_EXIT_B(okSoFar);//####
    return okSoFar;
} // constructTables

/*! @brief Bind the values that are to be inserted into the Keywords table.
 @param statement The prepared statement that is to be updated.
 @param stuff The source of data that is to be bound.
 @returns The SQLite error from the bind operation. */
static int setupInsertForKeywords(sqlite3_stmt * statement,
                                  const void *   stuff)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P2("statement = ", statement, "stuff = ", stuff);//####
    const char * nameString = static_cast<const char *>(stuff);
    int          result = sqlite3_bind_text(statement, 1, nameString, static_cast<int>(strlen(nameString)),
                                            SQLITE_TRANSIENT);
    
    if (SQLITE_OK != result)
    {
        OD_SYSLOG_S1("error description: ", sqlite3_errstr(result));//####
    }
    OD_SYSLOG_EXIT_LL(result);
    return result;
} // setupInsertForKeywords

/*! @brief Bind the values that are to be inserted into the Requests table.
 @param statement The prepared statement that is to be updated.
 @param stuff The source of data that is to be bound.
 @returns The SQLite error from the bind operation. */
static int setupInsertForRequests(sqlite3_stmt * statement,
                                  const void *   stuff)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P2("statement = ", statement, "stuff = ", stuff);//####
    const RequestDescription * descriptor = static_cast<const RequestDescription *>(stuff);
    const char *               portName = descriptor->_port.c_str();
    int                        result = sqlite3_bind_text(statement, 1, portName, static_cast<int>(strlen(portName)),
                                                          SQLITE_TRANSIENT);
    
    if (SQLITE_OK == result)
    {
        const char * name = descriptor->_name.c_str();
        
        result = sqlite3_bind_text(statement, 2, name, static_cast<int>(strlen(name)), SQLITE_TRANSIENT);
    }
    if (SQLITE_OK == result)
    {
        const char * input = descriptor->_inputs.c_str();
        
        result = sqlite3_bind_text(statement, 3, input, static_cast<int>(strlen(input)), SQLITE_TRANSIENT);
    }
    if (SQLITE_OK == result)
    {
        const char * output = descriptor->_outputs.c_str();
        
        result = sqlite3_bind_text(statement, 4, output, static_cast<int>(strlen(output)), SQLITE_TRANSIENT);
    }
    if (SQLITE_OK == result)
    {
        const char * version = descriptor->_version.c_str();
        
        result = sqlite3_bind_text(statement, 5, version, static_cast<int>(strlen(version)), SQLITE_TRANSIENT);
    }
    if (SQLITE_OK == result)
    {
        const char * description = descriptor->_description.c_str();
        
        result = sqlite3_bind_text(statement, 6, description, static_cast<int>(strlen(description)), SQLITE_TRANSIENT);
    }
    if (SQLITE_OK != result)
    {
        OD_SYSLOG_S1("error description: ", sqlite3_errstr(result));//####
    }
    OD_SYSLOG_EXIT_LL(result);
    return result;
} // setupInsertForRequests

/*! @brief Bind the values that are to be inserted into the RequestsKeywords table.
 @param statement The prepared statement that is to be updated.
 @param stuff The source of data that is to be bound.
 @returns The SQLite error from the bind operation. */
static int setupInsertForRequestsKeywords(sqlite3_stmt * statement,
                                          const void *   stuff)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P2("statement = ", statement, "stuff = ", stuff);//####
    const ReqKeyData * descriptor = static_cast<const ReqKeyData *>(stuff);
    const char *       keyword = descriptor->_key.c_str();
    int                result = sqlite3_bind_text(statement, 1, keyword, static_cast<int>(strlen(keyword)),
                                                  SQLITE_TRANSIENT);
    
    if (SQLITE_OK == result)
    {
        const char * name = descriptor->_name.c_str();
        
        result = sqlite3_bind_text(statement, 2, name, static_cast<int>(strlen(name)), SQLITE_TRANSIENT);
    }
    if (SQLITE_OK == result)
    {
        const char * portName = descriptor->_port.c_str();
        
        result = sqlite3_bind_text(statement, 3, portName, static_cast<int>(strlen(portName)), SQLITE_TRANSIENT);
    }
    OD_SYSLOG_EXIT_LL(result);
    return result;
} // setupInsertForRequestsKeywords

/*! @brief Bind the values that are to be inserted into the Services table.
 @param statement The prepared statement that is to be updated.
 @param stuff The source of data that is to be bound.
 @returns The SQLite error from the bind operation. */
static int setupInsertForServices(sqlite3_stmt * statement,
                                  const void *   stuff)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P2("statement = ", statement, "stuff = ", stuff);//####
    const char * nameString = static_cast<const char *>(stuff);
    int          result = sqlite3_bind_text(statement, 1, nameString, static_cast<int>(strlen(nameString)),
                                            SQLITE_TRANSIENT);
    
    if (SQLITE_OK != result)
    {
        OD_SYSLOG_S1("error description: ", sqlite3_errstr(result));//####
    }
    OD_SYSLOG_EXIT_LL(result);
    return result;
} // setupInsertForServices

/*! @brief Bind the values that are to be removed from the Services table.
 @param statement The prepared statement that is to be updated.
 @param stuff The source of data that is to be bound.
 @returns The SQLite error from the bind operation. */
static int setupRemoveForServices(sqlite3_stmt * statement,
                                  const void *   stuff)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P2("statement = ", statement, "stuff = ", stuff);//####
    const char * nameString = static_cast<const char *>(stuff);
    int          result = sqlite3_bind_text(statement, 1, nameString, static_cast<int>(strlen(nameString)),
                                            SQLITE_TRANSIENT);
    
    if (SQLITE_OK != result)
    {
        OD_SYSLOG_S1("error description: ", sqlite3_errstr(result));//####
    }
    OD_SYSLOG_EXIT_LL(result);
    return result;
} // setupRemoveForServices

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

bool RegistryService::registerLocalService(const yarp::os::ConstString & portName)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("portName = ", portName.c_str());//####
    bool             result = false;
    yarp::os::Bottle parameters(portName);
    ServiceRequest   request(YPP_REGISTER_REQUEST, parameters);
    ServiceResponse  response;
    
    if (request.send(YPP_SERVICE_REGISTRY_PORT_NAME, &response))
    {
        // Check that we got a successful self-registration!
        if (1 == response.count())
        {
            yarp::os::Value theValue = response.element(0);
            
            if (theValue.isString())
            {
                result = (theValue.toString() == YPP_OK_RESPONSE);
            }
        }
    }
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // RegistryService::registerLocalService

bool RegistryService::unregisterLocalService(const yarp::os::ConstString & portName)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("portName = ", portName.c_str());//####
    bool             result = false;
    yarp::os::Bottle parameters(portName);
    ServiceRequest   request(YPP_UNREGISTER_REQUEST, parameters);
    ServiceResponse  response;
    
    if (request.send(YPP_SERVICE_REGISTRY_PORT_NAME, &response))
    {
        // Check that we got a successful self-registration!
        if (1 == response.count())
        {
            yarp::os::Value theValue = response.element(0);
            
            if (theValue.isString())
            {
                result = (theValue.toString() == YPP_OK_RESPONSE);
            }
        }
    }
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // RegistryService::unregisterLocalService

#if defined(__APPLE__)
# pragma mark Constructors and destructors
#endif // defined(__APPLE__)

RegistryService::RegistryService(const bool                    useInMemoryDb,
                                 const yarp::os::ConstString & serviceHostName,
                                 const yarp::os::ConstString & servicePortNumber) :
        inherited(true, YPP_SERVICE_REGISTRY_PORT_NAME, serviceHostName, servicePortNumber), _db(NULL),
        _inMemory(useInMemoryDb), _isActive(false)
{
    OD_SYSLOG_ENTER();//####
    setUpRequestHandlers();
    OD_SYSLOG_EXIT_P(this);//####
} // RegistryService::RegistryService

RegistryService::~RegistryService(void)
{
    OD_SYSLOG_ENTER();//####
    if (_db)
    {
        sqlite3_close(_db);
    }
    OD_SYSLOG_EXIT();//####
} // RegistryService::~RegistryService

#if defined(__APPLE__)
# pragma mark Actions
#endif // defined(__APPLE__)

bool RegistryService::addRequestRecord(const yarp::os::Bottle &   keywordList,
                                       const RequestDescription & description)
{
    OD_SYSLOG_ENTER();//####
    bool                okSoFar = performSQLstatementWithNoResults(_db, kBeginTransaction);
    static const char * insertIntoKeywords = "INSERT INTO Keywords(keyword) VALUES(?1)";
    static const char * insertIntoRequests = "INSERT INTO Requests(portName,name,input,output,version,description) "
                                             "VALUES(?1,?2,?3,?4,?5,?6)";
    static const char * insertIntoRequestsKeywords = "INSERT INTO RequestsKeywords(keywords_id,requests_id) "
                                                        "SELECT ?1, KEY FROM requests WHERE name = ?2 AND "
                                                        "portname = ?3";
    static const char * insertIntoServices = "INSERT INTO Services(portName) VALUES(?1)";
    
    if (okSoFar)
    {
        // Add the service port name.
        okSoFar = performSQLstatementWithNoResults(_db, insertIntoServices, setupInsertForServices,
                                                   static_cast<const void *>(description._port.c_str()));
    }
    if (okSoFar)
    {
        // Add the request.
        okSoFar = performSQLstatementWithNoResults(_db, insertIntoRequests, setupInsertForRequests,
                                                   static_cast<const void *>(&description));
    }
    if (okSoFar)
    {
        // Add the keywords.
        int        numKeywords = keywordList.size();
        ReqKeyData reqData;
        
        reqData._name = description._name;
        reqData._port = description._port;
        for (int ii = 0; okSoFar && (ii < numKeywords); ++ii)
        {
            yarp::os::Value & aKeyword(keywordList.get(ii));
            
            if (aKeyword.isString())
            {
                reqData._key = aKeyword.toString();
                okSoFar = performSQLstatementWithNoResults(_db, insertIntoKeywords, setupInsertForKeywords,
                                                           static_cast<const void *>(reqData._key.c_str()));
                if (okSoFar)
                {
                    okSoFar = performSQLstatementWithNoResults(_db, insertIntoRequestsKeywords,
                                                               setupInsertForRequestsKeywords, &reqData);
                }
            }
            else
            {
                okSoFar = false;
            }
        }
    }
    if (okSoFar)
    {
        okSoFar = performSQLstatementWithNoResults(_db, kEndTransaction);
    }
    OD_SYSLOG_EXIT_B(okSoFar);//####
    return okSoFar;
} // RegistryService::addRequestRecord

bool RegistryService::removeServiceRecord(const yarp::os::ConstString & servicePortName)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("servicePortName = ", servicePortName.c_str());//####
    bool                okSoFar = performSQLstatementWithNoResults(_db, kBeginTransaction);
    static const char * removeFromServices = "DELETE FROM Services WHERE portName = ?1";
    
    // For each request provided by the service
    //    Remove it's RequestsKeywords
    //    Remove the request
    // Remove the service
    
    // DELETE FROM Services WHERE portName = ?1
    
    // Note that this should be do-able with the proper linkage settings...
    if (okSoFar)
    {
        // Add the service port name.
        okSoFar = performSQLstatementWithNoResults(_db, removeFromServices, setupRemoveForServices,
                                                   static_cast<const void *>(servicePortName.c_str()));
    }
    if (okSoFar)
    {
        okSoFar = performSQLstatementWithNoResults(_db, kEndTransaction);
    }
    OD_SYSLOG_EXIT_B(okSoFar);//####
    return okSoFar;
} // RegistryService::removeServiceRecord

bool RegistryService::setUpDatabase(void)
{
    OD_SYSLOG_ENTER();//####
    bool okSoFar = true;
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
            sqlite3_close(_db);
            _db = NULL;
        }
    }
    OD_SYSLOG_EXIT_B(okSoFar);//####
    return okSoFar;
} // RegistryService::setUpDatabase

void RegistryService::setUpRequestHandlers(void)
{
    OD_SYSLOG_ENTER();//####
    _requestHandlers.registerRequestHandler(YPP_REGISTER_REQUEST, new RegisterRequestHandler(*this));
    _requestHandlers.registerRequestHandler(YPP_UNREGISTER_REQUEST, new UnregisterRequestHandler(*this));
    OD_SYSLOG_EXIT();//####
} // RegistryService::setUpRequestHandlers

bool RegistryService::start(void)
{
    OD_SYSLOG_ENTER();//####
    if ((! isActive()) && (! isStarted()))
    {
        BaseService::start();
        if (isStarted() && setUpDatabase())
        {
            // Register ourselves!!!
            yarp::os::Bottle parameters(YPP_SERVICE_REGISTRY_PORT_NAME);
            ServiceRequest   request(YPP_REGISTER_REQUEST, parameters);
            ServiceResponse  response;
            
            if (request.send(getEndpoint(), &response))
            {
                // Check that we got a successful self-registration!
                if (1 == response.count())
                {
                    yarp::os::Value theValue = response.element(0);
                    
                    if (theValue.isString())
                    {
                        _isActive = (theValue.toString() == YPP_OK_RESPONSE);
                    }
                }
            }
        }
    }
    OD_SYSLOG_EXIT_B(isStarted());//####
    return isStarted();
} // RegistryService::start

bool RegistryService::stop(void)
{
    OD_SYSLOG_ENTER();//####
    bool result = BaseService::stop();
    
    _isActive = false;
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // RegistryService::stop

#if defined(__APPLE__)
# pragma mark Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
