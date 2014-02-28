//
//  YPPExampleService.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-06.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPExampleService.h"
#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPRequestHandler.h"
#include "YPPRequests.h"
#include <yarp/os/Property.h>

using namespace YarpPlusPlus;

#pragma mark Private structures and constants

/*! @brief The name for the 'echo' request. */
#define ECHO_REQUEST_NAME           "echo"
/*! @brief The protocol version number for the 'echo' request. */
#define ECHO_REQUEST_VERSION_NUMBER "1.0"

class EchoRequestHandler : public RequestHandler
{
public:
    
    /*! @brief The constructor.
     @param service The service that responds to this request. */
    EchoRequestHandler(BaseService & service);
    
    /*! @brief The destructor. */
    virtual ~EchoRequestHandler(void);
    
    /*! @brief Fill in a description dictionary for the request.
     @param info The dictionary to be filled in. */
    virtual void fillInDescription(yarp::os::Property & info);
    
    /*! @brief Process a request.
     @param restOfInput The arguments to the operation.
     @param replyMechanism non-@c NULL if a reply is expected and @c NULL otherwise. */
    virtual bool operator() (const yarp::os::Bottle &     restOfInput,
                             yarp::os::ConnectionWriter * replyMechanism);
    
protected:
    
private:
    
    typedef RequestHandler inherited;
    
}; // EchoRequestHandler

#pragma mark Constructors and destructors

EchoRequestHandler::EchoRequestHandler(BaseService & service) :
        inherited(ECHO_REQUEST_NAME, service)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // EchoRequestHandler::EchoRequestHandler

EchoRequestHandler::~EchoRequestHandler(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // EchoRequestHandler::~EchoRequestHandler

#pragma mark Actions

void EchoRequestHandler::fillInDescription(yarp::os::Property & info)
{
    OD_SYSLOG_ENTER();//####
    info.put(YPP_REQREP_DICT_NAME_KEY, ECHO_REQUEST_NAME);
    info.put(YPP_REQREP_DICT_INPUT_KEY, YPP_REQREP_ANYTHING YPP_REQREP_0_OR_MORE);
    info.put(YPP_REQREP_DICT_OUTPUT_KEY, YPP_REQREP_ANYTHING YPP_REQREP_0_OR_MORE);
    info.put(YPP_REQREP_DICT_VERSION_KEY, ECHO_REQUEST_VERSION_NUMBER);
    info.put(YPP_REQREP_DICT_DESCRIPTION_KEY, "Echo back any input");
    yarp::os::Value    keywords;
    yarp::os::Bottle * asList = keywords.asList();
    
    asList->addString(ECHO_REQUEST_NAME);
    info.put(YPP_REQREP_DICT_KEYWORDS_KEY, keywords);
    OD_SYSLOG_EXIT();//####
} // EchoRequestHandler::fillInDescription

bool EchoRequestHandler::operator() (const yarp::os::Bottle &     restOfInput,
                                     yarp::os::ConnectionWriter * replyMechanism)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("restOfInput = ", restOfInput.toString().c_str());//####
    bool result = true;
    
    if (replyMechanism)
    {
        yarp::os::Bottle argsCopy(restOfInput);
        
        argsCopy.write(*replyMechanism);
    }
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // EchoRequestHandler::operator()

#pragma mark Constructors and destructors

ExampleService::ExampleService(const yarp::os::ConstString & serviceEndpointName,
                               const yarp::os::ConstString & serviceHostName,
                               const yarp::os::ConstString & servicePortNumber) :
        inherited(true, serviceEndpointName, serviceHostName, servicePortNumber)
{
    OD_SYSLOG_ENTER();//####
    registerRequestHandler(ECHO_REQUEST_NAME, new EchoRequestHandler(*this));
    OD_SYSLOG_EXIT();//####
} // ExampleService::ExampleService

ExampleService::ExampleService(const int argc,
                               char **   argv) :
        BaseService(true, argc, argv)
{
    OD_SYSLOG_ENTER();//####
    registerRequestHandler(ECHO_REQUEST_NAME, new EchoRequestHandler(*this));
    OD_SYSLOG_EXIT();//####
} // ExampleService::ExampleService

ExampleService::~ExampleService(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // ExampleService::~ExampleService

#pragma mark Actions

bool ExampleService::start(void)
{
    OD_SYSLOG_ENTER();//####
    if (! isStarted())
    {
        BaseService::start();
        if (isStarted())
        {
            
        }
    }
    OD_SYSLOG_EXIT_B(isStarted());//####
    return isStarted();
} // ExampleService::start

bool ExampleService::stop(void)
{
    OD_SYSLOG_ENTER();//####
    bool result = BaseService::stop();
    
    OD_SYSLOG_EXIT_B(result);//####
    return result;
} // ExampleService::stop
