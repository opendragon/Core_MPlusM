//
//  YPPCommonTest.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-06.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include <yarp/os/all.h>
#include <yarp/conf/version.h>
#include <iostream>
#include "YPPServiceRequest.h"
#include "YPPServiceResponse.h"
#include "YPPBaseClient.h"
#include "YPPBaseService.h"
#include "YPPEndpoint.h"
#include "YPPInputHandler.h"
#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"

using std::cout;
using std::cerr;
using std::endl;

#pragma mark Private structures and constants

#pragma mark Local functions

/*! @brief The critical information for each volume.
 
 This class represents the information for a volume that is needed to perform the data delete operations. */
class EndpointStatusReporter : public yarp::os::PortReport
{
public:
    
    /*! @brief The constructor. */
    EndpointStatusReporter(void);
    
    /*! @brief The destructor. */
    virtual ~EndpointStatusReporter(void);
    
    /*! @brief Write out the endpoint event / state information.
     @param info The event / state information from the endpoint. */
    virtual void report(const yarp::os::PortInfo & info);

protected:

private:

}; // EndpointStatusReporter

#pragma mark Class methods

#pragma mark Constructors and destructors

EndpointStatusReporter::EndpointStatusReporter(void) :
        PortReport()
{
} // EndpointStatusReporter::EndpointStatusReporter

EndpointStatusReporter::~EndpointStatusReporter(void)
{
} // EndpointStatusReporter::~EndpointStatusReporter

#pragma mark Actions

void EndpointStatusReporter::report(const yarp::os::PortInfo & info)
{
    OD_SYSLOG_L1("tag = ", info.tag);
    OD_SYSLOG_B2("incoming = ", info.incoming, "created = ", info.created);
    OD_SYSLOG_S4("portName = ", info.portName.c_str(), "sourceName = ", info.sourceName.c_str(),
                 "targetName = ", info.targetName.c_str(), "carrierName = ", info.carrierName.c_str());
    OD_SYSLOG_S1("message = ", info.message.c_str());
} // EndpointStatusReporter::report

#pragma mark Accessors

#pragma mark Local functions

/*! @brief Create an endpoint for a test.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the endpoint constructor.
 @returns A newly created endpoint, or @c NULL if one could not be created. */
static YarpPlusPlus::Endpoint * doCreateEndpointForTest(int     argc,
                                                        char ** argv)
{
    YarpPlusPlus::Endpoint * stuff = NULL;
    
    if (argc > 0)
    {
        switch (argc)
        {
            // Argument order for tests = endpoint name [, IP address / name [, port [, carrier]]]
            case 1:
                stuff = new YarpPlusPlus::Endpoint(*argv);
                break;
                
            case 2:
                stuff = new YarpPlusPlus::Endpoint(*argv, argv[1]);
                break;
                
            case 3:
                stuff = new YarpPlusPlus::Endpoint(*argv, argv[1], argv[2]);
                break;
                
            case 4:
                stuff = new YarpPlusPlus::Endpoint(*argv, argv[1], argv[2], argv[3]);
                break;
                
            default:
                break;
                
        }
    }
    return stuff;
} // doCreateEndpointForTest

/*! @brief Perform a test case.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doCase1(int     argc,
                   char ** argv) // create endpoint
{
    int                      result;
    YarpPlusPlus::Endpoint * stuff = doCreateEndpointForTest(argc, argv);
    
    if (stuff)
    {
        if (stuff->open())
        {
            OD_SYSLOG_S1("endpoint name = ", stuff->getName().c_str());//####
            result = 0;
        }
        else
        {
            result = 1;
        }
        delete stuff;
    }
    else
    {
        result = 1;
    }
    return result;
} // doCase1

/*! @brief Perform a test case.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doCase2(int     argc,
                   char ** argv) // connect to endpoint
{
    int                      result;
    YarpPlusPlus::Endpoint * stuff = doCreateEndpointForTest(argc, argv);
    
    if (stuff)
    {
        EndpointStatusReporter reporter;
        
        if (stuff->open() && stuff->setReporter(reporter, true))
        {
            OD_SYSLOG_S1("endpoint name = ", stuff->getName().c_str());//####
            // Now we try to connect!
            yarp::os::Port        outPort;
            yarp::os::ConstString aName = YarpPlusPlus::Endpoint::getRandomPortName();
            
            OD_SYSLOG_S1("opening ", aName.c_str());//####
            if (outPort.open(aName))
            {
                OD_SYSLOG("(outPort.open(aName))");//####
                outPort.getReport(reporter);
                if (outPort.addOutput(stuff->getName()))
                {
                    OD_SYSLOG("(outPort.addOutput(stuff->getName()))");//####
                    result = 0;
                }
                else
                {
                    OD_SYSLOG("! (outPort.addOutput(stuff->getName()))");//####
                    result = 1;
                }
                outPort.close();
            }
            else
            {
                OD_SYSLOG("! (outPort.open(aName))");//####
                result = 1;
            }
        }
        else
        {
            result = 1;
        }
        delete stuff;
    }
    else
    {
        result = 1;
    }
    return result;
} // doCase2

/*! @brief A test input handler. */
class Test3Handler : public YarpPlusPlus::InputHandler
{
public:
    
    /*! @brief The constructor. */
    Test3Handler(void);
    
    /*! @brief The destructor. */
    virtual ~Test3Handler(void);
    
    /*! @brief Process partially-structured input data.
     @param input The partially-structured input data.
     @param replyMechanism @c NULL if no reply is expected and non-@c NULL otherwise.
     @returns @c true if the input was correctly structured and successfully processed. */
    virtual bool handleInput(yarp::os::Bottle &           input,
                             yarp::os::ConnectionWriter * replyMechanism);
    
protected:
    
private:
    
}; // Test3Handler

#pragma mark Class methods

#pragma mark Constructors and destructors

Test3Handler::Test3Handler(void) :
        InputHandler()
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // Test3Handler::Test3Handler

Test3Handler::~Test3Handler(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // Test3Handler::~Test3Handler

#pragma mark Actions

bool Test3Handler::handleInput(yarp::os::Bottle &           input,
                               yarp::os::ConnectionWriter * replyMechanism)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P1("replyMechanism = ", replyMechanism);//####
    OD_SYSLOG_S1("got ", input.toString().c_str());//####
    OD_SYSLOG_EXIT_B(true);//####
    return true;
} // Test3Handler::handleInput

#pragma mark Accessors

#pragma mark Local functions

/*! @brief Perform a test case.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doCase3(int     argc,
                   char ** argv) // send to endpoint
{
    int                      result;
    YarpPlusPlus::Endpoint * stuff = doCreateEndpointForTest(argc, argv);
    
    if (stuff)
    {
        EndpointStatusReporter reporter;
        Test3Handler           handler;

        if (stuff->setInputHandler(handler) && stuff->open() && stuff->setReporter(reporter, true))
        {
            OD_SYSLOG_S1("endpoint name = ", stuff->getName().c_str());//####
            // Now we try to connect!
            yarp::os::Port        outPort;
            yarp::os::ConstString aName = YarpPlusPlus::Endpoint::getRandomPortName();
            
            OD_SYSLOG_S1("opening ", aName.c_str());//####
            if (outPort.open(aName))
            {
                OD_SYSLOG("(outPort.open(aName))");//####
                outPort.getReport(reporter);
                if (outPort.addOutput(stuff->getName()))
                {
                    OD_SYSLOG("(outPort.addOutput(stuff->getName()))");//####
                    yarp::os::Bottle message;
                    
                    message.addString(aName);
                    message.addString("howdi");
                    if (outPort.write(message))
                    {
                        OD_SYSLOG("(outPort.write(message))");//####
                        result = 0;
                    }
                    else
                    {
                        OD_SYSLOG("! (outPort.write(message))");//####
                        result = 1;
                    }
                }
                else
                {
                    OD_SYSLOG("! (outPort.addOutput(stuff->getName()))");//####
                    result = 1;
                }
                outPort.close();
            }
            else
            {
                OD_SYSLOG("! (outPort.open(aName))");//####
                result = 1;
            }
        }
        else
        {
            result = 1;
        }
        delete stuff;
    }
    else
    {
        result = 1;
    }
    return result;
} // doCase3

/*! @brief A test input handler. */
class Test4Handler : public YarpPlusPlus::InputHandler
{
public:
    
    /*! @brief The constructor. */
    Test4Handler(void);
    
    /*! @brief The destructor. */
    virtual ~Test4Handler(void);
    
    /*! @brief Process partially-structured input data.
     @param input The partially-structured input data.
     @param replyMechanism @c NULL if no reply is expected and non-@c NULL otherwise.
     @returns @c true if the input was correctly structured and successfully processed. */
    virtual bool handleInput(yarp::os::Bottle &           input,
                             yarp::os::ConnectionWriter * replyMechanism);
    
protected:
    
private:
    
}; // Test4Handler

#pragma mark Class methods

#pragma mark Constructors and destructors

Test4Handler::Test4Handler(void) :
        InputHandler()
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // Test4Handler::Test4Handler

Test4Handler::~Test4Handler(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // Test4Handler::~Test4Handler

#pragma mark Actions

bool Test4Handler::handleInput(yarp::os::Bottle &           input,
                               yarp::os::ConnectionWriter * replyMechanism)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P1("replyMechanism = ", replyMechanism);//####
    OD_SYSLOG_S1("got ", input.toString().c_str());//####
    if (replyMechanism)
    {
        input.write(*replyMechanism);
    }
    OD_SYSLOG_EXIT_B(true);//####
    return true;
} // Test4Handler::handleInput

#pragma mark Accessors

#pragma mark Local functions

/*! @brief Perform a test case.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doCase4(int     argc,
                   char ** argv) // send to endpoint
{
    int                      result;
    YarpPlusPlus::Endpoint * stuff = doCreateEndpointForTest(argc, argv);
    
    if (stuff)
    {
        EndpointStatusReporter reporter;
        Test4Handler           handler;

        if (stuff->setInputHandler(handler) && stuff->open() && stuff->setReporter(reporter, true))
        {
            OD_SYSLOG_S1("endpoint name = ", stuff->getName().c_str());//####
            // Now we try to connect!
            yarp::os::Port        outPort;
            yarp::os::ConstString aName = YarpPlusPlus::Endpoint::getRandomPortName();
            
            OD_SYSLOG_S1("opening ", aName.c_str());//####
            if (outPort.open(aName))
            {
                OD_SYSLOG("(outPort.open(aName))");//####
                outPort.getReport(reporter);
                if (outPort.addOutput(stuff->getName()))
                {
                    OD_SYSLOG("(outPort.addOutput(stuff->getName()))");//####
                    yarp::os::Bottle message;
                    yarp::os::Bottle response;
                    
                    message.addString(aName);
                    message.addString("howdi");
                    if (outPort.write(message, response))
                    {
                        OD_SYSLOG("(outPort.write(message, response))");//####
                        OD_SYSLOG_S1("got ", response.toString().c_str());//####
                        result = 0;
                    }
                    else
                    {
                        OD_SYSLOG("! (outPort.write(message, response))");//####
                        result = 1;
                    }
                }
                else
                {
                    OD_SYSLOG("! (outPort.addOutput(stuff->getName()))");//####
                    result = 1;
                }
                outPort.close();
            }
            else
            {
                OD_SYSLOG("! (outPort.open(aName))");//####
                result = 1;
            }
        }
        else
        {
            result = 1;
        }
        delete stuff;
    }
    else
    {
        result = 1;
    }
    return result;
} // doCase4

/*! @brief A test input handler. */
class Test5Handler : public YarpPlusPlus::InputHandler
{
public:
    
    /*! @brief The constructor. */
    Test5Handler(void);
    
    /*! @brief The destructor. */
    virtual ~Test5Handler(void);
    
    /*! @brief Process partially-structured input data.
     @param input The partially-structured input data.
     @param replyMechanism @c NULL if no reply is expected and non-@c NULL otherwise.
     @returns @c true if the input was correctly structured and successfully processed. */
    virtual bool handleInput(yarp::os::Bottle &           input,
                             yarp::os::ConnectionWriter * replyMechanism);
    
protected:
    
private:
    
}; // Test5Handler

#pragma mark Class methods

#pragma mark Constructors and destructors

Test5Handler::Test5Handler(void) :
        InputHandler()
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // Test5Handler::Test5Handler

Test5Handler::~Test5Handler(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // Test5Handler::~Test5Handler

#pragma mark Actions

bool Test5Handler::handleInput(yarp::os::Bottle &           input,
                               yarp::os::ConnectionWriter * replyMechanism)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P1("replyMechanism = ", replyMechanism);//####
    OD_SYSLOG_S1("got ", input.toString().c_str());//####
    if (replyMechanism)
    {
        input.write(*replyMechanism);
    }
    OD_SYSLOG_EXIT_B(true);//####
    return true;
} // Test5Handler::handleInput

#pragma mark Accessors

/*! @brief A test input handler creator. */
class Test5HandlerCreator : public YarpPlusPlus::InputHandlerCreator
{
public:
    
    /*! @brief The constructor. */
    Test5HandlerCreator(void);
    
    /*! @brief The destructor. */
    virtual ~Test5HandlerCreator(void);
    
    /*! @brief Create a new InputHandler object to process input data.
     @returns A new InputHandler or @c NULL if one cannot be created. */
    virtual YarpPlusPlus::InputHandler * create(void);

protected:
    
private:
    
}; // Test5HandlerCreator

#pragma mark Class methods

#pragma mark Constructors and destructors

Test5HandlerCreator::Test5HandlerCreator(void) :
        InputHandlerCreator()
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // Test5HandlerCreator::Test5HandlerCreator

Test5HandlerCreator::~Test5HandlerCreator(void)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_EXIT();//####
} // Test5HandlerCreator::~Test5HandlerCreator

#pragma mark Actions

YarpPlusPlus::InputHandler * Test5HandlerCreator::create(void)
{
    return new Test5Handler;
} // Test5HandlerCreator::create

#pragma mark Accessors

#pragma mark Local functions

/*! @brief Perform a test case.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doCase5(int     argc,
                   char ** argv) // send to endpoint
{
    int                      result;
    YarpPlusPlus::Endpoint * stuff = doCreateEndpointForTest(argc, argv);
    
    if (stuff)
    {
        EndpointStatusReporter reporter;
        Test5HandlerCreator    handlerCreator;

        if (stuff->setInputHandlerCreator(handlerCreator) && stuff->open() && stuff->setReporter(reporter, true))
        {
            OD_SYSLOG_S1("endpoint name = ", stuff->getName().c_str());//####
            // Now we try to connect!
            yarp::os::Port        outPort;
            yarp::os::ConstString aName = YarpPlusPlus::Endpoint::getRandomPortName();
            
            OD_SYSLOG_S1("opening ", aName.c_str());//####
            if (outPort.open(aName))
            {
                OD_SYSLOG("(outPort.open(aName))");//####
                outPort.getReport(reporter);
                if (outPort.addOutput(stuff->getName()))
                {
                    OD_SYSLOG("(outPort.addOutput(stuff->getName()))");//####
                    yarp::os::Bottle message;
                    yarp::os::Bottle response;
                    
                    message.addString(aName);
                    message.addString("howdi");
                    if (outPort.write(message, response))
                    {
                        OD_SYSLOG("(outPort.write(message, response))");//####
                        OD_SYSLOG_S1("got ", response.toString().c_str());//####
                        result = 0;
                    }
                    else
                    {
                        OD_SYSLOG("! (outPort.write(message, response))");//####
                        result = 1;
                    }
                }
                else
                {
                    OD_SYSLOG("! (outPort.addOutput(stuff->getName()))");//####
                    result = 1;
                }
                outPort.close();
            }
            else
            {
                OD_SYSLOG("! (outPort.open(aName))");//####
                result = 1;
            }
        }
        else
        {
            result = 1;
        }
        delete stuff;
    }
    else
    {
        result = 1;
    }
    return result;
} // doCase5

/*! @brief Perform a test case.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doCase10(int     argc,
                    char ** argv) // create request
{
    // Simple new / delete
    YarpPlusPlus::ServiceRequest * stuff = new YarpPlusPlus::ServiceRequest;
    
    delete stuff;
    return 0;
} // doCase10

/*! @brief Perform a test case.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doCase11(int     argc,
                    char ** argv) // create response
{
    // Simple new / delete
    YarpPlusPlus::ServiceResponse * stuff = new YarpPlusPlus::ServiceResponse;
    
    delete stuff;
    return 0;
} // doCase11

/*! @brief Perform a test case.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doCase12(int     argc,
                    char ** argv) // create service
{
    // Simple new / delete
    YarpPlusPlus::BaseService * stuff = new YarpPlusPlus::BaseService;
    
    delete stuff;
    return 0;
} // doCase12

/*! @brief Perform a test case.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used for the test.
 @returns @c 0 on success and @c 1 on failure. */
static int doCase13(int     argc,
                    char ** argv) // create client
{
    // Simple new / delete
    YarpPlusPlus::BaseClient * stuff = new YarpPlusPlus::BaseClient;
    
    delete stuff;
    return 0;
} // doCase13

#pragma mark Global functions

/*! @brief The entry point for unit tests.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the unit tests.
 @returns @c 0 on a successful test and @c 1 on failure. */
int main(int     argc,
         char ** argv)
{
    OD_SYSLOG_INIT(*argv, kODSyslogOptionIncludeProcessID | kODSyslogOptionIncludeThreadID);//####
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("YARP Version = ", YARP_VERSION_STRING);//####
    yarp::os::Network yarp; // This is necessary to establish any connection to the YARP infrastructure
    int               result;
    
    if (--argc > 0)
    {
        int selector = atoi(argv[1]);
        
        cout << selector << endl;
        switch (selector)
        {
            case 1:
                result = doCase1(argc - 1, argv + 2);
                break;
                
            case 2:
                result = doCase2(argc - 1, argv + 2);
                break;
                
            case 3:
                result = doCase3(argc - 1, argv + 2);
                break;
                
            case 4:
                result = doCase4(argc - 1, argv + 2);
                break;
                
            case 5:
                result = doCase5(argc - 1, argv + 2);
                break;
                
            case 10:
                result = doCase10(argc - 1, argv + 2);
                break;
                
            case 11:
                result = doCase11(argc - 1, argv + 2);
                break;
                
            case 12:
                result = doCase12(argc - 1, argv + 2);
                break;
                
            case 13:
                result = doCase13(argc - 1, argv + 2);
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
