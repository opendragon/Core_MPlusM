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

using namespace yarp::os;
using namespace YarpPlusPlus;

using std::cout;
using std::cerr;
using std::endl;

class EndpointStatusReporter : public PortReport
{
public:
    EndpointStatusReporter(void) {}
    virtual ~EndpointStatusReporter(void) {}
    virtual void report(const PortInfo & info);
protected:
private:
}; // EndpointStatusReporter

void EndpointStatusReporter::report(const PortInfo & info)
{
    OD_SYSLOG_L1("tag = ", info.tag);
    OD_SYSLOG_B2("incoming = ", info.incoming, "created = ", info.created);
    OD_SYSLOG_S4("portName = ", info.portName.c_str(), "sourceName = ", info.sourceName.c_str(),
                 "targetName = ", info.targetName.c_str(), "carrierName = ", info.carrierName.c_str());
    OD_SYSLOG_S1("message = ", info.message.c_str());
} // EndpointStatusReporter::report

static Endpoint * doCreateEndpointForTest(int     argc,
                                          char ** argv)
{
    Endpoint * stuff = NULL;
    
    if (argc > 0)
    {
        switch (argc)
        {
            // Argument order for tests = endpoint name [, IP address / name [, port [, carrier]]]
            case 1:
                stuff = new Endpoint(*argv);
                break;
                
            case 2:
                stuff = new Endpoint(*argv, argv[1]);
                break;
                
            case 3:
                stuff = new Endpoint(*argv, argv[1], argv[2]);
                break;
                
            case 4:
                stuff = new Endpoint(*argv, argv[1], argv[2], argv[3]);
                break;
                
            default:
                break;
                
        }
    }
    return stuff;
} // doCreateEndpointForTest

static int doCase1(int     argc,
                   char ** argv) // create endpoint
{
    int        result;
    Endpoint * stuff = doCreateEndpointForTest(argc, argv);
    
    if (stuff)
    {
        OD_SYSLOG_S1("endpoint name = ", stuff->getName().c_str());//####
        delete stuff;
        result = 0;
    }
    else
    {
        result = 1;
    }
    return result;
} // doCase1

static int doCase2(int     argc,
                   char ** argv) // connect to endpoint
{
    int        result;
    Endpoint * stuff = doCreateEndpointForTest(argc, argv);
    
    if (stuff)
    {
        EndpointStatusReporter reporter;

        stuff->setReporter(reporter, true);
        OD_SYSLOG_S1("endpoint name = ", stuff->getName().c_str());//####
        // Now we try to connect!
        Port        outPort;
        ConstString aName = Endpoint::getRandomPortName();
        
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
        delete stuff;
    }
    else
    {
        result = 1;
    }
    return result;
} // doCase2

class Test3Handler : public InputHandler
{
public:
    Test3Handler(void) {}
    
    virtual ~Test3Handler(void) {}
    
    virtual bool handleInput(Bottle &           input,
                             ConnectionWriter * replyMechanism);
    
protected:
private:
    
}; // Test3Handler

bool Test3Handler::handleInput(Bottle &           input,
                               ConnectionWriter * replyMechanism)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_P1("replyMechanism = ", replyMechanism);//####
    OD_SYSLOG_S1("got ", input.toString().c_str());//####
    OD_SYSLOG_EXIT_B(true);//####
    return true;
} // Test3Handler::handleInput

static int doCase3(int     argc,
                   char ** argv) // send to endpoint
{
    int        result;
    Endpoint * stuff = doCreateEndpointForTest(argc, argv);
    
    if (stuff)
    {
        EndpointStatusReporter reporter;
        Test3Handler           handler;
        
        stuff->setReporter(reporter, true);
        stuff->setInputHandler(handler);
        OD_SYSLOG_S1("endpoint name = ", stuff->getName().c_str());//####
        // Now we try to connect!
        Port        outPort;
        ConstString aName = Endpoint::getRandomPortName();
        
        OD_SYSLOG_S1("opening ", aName.c_str());//####
        if (outPort.open(aName))
        {
            OD_SYSLOG("(outPort.open(aName))");//####
            outPort.getReport(reporter);
            if (outPort.addOutput(stuff->getName()))
            {
                OD_SYSLOG("(outPort.addOutput(stuff->getName()))");//####
                Bottle message;
                
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
        delete stuff;
    }
    else
    {
        result = 1;
    }
    return result;
} // doCase3

class Test4Handler : public InputHandler
{
public:
    Test4Handler(void) {}
    
    virtual ~Test4Handler(void) {}
    
    virtual bool handleInput(Bottle &           input,
                             ConnectionWriter * replyMechanism);
    
protected:
private:
    
}; // Test4Handler

bool Test4Handler::handleInput(Bottle &           input,
                               ConnectionWriter * replyMechanism)
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
} // Test8Handler::handleInput

static int doCase4(int     argc,
                   char ** argv) // send to endpoint
{
    int        result;
    Endpoint * stuff = doCreateEndpointForTest(argc, argv);
    
    if (stuff)
    {
        EndpointStatusReporter reporter;
        Test4Handler           handler;
        
        stuff->setReporter(reporter, true);
        stuff->setInputHandler(handler);
        OD_SYSLOG_S1("endpoint name = ", stuff->getName().c_str());//####
        // Now we try to connect!
        Port        outPort;
        ConstString aName = Endpoint::getRandomPortName();
        
        OD_SYSLOG_S1("opening ", aName.c_str());//####
        if (outPort.open(aName))
        {
            OD_SYSLOG("(outPort.open(aName))");//####
            outPort.getReport(reporter);
            if (outPort.addOutput(stuff->getName()))
            {
                OD_SYSLOG("(outPort.addOutput(stuff->getName()))");//####
                Bottle message;
                Bottle response;
                
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
        delete stuff;
    }
    else
    {
        result = 1;
    }
    return result;
} // doCase4

static int doCase10(int     argc,
                    char ** argv) // create request
{
    // Simple new / delete
    ServiceRequest * stuff = new ServiceRequest;
    
    delete stuff;
    return 0;
} // doCase10

static int doCase11(int     argc,
                    char ** argv) // create response
{
    // Simple new / delete
    ServiceResponse * stuff = new ServiceResponse;
    
    delete stuff;
    return 0;
} // doCase11

static int doCase12(int     argc,
                    char ** argv) // create service
{
    // Simple new / delete
    BaseService * stuff = new BaseService;
    
    delete stuff;
    return 0;
} // doCase12

static int doCase13(int     argc,
                    char ** argv) // create client
{
    // Simple new / delete
    BaseClient * stuff = new BaseClient;
    
    delete stuff;
    return 0;
} // doCase13

int main(int     argc,
         char ** argv)
{
    OD_SYSLOG_INIT(*argv, kODSyslogOptionIncludeProcessID | kODSyslogOptionIncludeThreadID);//####
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("YARP Version = ", YARP_VERSION_STRING);//####
    Network yarp; // This is necessary to establish any connection to the YARP infrastructure
    int     result;
    
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
