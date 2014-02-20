//
//  YPPBaseService.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-06.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#include "YPPBaseService.h"
#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPException.h"

#pragma mark Private structures and constants

#pragma mark Local functions

#pragma mark Class methods

#pragma mark Constructors and destructors

YarpPlusPlus::BaseService::BaseService(const yarp::os::ConstString & serviceEndpointName,
                                       const yarp::os::ConstString & serviceHostName,
                                       const yarp::os::ConstString & servicePortNumber) :
        _started(false), _endpoint(NULL)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S3("serviceEndpointName = ", serviceEndpointName.c_str(), "serviceHostName = ",//####
                 serviceHostName.c_str(), "servicePortNumber = ", servicePortNumber.c_str());//####
    _endpoint = new Endpoint(serviceEndpointName, serviceHostName, servicePortNumber);
    OD_SYSLOG_EXIT();//####
} // YarpPlusPlus::BaseService::BaseService

YarpPlusPlus::BaseService::BaseService(const int argc,
                                       char **   argv) :
        _started(false), _endpoint(NULL)
{
    OD_SYSLOG_ENTER();//####
    switch (argc)
    {
            // Argument order for tests = endpoint name [, IP address / name [, port [, carrier]]]
        case 1:
            _endpoint = new YarpPlusPlus::Endpoint(*argv);
            break;
            
        case 2:
            _endpoint = new YarpPlusPlus::Endpoint(*argv, argv[1]);
            break;
            
        case 3:
            _endpoint = new YarpPlusPlus::Endpoint(*argv, argv[1], argv[2]);
            break;
            
        default:
            OD_SYSLOG_EXIT_THROW_S("Invalid parameters for service endpoint");//####
            throw new YarpPlusPlus::Exception("Invalid parameters for service endpoint");
            
    }
    OD_SYSLOG_EXIT();//####
} // YarpPlusPlus::BaseService::BaseService

YarpPlusPlus::BaseService::~BaseService(void)
{
    OD_SYSLOG_ENTER();//####
    delete _endpoint;
    OD_SYSLOG_EXIT();//####    
} // YarpPlusPlus::BaseService::~BaseService

#pragma mark Actions

bool YarpPlusPlus::BaseService::start(void)
{
    OD_SYSLOG_ENTER();//####
    _started = true;
    OD_SYSLOG_EXIT_B(_started);//####
    return _started;
} // YarpPlusPlus::BaseService::start

bool YarpPlusPlus::BaseService::stop(void)
{
    OD_SYSLOG_ENTER();//####
    _started = false;
    OD_SYSLOG_EXIT_B(! _started);//####
    return (! _started);
} // YarpPlusPlus::BaseService::stop

#pragma mark Accessors

#pragma mark Global functions
