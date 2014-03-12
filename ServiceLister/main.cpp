//
//  ServiceLister/main.cpp
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-03-12.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

//#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include "YPPBaseClient.h"
#include "YPPRequests.h"
#include <ace/Version.h>
#include <iostream>
#include <yarp/conf/version.h>
#include <yarp/os/all.h>

using std::cout;
using std::cerr;
using std::endl;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

/*! @brief The entry point for creating an example client.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the example client.
 @returns @c 0 on a successful test and @c 1 on failure. */
int main(int     argc,
         char ** argv)
{
#if defined(ENABLE_OD_SYSLOG)
# pragma unused(argc)
#else // ! defined(ENABLE_OD_SYSLOG)
# pragma unused(argc,argv)
#endif // ! defined(ENABLE_OD_SYSLOG)
    OD_SYSLOG_INIT(*argv, kODSyslogOptionIncludeProcessID | kODSyslogOptionIncludeThreadID |//####
                   kODSyslogOptionEnableThreadSupport);//####
    OD_SYSLOG_ENTER();//####
    cout << "YARP++ Version " << YPP_VERSION << ", YARP Version " << YARP_VERSION_STRING << ", ACE Version = " <<
            ACE_VERSION << endl;
    yarp::os::Network yarp; // This is necessary to establish any connection to the YARP infrastructure
    yarp::os::Bottle  matches(YarpPlusPlus::FindMatchingServices("request:*"));
    
    if (YarpPlusPlus::BaseClient::kExpectedResponseSize == matches.size())
    {
        // First, check if the search succeeded.
        yarp::os::ConstString matchesFirstString(matches.get(0).toString());
        
        if (! strcmp(YPP_OK_RESPONSE, matchesFirstString.c_str()))
        {
            // Now, process the second element.
            yarp::os::Bottle * matchesList = matches.get(1).asList();
            
            if (matchesList)
            {
                int matchesCount = matchesList->size();
                
                if (matchesCount)
                {
                    cout << "Service ports: ";
                    for (int ii = 0; ii < matchesCount; ++ii)
                    {
                        yarp::os::ConstString aMatch(matchesList->get(ii).toString());
                        
                        if (ii)
                        {
                            cout << ", ";
                        }
                        cout << aMatch.c_str();
                    }
                    cout << endl;
                }
                else
                {
                    cout << "No services found." << endl;
                }
            }
        }
        else
        {
            yarp::os::ConstString reason(matches.get(1).toString());
            
            cout << "Failed: " << reason.c_str() << "." << endl;
        }
    }
    else
    {
        cout << "Problem getting information from the Service Registry." << endl;
    }
    OD_SYSLOG_EXIT_L(0);//####
    return 0;
} // main
