//--------------------------------------------------------------------------------------
//
//  File:       ServiceLister/main.cpp
//
//  Project:    M+M
//
//  Contains:   A utility application to list the available services.
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
//  Created:    2014-03-12
//
//--------------------------------------------------------------------------------------

#include "M+MUtilities.h"

//#include "ODEnableLogging.h"
#include "ODLogging.h"

#include <cstring>
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
#include <yarp/os/all.h>
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief A utility application to list the available services. */

/*! @dir ServiceLister
 @brief The ServiceLister application. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

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

/*! @brief The entry point for listing the available services.

 There are no input arguments and standard output will receive a list of the available services.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the example client.
 @returns @c 0 on a successful test and @c 1 on failure. */
int main(int      argc,
         char * * argv)
{
#if MAC_OR_LINUX_
# pragma unused(argc)
#endif // MAC_OR_LINUX_
    OD_LOG_INIT(*argv, kODLoggingOptionIncludeProcessID | kODLoggingOptionIncludeThreadID |//####
                kODLoggingOptionEnableThreadSupport | kODLoggingOptionWriteToStderr);//####
    OD_LOG_ENTER();//####
    try
    {
#if CheckNetworkWorks_
        if (yarp::os::Network::checkNetwork())
#endif // CheckNetworkWorks_
        {
            yarp::os::Network yarp; // This is necessary to establish any connection to the YARP infrastructure
            
            MplusM::Common::Initialize(*argv);
            MplusM::Common::StringVector services;
            
            MplusM::Utilities::GetServiceNames(services);
            bool reported = false;
            
            for (MplusM::Common::StringVector::const_iterator walker(services.begin()); services.end() != walker;
                 ++walker)
            {
                MplusM::Utilities::ServiceDescriptor descriptor;
                
                if (MplusM::Utilities::GetNameAndDescriptionForService(*walker, descriptor, STANDARD_WAIT_TIME))
                {
                    if (! reported)
                    {
                        cout << "Services: " << endl;
                    }
                    reported = true;
                    cout << endl;
                    cout << "Service port:      " << walker->c_str() << endl;
                    cout << "Service name:      " << descriptor._canonicalName.c_str() << endl;
                    MplusM::OutputDescription(cout, "Description:       ", descriptor._description);
                    MplusM::OutputDescription(cout, "Requests:          ", descriptor._requestsDescription);
                    cout << "Path:              " << descriptor._path.c_str() << endl;
                    bool                  sawInputs = false;
                    bool                  sawOutputs = false;
                    yarp::os::ConstString channelNames;
                    
                    for (MplusM::Common::StringVector::const_iterator walker(descriptor._inputChannels.begin());
                         descriptor._inputChannels.end() != walker; ++walker)
                    {
                        if (sawInputs)
                        {
                            channelNames += " ";
                        }
                        channelNames += *walker;
                        sawInputs = true;
                    }
                    MplusM::OutputDescription(cout, "Secondary inputs:  ", channelNames);
                    channelNames = "";
                    for (MplusM::Common::StringVector::const_iterator walker(descriptor._outputChannels.begin());
                         descriptor._outputChannels.end() != walker; ++walker)
                    {
                        if (sawOutputs)
                        {
                            channelNames += " ";
                        }
                        channelNames += *walker;
                        sawOutputs = true;
                    }
                    MplusM::OutputDescription(cout, "Secondary outputs: ", channelNames);
                }
            }
            if (reported)
            {
                cout << endl;
            }
            else
            {
                cout << "No services found." << endl;
            }
        }
#if CheckNetworkWorks_
        else
        {
            OD_LOG("! (yarp::os::Network::checkNetwork())");//####
            cerr << "YARP network not running." << endl;
        }
#endif // CheckNetworkWorks_
    }
    catch (...)
    {
        OD_LOG("Exception caught");//####
    }
    yarp::os::Network::fini();
    OD_LOG_EXIT_L(0);//####
    return 0;
} // main
