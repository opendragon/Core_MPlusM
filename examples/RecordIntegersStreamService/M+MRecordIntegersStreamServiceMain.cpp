//--------------------------------------------------------------------------------------------------
//
//  File:       RecordIntegersStreamServiceMain.cpp
//
//  Project:    M+M
//
//  Contains:   The main application for the record integers output stream.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2014 by HPlus Technologies Ltd. and Simon Fraser University.
//
//              All rights reserved. Redistribution and use in source and binary forms, with or
//              without modification, are permitted provided that the following conditions are met:
//                * Redistributions of source code must retain the above copyright notice, this list
//                  of conditions and the following disclaimer.
//                * Redistributions in binary form must reproduce the above copyright notice, this
//                  list of conditions and the following disclaimer in the documentation and/or
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
//  Created:    2014-06-24
//
//--------------------------------------------------------------------------------------------------

#include "M+MEndpoint.h"
#include "M+MRecordIntegersStreamService.h"

//#include "ODEnableLogging.h"
#include "ODLogging.h"

#if (! MAC_OR_LINUX_) //ASSUME WINDOWS
# include "getopt.h"
#endif //(! MAC_OR_LINUX_)

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The main application for the record integers output stream. */

/*! @dir RecordIntegersStreamService
 @brief The set of files that implement the record integers output stream. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Example;
using std::cin;
using std::cout;
using std::endl;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief The accepted command line arguments for the service. */
#define RECORDINTEGERSSTREAM_OPTIONS "p:"

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

/*! @brief The entry point for running the record integers output stream service.
 
 The second, optional, argument is the port number to be used and the first, optional, argument is the name of the
 channel to be used. There is no output.
 The option 'p' specifies the path to the file being written.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the example service.
 @returns @c 0 on a successful test and @c 1 on failure. */
int main(int     argc,
         char ** argv)
{
#if defined(MpM_ServicesLogToStandardError)
    OD_LOG_INIT(*argv, kODLoggingOptionIncludeProcessID | kODLoggingOptionIncludeThreadID | //####
                kODLoggingOptionWriteToStderr | kODLoggingOptionEnableThreadSupport); //####
#else // ! defined(MpM_ServicesLogToStandardError)
    OD_LOG_INIT(*argv, kODLoggingOptionIncludeProcessID | kODLoggingOptionIncludeThreadID | //####
                kODLoggingOptionEnableThreadSupport); //####
#endif // ! defined(MpM_ServicesLogToStandardError)
    OD_LOG_ENTER(); //####
    MplusM::Common::SetUpLogger(*argv);
    try
    {
        bool                  stdinAvailable = MplusM::CanReadFromStandardInput();
        yarp::os::ConstString recordPath;

        opterr = 0; // Suppress the error message resulting from an unknown option.
        for (int cc = getopt(argc, argv, RECORDINTEGERSSTREAM_OPTIONS); -1 != cc;
             cc = getopt(argc, argv, RECORDINTEGERSSTREAM_OPTIONS))
        {
            switch (cc)
            {
                case 'p' :
                    recordPath = optarg;
                    OD_LOG_S1s("recordPath <- ", recordPath); //####
                    break;
                    
                default :
                    // Ignore unknown options.
                    break;
                    
            }
        }
#if CheckNetworkWorks_
        if (yarp::os::Network::checkNetwork())
#endif // CheckNetworkWorks_
        {
            yarp::os::Network     yarp; // This is necessary to establish any connection to the YARP
                                        // infrastructure
            yarp::os::ConstString serviceEndpointName;
            yarp::os::ConstString servicePortNumber;
            
            MplusM::Common::Initialize(*argv);
            // Note that we can't use Random::uniform util after the seed has been set
            if (0 == recordPath.size())
            {
                char buff[40]; // Should be more than adequate!
                int  randNumb = yarp::os::Random::uniform(0, 10000);
                
#if MAC_OR_LINUX_
                snprintf(buff, sizeof(buff), "/tmp/record_%x", randNumb);
#else // ! MAC_OR_LINUX_
                _snprintf(buff, sizeof(buff) - 1, "/tmp/record_%x", randNumb);
                // Correct for the weird behaviour of _snprintf
                buff[sizeof(buff) - 1] = '\0';
#endif // ! MAC_OR_LINUX_
                recordPath = buff;
                OD_LOG_S1s("recordPath <- ", recordPath); //####
            }
            if (optind >= argc)
            {
                serviceEndpointName = GetRandomChannelName(DEFAULT_RECORDINTEGERS_SERVICE_NAME);
            }
            else if ((optind + 1) == argc)
            {
                serviceEndpointName = argv[optind];
            }
            else
            {
                // 2 args
                serviceEndpointName = argv[optind];
                servicePortNumber = argv[optind + 1];
            }
            RecordIntegersStreamService * stuff = new RecordIntegersStreamService(*argv,
                                                                              serviceEndpointName,
                                                                              servicePortNumber);
            
            if (stuff)
            {
                if (stuff->start())
                {
                    yarp::os::ConstString channelName(stuff->getEndpoint().getName());
                    
                    OD_LOG_S1s("channelName = ", channelName); //####
                    if (MplusM::Common::RegisterLocalService(channelName))
                    {
                        bool             configured = false;
                        yarp::os::Bottle configureData;
                        std::string      inputLine;
                        
                        MplusM::StartRunning();
                        MplusM::Common::SetSignalHandlers(MplusM::SignalRunningStop);
                        stuff->startPinger();
                        if (! stdinAvailable)
                        {
                            configureData.addString(recordPath);
                            if (stuff->configure(configureData))
                            {
                                stuff->startStreams();
                            }
                        }
                        for ( ; MplusM::IsRunning() && stuff; )
                        {
                            if (stdinAvailable)
                            {
                                char inChar;
                                
                                cout << "Operation: [b c e q r]? ";
                                cin >> inChar;
                                switch (inChar)
                                {
                                    case 'b' :
                                    case 'B' :
                                        // Start streams
                                        if (! configured)
                                        {
                                            configureData.clear();
                                            configureData.addString(recordPath);
                                            if (stuff->configure(configureData))
                                            {
                                                configured = true;
                                            }
                                        }
                                        if (configured)
                                        {
                                            stuff->startStreams();
                                        }
                                        break;
                                        
                                    case 'c' :
                                    case 'C' :
                                        // Configure
                                        cout << "Path: ";
                                        // Eat whitespace until we get something useful.
                                        cin >> inChar;
                                        if (getline(cin, inputLine))
                                        {
                                            recordPath = yarp::os::ConstString(1, inChar);
                                            recordPath += inputLine.c_str();
                                            OD_LOG_S1s("recordPath <-", recordPath); //####
                                            configureData.clear();
                                            configureData.addString(recordPath);
                                            if (stuff->configure(configureData))
                                            {
                                                configured = true;
                                            }
                                        }
                                        break;
                                        
                                    case 'e' :
                                    case 'E' :
                                        // Stop streams
                                        stuff->stopStreams();
                                        break;
                                        
                                    case 'q' :
                                    case 'Q' :
                                        // Quit
                                        MplusM::StopRunning();
                                        break;
                                        
                                    case 'r' :
                                    case 'R' :
                                        // Restart streams
                                        if (! configured)
                                        {
                                            configureData.clear();
                                            configureData.addString(recordPath);
                                            if (stuff->configure(configureData))
                                            {
                                                configured = true;
                                            }
                                        }
                                        if (configured)
                                        {
                                            stuff->restartStreams();
                                        }
                                        break;
                                        
                                    default :
                                        cout << "Unrecognized request '" << inChar << "'." << endl;
                                        break;
                                        
                                }
                            }
                            else
                            {
#if defined(MpM_MainDoesDelayNotYield)
                                yarp::os::Time::delay(ONE_SECOND_DELAY / 10.0);
#else // ! defined(MpM_MainDoesDelayNotYield)
                                yarp::os::Time::yield();
#endif // ! defined(MpM_MainDoesDelayNotYield)
                            }
                        }
                        MplusM::Common::UnregisterLocalService(channelName);
                        stuff->stop();
                    }
                    else
                    {
                        OD_LOG("! (MplusM::Common::::RegisterLocalService(channelName))"); //####
                    }
                }
                else
                {
                    OD_LOG("! (stuff->start())"); //####
                }
                delete stuff;
            }
            else
            {
                OD_LOG("! (stuff)"); //####
            }
        }
#if CheckNetworkWorks_
        else
        {
            OD_LOG("! (yarp::os::Network::checkNetwork())"); //####
            MplusM::Common::GetLogger().fail("YARP network not running.");
        }
#endif // CheckNetworkWorks_
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
    }
    yarp::os::Network::fini();
    OD_LOG_EXIT_L(0); //####
    return 0;
} // main
