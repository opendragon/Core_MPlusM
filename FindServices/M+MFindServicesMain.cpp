//--------------------------------------------------------------------------------------------------
//
//  File:       M+MFindServicesMain.cpp
//
//  Project:    M+M
//
//  Contains:   A utility application to search for matching services.
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
//                  list of conditions and the following disclaimer in the documentation and / or
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
//  Created:    2014-10-06
//
//--------------------------------------------------------------------------------------------------

#include <mpm/M+MUtilities.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if (! MAC_OR_LINUX_) //ASSUME WINDOWS
# include <mpm/getopt.h>
#endif //(! MAC_OR_LINUX_)

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief A utility application to search for matching services. */

/*! @dir FindServices
 @brief The set of files that implement the find services application. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using std::cin;
using std::cout;
using std::endl;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief The accepted command line arguments for the service. */
#define FINDSERVICES_OPTIONS STANDARD_OPTIONS "c:"

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

/*! @brief Display the available commands. */
static void displayCommands(void)
{
    OD_LOG_ENTER(); //####
    cout << "Commands:" << endl;
    cout << "  ? - display this list" << endl;
    cout << "  f - enter a search criteria for services" << endl;
    cout << "  q - quit the application" << endl;
    OD_LOG_EXIT(); //####
} // displayCommands

/*! @brief Produce a list of matching channel names from the given criteria.
 @param criteria The matching criteria to use.
 @param flavour The output format to be used. */
static void getMatchingChannels(const yarp::os::ConstString & criteria,
                                const OutputFlavour           flavour)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("criteria = ", criteria); //####
    bool         reported = false;
    StringVector services;
    
    if (Utilities::GetServiceNamesFromCriteria(criteria, services, true))
    {
        if (kOutputFlavourJSON == flavour)
        {
            cout << "[ ";
        }
        if (0 < services.size())
        {
            for (StringVector::const_iterator walker(services.begin()); services.end() != walker;
                 ++walker)
            {
                switch (flavour)
                {
                    case kOutputFlavourTabs :
                        if (reported)
                        {
                            cout << "\t";
                        }
                        cout << SanitizeString(*walker).c_str();
                        break;
                        
                    case kOutputFlavourJSON :
                        if (reported)
                        {
                            cout << ", ";
                        }
                        cout << T_(CHAR_DOUBLEQUOTE);
                        cout << SanitizeString(*walker).c_str();
                        cout << T_(CHAR_DOUBLEQUOTE);
                        break;

                    case kOutputFlavourNormal :
                        if (reported)
                        {
                            cout << ", ";
                        }
                        cout << SanitizeString(*walker).c_str();
                        break;
                        
                    default :
                        break;
                        
                }
                reported = true;
            }
        }
        switch (flavour)
        {
            case kOutputFlavourTabs :
                if (reported)
                {
                    cout << endl;
                }
                break;
                
            case kOutputFlavourJSON :
                cout << " ]" << endl;
                break;
                
            case kOutputFlavourNormal :
                if (reported)
                {
                    cout << endl;
                }
                else
                {
                    cout << "No services found." << endl;
                }
                break;
                
            default :
                break;
                
        }
    }
    else
    {
        switch (flavour)
        {
            case kOutputFlavourTabs :
                cout << "There was a problem with the criteria." << endl;
                break;
                
            case kOutputFlavourJSON :
                cout << T_(CHAR_DOUBLEQUOTE) << "There was a problem with the criteria." <<
                        T_(CHAR_DOUBLEQUOTE) << endl;
                break;
                
            case kOutputFlavourNormal :
                cout << "There was a problem with the criteria." << endl;
                break;
                
            default :
                break;
                
        }
    }
    cout.flush();
    OD_LOG_EXIT(); //####
} // getMatchingChannels

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

/*! @brief The entry point for finding matching services.
 
 There are no input arguments and standard output will receive a list of the matching services.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the example client.
 @returns @c 0 on a successful test and @c 1 on failure. */
int main(int      argc,
         char * * argv)
{
    OD_LOG_INIT(*argv, kODLoggingOptionIncludeProcessID | kODLoggingOptionIncludeThreadID | //####
                kODLoggingOptionEnableThreadSupport | kODLoggingOptionWriteToStderr); //####
    OD_LOG_ENTER(); //####
#if MAC_OR_LINUX_
    SetUpLogger(*argv);
#endif // MAC_OR_LINUX_
    yarp::os::ConstString criteria;
    OutputFlavour         flavour = kOutputFlavourNormal;

    opterr = 0; // Suppress the error message resulting from an unknown option.
    for (int cc = getopt(argc, argv, FINDSERVICES_OPTIONS); -1 != cc;
         cc = getopt(argc, argv, FINDSERVICES_OPTIONS))
    {
        switch (cc)
        {
            case 'c' :
                criteria = optarg;
                break;
                
            case 'j' :
                flavour = kOutputFlavourJSON;
                break;
                
            case 't' :
                flavour = kOutputFlavourTabs;
                break;
                
            default :
                // Ignore unknown options.
                break;
                
        }
    }
    try
    {
        Utilities::CheckForNameServerReporter();
#if CheckNetworkWorks_
        if (yarp::os::Network::checkNetwork(NETWORK_CHECK_TIMEOUT))
#endif // CheckNetworkWorks_
        {
            yarp::os::Network yarp; // This is necessary to establish any connections to the YARP
                                    // infrastructure
            
            Initialize(*argv);
            if (0 < criteria.size())
            {
                getMatchingChannels(criteria, flavour);
            }
            else if (CanReadFromStandardInput())
            {
                StartRunning();
                for ( ; IsRunning(); )
                {
                    char        inChar;
                    std::string inputLine;
                    
                    cout << "Operation: [? f q]? ";
                    cout.flush();
                    cin >> inChar;
                    switch (inChar)
                    {
                        case '?' :
                            // Help
                            displayCommands();
                            break;
                            
                        case 'f' :
                        case 'F' :
                            cout << "Match criteria: ";
                            cout.flush();
                            cin >> inChar;
                            if (getline(cin, inputLine))
                            {
                                inputLine = inChar + inputLine;
                                criteria = inputLine.c_str();
                                if (0 < criteria.size())
                                {
                                    getMatchingChannels(criteria, flavour);
                                }
                            }
                            break;
                            
                        case 'q' :
                        case 'Q' :
                            // Quit
                            StopRunning();
                            break;
                            
                        default :
                            cout << "Unrecognized request '" << inChar << "'." << endl;
                            break;
                            
                    }
                }
            }
        }
#if CheckNetworkWorks_
        else
        {
            OD_LOG("! (yarp::os::Network::checkNetwork(NETWORK_CHECK_TIMEOUT))"); //####
# if MAC_OR_LINUX_
            GetLogger().fail("YARP network not running.");
# else // ! MAC_OR_LINUX_
            std::cerr << "YARP network not running." << std::endl;
# endif // ! MAC_OR_LINUX_
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
