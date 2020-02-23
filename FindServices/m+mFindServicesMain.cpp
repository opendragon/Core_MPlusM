//--------------------------------------------------------------------------------------------------
//
//  File:       m+mFindServicesMain.cpp
//
//  Project:    m+m
//
//  Contains:   A utility application to search for matching services.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2014 by H Plus Technologies Ltd. and Simon Fraser University.
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

#include <m+m/m+mStringArgumentDescriptor.hpp>
#include <m+m/m+mUtilities.hpp>

//#include <odlEnable.h>
#include <odlInclude.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief A utility application to search for matching services. */

/*! @dir FindServices
 @brief The set of files that implement the Find Services application. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using std::cerr;
using std::cin;
using std::cout;
using std::endl;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

/*! @brief Display the available commands. */
static void
displayCommands(void)
{
    ODL_ENTER(); //####
    cout << "Commands:" << endl;
    cout << "  ? - display this list" << endl;
    cout << "  f - enter a search criteria for services" << endl;
    cout << "  q - quit the application" << endl;
    ODL_EXIT(); //####
} // displayCommands

/*! @brief Produce a list of matching channel names from the given criteria.
 @param[in] criteria The matching criteria to use.
 @param[in] flavour The output format to be used. */
static void
getMatchingChannels(const YarpString &  criteria,
                    const OutputFlavour flavour)
{
    ODL_ENTER(); //####
    ODL_S1s("criteria = ", criteria); //####
    bool             reported = false;
    YarpStringVector services;

    if (Utilities::GetServiceNamesFromCriteria(criteria, services, true))
    {
        if (kOutputFlavourJSON == flavour)
        {
            cout << "[ ";
        }
        if (0 < services.size())
        {
            for (YarpStringVector::const_iterator walker(services.begin());
                 services.end() != walker; ++walker)
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
                        cout << T_(CHAR_DOUBLEQUOTE_);
                        cout << SanitizeString(*walker).c_str();
                        cout << T_(CHAR_DOUBLEQUOTE_);
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
                cout << T_(CHAR_DOUBLEQUOTE_) << "There was a problem with the criteria." <<
                        T_(CHAR_DOUBLEQUOTE_) << endl;
                break;

            case kOutputFlavourNormal :
                cout << "There was a problem with the criteria." << endl;
                break;

            default :
                break;

        }
    }
    cout.flush();
    ODL_EXIT(); //####
} // getMatchingChannels

/*! @brief Set up the environment and perform the operation.
 @param[in] criteria The matching criteria for the service.
 @param[in] flavour The format for the output. */
static void
setUpAndGo(const YarpString &  criteria,
           const OutputFlavour flavour)
{
    ODL_ENTER(); //####
    ODL_S1s("criteria = ", criteria); //####
    if (0 < criteria.length())
    {
        getMatchingChannels(criteria, flavour);
    }
    else if (CanReadFromStandardInput())
    {
        YarpString localCriteria;

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
                        localCriteria = inputLine.c_str();
                        if (0 < localCriteria.length())
                        {
                            getMatchingChannels(localCriteria, flavour);
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
    ODL_EXIT(); //####
} // setUpAndGo

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

/*! @brief The entry point for finding matching services.

 The first, optional, argument is the search criteria to be used. If the search criteria is not
 specified, all service channels will be reported. Standard output will receive a list of the
 matching services.
 @param[in] argc The number of arguments in 'argv'.
 @param[in] argv The arguments to be used with the application.
 @return @c 0 on a successful test and @c 1 on failure. */
int
main(int      argc,
     char * * argv)
{
    YarpString progName(*argv);

    ODL_INIT(progName.c_str(), kODLoggingOptionIncludeProcessID | //####
             kODLoggingOptionIncludeThreadID | kODLoggingOptionEnableThreadSupport | //####
             kODLoggingOptionWriteToStderr); //####
    ODL_ENTER(); //####
#if MAC_OR_LINUX_
    SetUpLogger(progName);
#endif // MAC_OR_LINUX_
    Utilities::StringArgumentDescriptor firstArg("criteria", T_("Matching criteria for service"),
                                                 Utilities::kArgModeOptional, "");
    Utilities::DescriptorVector         argumentList;
    OutputFlavour                       flavour;

    argumentList.push_back(&firstArg);
    if (Utilities::ProcessStandardUtilitiesOptions(argc, argv, argumentList,
                                                   "Find matching services", 2014,
                                                   STANDARD_COPYRIGHT_NAME_, flavour))
    {
        try
        {
            Utilities::SetUpGlobalStatusReporter();
            Utilities::CheckForNameServerReporter();
            if (Utilities::CheckForValidNetwork())
            {
                yarp::os::Network yarp; // This is necessary to establish any connections to the
                                        // YARP infrastructure

                Initialize(progName);
                if (Utilities::CheckForRegistryService())
                {
                    YarpString criteria(firstArg.getCurrentValue());

                    setUpAndGo(criteria, flavour);
                }
                else
                {
                    ODL_LOG("! (Utilities::CheckForRegistryService())"); //####
                    MpM_FAIL_(MSG_REGISTRY_NOT_RUNNING);
                }
            }
            else
            {
                ODL_LOG("! (Utilities::CheckForValidNetwork())"); //####
                MpM_FAIL_(MSG_YARP_NOT_RUNNING);
            }
            Utilities::ShutDownGlobalStatusReporter();
        }
        catch (...)
        {
            ODL_LOG("Exception caught"); //####
        }
        yarp::os::Network::fini();
    }
    ODL_EXIT_I(0); //####
    return 0;
} // main
