//--------------------------------------------------------------------------------------------------
//
//  File:       m+mServiceListMain.cpp
//
//  Project:    m+m
//
//  Contains:   A utility application to list the available services.
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
//  Created:    2014-03-12
//
//--------------------------------------------------------------------------------------------------

#include <m+m/m+mUtilities.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief A utility application to list the available services. */

/*! @dir ServiceList
 @brief The set of files that implement the Service List application. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using std::cerr;
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

/*! @brief Set up the environment and perform the operation.
 @param flavour The format for the output. */
static void
setUpAndGo(const OutputFlavour flavour)
{
    OD_LOG_ENTER(); //####
    bool             reported = false;
    YarpStringVector services;
    
    if (Utilities::GetServiceNames(services))
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
                Utilities::ServiceDescriptor descriptor;
                
                if (Utilities::GetNameAndDescriptionForService(*walker, descriptor,
                                                               STANDARD_WAIT_TIME_))
                {
                    bool       sawClients = false;
                    bool       sawInputs = false;
                    bool       sawOutputs = false;
                    YarpString clientChannelNames;
                    YarpString description;
                    YarpString extraInformation;
                    YarpString inChannelNames;
                    YarpString kind;
                    YarpString outChannelNames;
                    YarpString requests;
                    YarpString serviceName;
                    YarpString servicePortName;
                    YarpString tag;
                    
                    switch (flavour)
                    {
                        case kOutputFlavourTabs :
                            if (reported)
                            {
                                cout << endl;
                            }
                            break;
                            
                        case kOutputFlavourJSON :
                            if (reported)
                            {
                                cout << "," << endl;
                            }
                            cout << "{ ";
                            break;
                            
                        case kOutputFlavourNormal :
                            if (! reported)
                            {
                                cout << "Services: " << endl;
                            }
                            cout << endl;
                            break;
                            
                        default :
                            break;
                            
                    }
                    reported = true;
                    if (kOutputFlavourJSON == flavour)
                    {
                        clientChannelNames = "[ ";
                        inChannelNames = "[ ";
                        outChannelNames = "[ ";
                    }
                    ChannelVector & inChannels = descriptor._inputChannels;
                    
                    for (ChannelVector::const_iterator iWalker(inChannels.begin());
                         inChannels.end() != iWalker; ++iWalker)
                    {
                        ChannelDescription iDescriptor(*iWalker);
                        
                        if (kOutputFlavourJSON == flavour)
                        {
                            if (sawInputs)
                            {
                                inChannelNames += ", ";
                            }
                            inChannelNames += T_("{ " CHAR_DOUBLEQUOTE_ "Name" CHAR_DOUBLEQUOTE_
                                                 ": " CHAR_DOUBLEQUOTE_);
                            inChannelNames += SanitizeString(iDescriptor._portName);
                            inChannelNames += T_(CHAR_DOUBLEQUOTE_ ", " CHAR_DOUBLEQUOTE_ "Protocol"
                                                 CHAR_DOUBLEQUOTE_ ": " CHAR_DOUBLEQUOTE_);
                            inChannelNames += SanitizeString(iDescriptor._portProtocol);
                            inChannelNames += T_(CHAR_DOUBLEQUOTE_ " }");
                        }
                        else
                        {
                            if (sawInputs)
                            {
                                inChannelNames += " ";
                            }
                            inChannelNames += iDescriptor._portName;
                            if (0 < iDescriptor._portProtocol.length())
                            {
                                inChannelNames += "{protocol=";
                                inChannelNames += iDescriptor._portProtocol + "}";
                            }
                        }
                        sawInputs = true;
                    }
                    if (kOutputFlavourJSON == flavour)
                    {
                        inChannelNames += " ]";
                    }
                    ChannelVector & outChannels = descriptor._outputChannels;
                    
                    for (ChannelVector::const_iterator oWalker(outChannels.begin());
                         outChannels.end() != oWalker; ++oWalker)
                    {
                        ChannelDescription oDescriptor(*oWalker);
                        
                        if (kOutputFlavourJSON == flavour)
                        {
                            if (sawOutputs)
                            {
                                outChannelNames += ", ";
                            }
                            outChannelNames += T_("{ " CHAR_DOUBLEQUOTE_ "Name" CHAR_DOUBLEQUOTE_
                                                  ": " CHAR_DOUBLEQUOTE_);
                            outChannelNames += SanitizeString(oDescriptor._portName);
                            outChannelNames += T_(CHAR_DOUBLEQUOTE_ ", " CHAR_DOUBLEQUOTE_
                                                  "Protocol" CHAR_DOUBLEQUOTE_ ": "
                                                  CHAR_DOUBLEQUOTE_);
                            outChannelNames += SanitizeString(oDescriptor._portProtocol);
                            outChannelNames += T_(CHAR_DOUBLEQUOTE_ " }");
                        }
                        else
                        {
                            if (sawOutputs)
                            {
                                outChannelNames += " ";
                            }
                            outChannelNames += oDescriptor._portName;
                            if (0 < oDescriptor._portProtocol.length())
                            {
                                outChannelNames += "{protocol=";
                                outChannelNames += oDescriptor._portProtocol + "}";
                            }
                        }
                        sawOutputs = true;
                    }
                    if (kOutputFlavourJSON == flavour)
                    {
                        outChannelNames += " ]";
                    }
                    ChannelVector & clientChannels = descriptor._clientChannels;
                    
                    for (ChannelVector::const_iterator cWalker(clientChannels.begin());
                         clientChannels.end() != cWalker; ++cWalker)
                    {
                        ChannelDescription cDescriptor(*cWalker);
                        
                        if (kOutputFlavourJSON == flavour)
                        {
                            if (sawClients)
                            {
                                clientChannelNames += ", ";
                            }
                            clientChannelNames += T_("{ " CHAR_DOUBLEQUOTE_ "Name" CHAR_DOUBLEQUOTE_
                                                     ": " CHAR_DOUBLEQUOTE_);
                            clientChannelNames += SanitizeString(cDescriptor._portName);
                            clientChannelNames += T_(CHAR_DOUBLEQUOTE_ ", " CHAR_DOUBLEQUOTE_
                                                     "Protocol" CHAR_DOUBLEQUOTE_ ": "
                                                     CHAR_DOUBLEQUOTE_);
                            clientChannelNames +=  SanitizeString(cDescriptor._portProtocol);
                            clientChannelNames += T_(CHAR_DOUBLEQUOTE_ " }");
                        }
                        else
                        {
                            if (sawClients)
                            {
                                clientChannelNames += " ";
                            }
                            clientChannelNames += cDescriptor._portName;
                            if (0 < cDescriptor._portProtocol.length())
                            {
                                clientChannelNames += "{protocol=";
                                clientChannelNames += cDescriptor._portProtocol + "}";
                            }
                        }
                        sawClients = true;
                    }
                    if (kOutputFlavourJSON == flavour)
                    {
                        clientChannelNames += " ]";
                    }
                    kind = SanitizeString(descriptor._kind, kOutputFlavourJSON != flavour);
                    servicePortName = SanitizeString(*walker, kOutputFlavourJSON != flavour);
                    serviceName = SanitizeString(descriptor._serviceName,
                                                 kOutputFlavourJSON != flavour);
                    tag = SanitizeString(descriptor._tag, kOutputFlavourJSON != flavour);
                    switch (flavour)
                    {
                        case kOutputFlavourJSON :
                            cout << T_(CHAR_DOUBLEQUOTE_ "ServicePort" CHAR_DOUBLEQUOTE_ ": "
                                       CHAR_DOUBLEQUOTE_) << servicePortName.c_str() <<
                                    T_(CHAR_DOUBLEQUOTE_ ", ");
                            cout << T_(CHAR_DOUBLEQUOTE_ "ServiceName" CHAR_DOUBLEQUOTE_ ": "
                                       CHAR_DOUBLEQUOTE_) << serviceName.c_str() <<
                                    T_(CHAR_DOUBLEQUOTE_ ", ");
                            cout << T_(CHAR_DOUBLEQUOTE_ "Tag" CHAR_DOUBLEQUOTE_ ": "
                                       CHAR_DOUBLEQUOTE_) << tag.c_str() <<
                                    T_(CHAR_DOUBLEQUOTE_ ", ");
                            cout << T_(CHAR_DOUBLEQUOTE_ "ServiceKind" CHAR_DOUBLEQUOTE_ ": "
                                       CHAR_DOUBLEQUOTE_) << kind.c_str() <<
                                    T_(CHAR_DOUBLEQUOTE_ ", ");
                            description = SanitizeString(descriptor._description);
                            cout << T_(CHAR_DOUBLEQUOTE_ "Description" CHAR_DOUBLEQUOTE_ ": "
                                       CHAR_DOUBLEQUOTE_) << "The " << description.c_str() <<
                                    T_(CHAR_DOUBLEQUOTE_ ", ");
                            extraInformation = SanitizeString(descriptor._extraInfo);
                            cout << T_(CHAR_DOUBLEQUOTE_ "ExtraInfo" CHAR_DOUBLEQUOTE_ ": "
                                       CHAR_DOUBLEQUOTE_) << extraInformation.c_str() <<
                                    T_(CHAR_DOUBLEQUOTE_ ", ");
                            requests = SanitizeString(descriptor._requestsDescription);
                            cout << T_(CHAR_DOUBLEQUOTE_ "Requests" CHAR_DOUBLEQUOTE_ ": "
                                       CHAR_DOUBLEQUOTE_) << requests.c_str() <<
                                    T_(CHAR_DOUBLEQUOTE_ ", ");
                            cout << T_(CHAR_DOUBLEQUOTE_ "Path" CHAR_DOUBLEQUOTE_ ": "
                                       CHAR_DOUBLEQUOTE_) << descriptor._path.c_str() <<
                                    T_(CHAR_DOUBLEQUOTE_ ", ");
                            cout << T_(CHAR_DOUBLEQUOTE_ "SecondaryInputs" CHAR_DOUBLEQUOTE_
                                       ": ") << inChannelNames.c_str() << ", ";
                            cout << T_(CHAR_DOUBLEQUOTE_ "SecondaryOutputs" CHAR_DOUBLEQUOTE_
                                       ": ") << outChannelNames.c_str() << " }";
                            cout << T_(CHAR_DOUBLEQUOTE_ "SecondaryClients" CHAR_DOUBLEQUOTE_
                                       ": ") << clientChannelNames.c_str() << " }";
                            break;
                            
                        case kOutputFlavourTabs :
                            cout << servicePortName.c_str() << "\t";
                            cout << serviceName.c_str() << "\t";
                            cout << tag.c_str() << "\t" << kind.c_str() << "\t";
                            description = SanitizeString(descriptor._description, true);
                            cout << "The " << description.c_str() << "\t";
                            extraInformation = SanitizeString(descriptor._extraInfo, true);
                            cout << extraInformation.c_str() << "\t";
                            requests = SanitizeString(descriptor._requestsDescription, true);
                            cout << requests.c_str() << "\t" << descriptor._path.c_str() << "\t" <<
                                    inChannelNames.c_str() << "\t" << outChannelNames.c_str() <<
                                    "\t" << clientChannelNames.c_str();
                            break;
                            
                        case kOutputFlavourNormal :
                            cout << "Service port:      " << servicePortName.c_str() << endl;
                            cout << "Service name:      " << serviceName.c_str() << endl;
                            cout << "Tag:               " << tag.c_str() << endl;
                            cout << "Service kind:      " << kind.c_str() << endl;
                            OutputDescription(cout, "Description:       ",
                                              YarpString("The ") + descriptor._description);
                            if (0 < descriptor._extraInfo.length())
                            {
                                OutputDescription(cout, "Extra information: ",
                                                  descriptor._extraInfo);
                            }
                            OutputDescription(cout, "Requests:          ",
                                              descriptor._requestsDescription);
                            cout << "Path:              " << descriptor._path.c_str() << endl;
                            if (0 < inChannelNames.size())
                            {
                                OutputDescription(cout, "Secondary inputs:  ", inChannelNames);
                            }
                            if (0 < outChannelNames.size())
                            {
                                OutputDescription(cout, "Secondary outputs: ", outChannelNames);
                            }
                            if (0 < clientChannelNames.size())
                            {
                                OutputDescription(cout, "Secondary clients: ", clientChannelNames);
                            }
                            break;
                            
                        default :
                            break;
                            
                    }
                }
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
    OD_LOG_EXIT(); //####
} // setUpAndGo

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

/*! @brief The entry point for listing the available services.
 
 There are no input arguments and standard output will receive a list of the available services.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the application.
 @returns @c 0 on a successful test and @c 1 on failure. */
int
main(int      argc,
     char * * argv)
{
    YarpString progName(*argv);

    OD_LOG_INIT(progName.c_str(), kODLoggingOptionIncludeProcessID | //####
                kODLoggingOptionIncludeThreadID | kODLoggingOptionEnableThreadSupport | //####
                kODLoggingOptionWriteToStderr); //####
    OD_LOG_ENTER(); //####
#if MAC_OR_LINUX_
    SetUpLogger(progName);
#endif // MAC_OR_LINUX_
    Utilities::DescriptorVector argumentList;
    OutputFlavour               flavour;
    
    if (Utilities::ProcessStandardUtilitiesOptions(argc, argv, argumentList,
                                                   "List the available services", 2014,
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
                    setUpAndGo(flavour);
                }
                else
                {
                    OD_LOG("! (Utilities::CheckForRegistryService())"); //####
                    MpM_FAIL_(MSG_REGISTRY_NOT_RUNNING);
                }
            }
            else
            {
                OD_LOG("! (Utilities::CheckForValidNetwork())"); //####
                MpM_FAIL_(MSG_YARP_NOT_RUNNING);
            }
            Utilities::ShutDownGlobalStatusReporter();
        }
        catch (...)
        {
            OD_LOG("Exception caught"); //####
        }
        yarp::os::Network::fini();
    }
    OD_LOG_EXIT_L(0); //####
    return 0;
} // main
