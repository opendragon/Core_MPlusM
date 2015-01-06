//--------------------------------------------------------------------------------------------------
//
//  File:       M+MServiceListerMain.cpp
//
//  Project:    M+M
//
//  Contains:   A utility application to list the available services.
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
//  Created:    2014-03-12
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
 @brief A utility application to list the available services. */

/*! @dir ServiceLister
 @brief The set of files that implement the service lister application. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using std::cout;
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
    OD_LOG_INIT(*argv, kODLoggingOptionIncludeProcessID | kODLoggingOptionIncludeThreadID | //####
                kODLoggingOptionEnableThreadSupport | kODLoggingOptionWriteToStderr); //####
    OD_LOG_ENTER(); //####
#if MAC_OR_LINUX_
    SetUpLogger(*argv);
#endif // MAC_OR_LINUX_
    OutputFlavour flavour = kOutputFlavourNormal;

    opterr = 0; // Suppress the error message resulting from an unknown option.
    for (int cc = getopt(argc, argv, STANDARD_OPTIONS); -1 != cc;
         cc = getopt(argc, argv, STANDARD_OPTIONS))
    {
        switch (cc)
        {
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
            bool         reported = false;
            StringVector services;
            
            if (Utilities::GetServiceNames(services))
            {
                if (kOutputFlavourJSON == flavour)
                {
                    cout << "[ ";
                }
                if (0 < services.size())
                {
                    for (StringVector::const_iterator walker(services.begin());
                         services.end() != walker; ++walker)
                    {
                        Utilities::ServiceDescriptor descriptor;
                        
                        if (Utilities::GetNameAndDescriptionForService(*walker, descriptor,
                                                                       STANDARD_WAIT_TIME))
                        {
                            bool                  sawInputs = false;
                            bool                  sawOutputs = false;
                            yarp::os::ConstString description;
                            yarp::os::ConstString inChannelNames;
                            yarp::os::ConstString outChannelNames;
                            yarp::os::ConstString requests;
                            yarp::os::ConstString serviceName;
                            yarp::os::ConstString servicePortName;
                            yarp::os::ConstString tag;
                            
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
                                    inChannelNames += T_("{ " CHAR_DOUBLEQUOTE "Name"
                                                         CHAR_DOUBLEQUOTE ": " CHAR_DOUBLEQUOTE);
                                    inChannelNames += SanitizeString(iDescriptor._portName);
                                    inChannelNames += T_(CHAR_DOUBLEQUOTE ", " CHAR_DOUBLEQUOTE
                                                         "Protocol" CHAR_DOUBLEQUOTE ": "
                                                         CHAR_DOUBLEQUOTE);
                                    inChannelNames += SanitizeString(iDescriptor._portProtocol);
                                    inChannelNames += T_(CHAR_DOUBLEQUOTE " }");
                                }
                                else
                                {
                                    if (sawInputs)
                                    {
                                        inChannelNames += " ";
                                    }
                                    inChannelNames += iDescriptor._portName;
                                    if (0 < iDescriptor._portProtocol.size())
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
                                    outChannelNames += T_("{ " CHAR_DOUBLEQUOTE "Name"
                                                          CHAR_DOUBLEQUOTE ": " CHAR_DOUBLEQUOTE);
                                    outChannelNames += SanitizeString(oDescriptor._portName);
                                    outChannelNames += T_(CHAR_DOUBLEQUOTE ", " CHAR_DOUBLEQUOTE
                                                          "Protocol" CHAR_DOUBLEQUOTE ": "
                                                          CHAR_DOUBLEQUOTE);
                                    outChannelNames += SanitizeString(oDescriptor._portProtocol);
                                    outChannelNames += T_(CHAR_DOUBLEQUOTE " }");
                                }
                                else
                                {
                                    if (sawOutputs)
                                    {
                                        outChannelNames += " ";
                                    }
                                    outChannelNames += oDescriptor._portName;
                                    if (0 < oDescriptor._portProtocol.size())
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
                            servicePortName = SanitizeString(*walker,
                                                             kOutputFlavourJSON != flavour);
                            serviceName = SanitizeString(descriptor._serviceName,
                                                         kOutputFlavourJSON != flavour);
                            tag = SanitizeString(descriptor._tag, kOutputFlavourJSON != flavour);
                            switch (flavour)
                            {
                                case kOutputFlavourJSON :
                                    cout << T_(CHAR_DOUBLEQUOTE "ServicePort" CHAR_DOUBLEQUOTE ": "
                                               CHAR_DOUBLEQUOTE) << servicePortName.c_str() <<
                                            T_(CHAR_DOUBLEQUOTE ", ");
                                    cout << T_(CHAR_DOUBLEQUOTE "ServiceName" CHAR_DOUBLEQUOTE ": "
                                               CHAR_DOUBLEQUOTE) << serviceName.c_str() <<
                                            T_(CHAR_DOUBLEQUOTE ", ");
                                    cout << T_(CHAR_DOUBLEQUOTE "Tag" CHAR_DOUBLEQUOTE ": "
                                               CHAR_DOUBLEQUOTE) << tag.c_str() <<
                                            T_(CHAR_DOUBLEQUOTE ", ");
                                    cout << T_(CHAR_DOUBLEQUOTE "ServiceKind" CHAR_DOUBLEQUOTE ": "
                                               CHAR_DOUBLEQUOTE) << descriptor._kind.c_str() <<
                                            T_(CHAR_DOUBLEQUOTE ", ");
                                    description = SanitizeString(descriptor._description);
                                    cout << T_(CHAR_DOUBLEQUOTE "Description" CHAR_DOUBLEQUOTE ": "
                                               CHAR_DOUBLEQUOTE) << description.c_str() <<
                                            T_(CHAR_DOUBLEQUOTE ", ");
                                    requests = SanitizeString(descriptor._requestsDescription);
                                    cout << T_(CHAR_DOUBLEQUOTE "Requests" CHAR_DOUBLEQUOTE ": "
                                               CHAR_DOUBLEQUOTE) << requests.c_str() <<
                                            T_(CHAR_DOUBLEQUOTE ", ");
                                    cout << T_(CHAR_DOUBLEQUOTE "Path" CHAR_DOUBLEQUOTE ": "
                                               CHAR_DOUBLEQUOTE) << descriptor._path.c_str() <<
                                            T_(CHAR_DOUBLEQUOTE ", ");
                                    cout << T_(CHAR_DOUBLEQUOTE "SecondaryInputs" CHAR_DOUBLEQUOTE
                                               ": ") << inChannelNames.c_str() << ", ";
                                    cout << T_(CHAR_DOUBLEQUOTE "SecondaryOutputs" CHAR_DOUBLEQUOTE
                                               ": ") << outChannelNames.c_str() << " }";
                                    break;
                                    
                                case kOutputFlavourTabs :
                                    cout << servicePortName.c_str() << "\t";
                                    cout << serviceName.c_str() << "\t";
                                    cout << tag.c_str() << "\t";
                                    cout << descriptor._kind.c_str() << "\t";
                                    description = SanitizeString(descriptor._description, true);
                                    cout << description.c_str() << "\t";
                                    requests = SanitizeString(descriptor._requestsDescription,
                                                              true);
                                    cout << requests.c_str() << "\t" << descriptor._path.c_str() <<
                                            "\t" << inChannelNames.c_str() << "\t" <<
                                            outChannelNames.c_str();
                                    break;
                                    
                                case kOutputFlavourNormal :
                                    cout << "Service port:      " << servicePortName.c_str() <<
                                            endl;
                                    cout << "Service name:      " << serviceName.c_str() << endl;
                                    cout << "Tag:               " << tag.c_str() << endl;
                                    cout << "Service kind:      " << descriptor._kind.c_str() <<
                                            endl;
                                    OutputDescription(cout, "Description:       ",
                                                      descriptor._description);
                                    OutputDescription(cout, "Requests:          ",
                                                      descriptor._requestsDescription);
                                    cout << "Path:              " << descriptor._path.c_str() <<
                                            endl;
                                    if (0 < inChannelNames.size())
                                    {
                                        OutputDescription(cout, "Secondary inputs:  ",
                                                          inChannelNames);
                                    }
                                    if (0 < outChannelNames.size())
                                    {
                                        OutputDescription(cout, "Secondary outputs: ",
                                                          outChannelNames);
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
