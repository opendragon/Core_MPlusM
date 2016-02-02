//--------------------------------------------------------------------------------------------------
//
//  File:       m+mPortListerMain.cpp
//
//  Project:    m+m
//
//  Contains:   A utility application to list the available ports.
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
//  Created:    2014-03-28
//
//--------------------------------------------------------------------------------------------------

#include <m+m/m+mBaseClient.h>
#include <m+m/m+mRequests.h>
#include <m+m/m+mUtilities.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief A utility application to list the available ports. */

/*! @dir PortLister
 @brief The set of files that implement the Port Lister application. */
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

/*! @brief Report the connections for a given port.
 @param flavour The format for the output.
 @param portName The port to be inspected.
 @param checker A function that provides for early exit from loops.
 @param checkStuff The private data for the early exit function. */
static void
reportConnections(const OutputFlavour flavour,
                  const YarpString &  portName,
                  CheckFunction       checker,
                  void *              checkStuff)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("portName = ", portName); //####
    OD_LOG_P1("checkStuff = ", checkStuff); //####
    bool          sawInputs = false;
    bool          sawOutputs = false;
    ChannelVector inputs;
    ChannelVector outputs;
    YarpString    inputsAsString;
    YarpString    outputsAsString;
    
    Utilities::GatherPortConnections(portName, inputs, outputs, Utilities::kInputAndOutputBoth,
                                     false, checker, checkStuff);
    if (0 < inputs.size())
    {
        for (ChannelVector::const_iterator walker(inputs.begin()); inputs.end() != walker; ++walker)
        {
            switch (flavour)
            {
                case kOutputFlavourTabs :
                    if (sawInputs)
                    {
                        inputsAsString += ", ";
                    }
                    inputsAsString += SanitizeString(walker->_portName, true);
                    switch (walker->_portMode)
                    {
                        case kChannelModeTCP :
                            inputsAsString += " TCP";
                            break;
                            
                        case kChannelModeUDP :
                            inputsAsString += " UDP";
                            break;
                            
                        case kChannelModeOther :
                            inputsAsString += " unknown";
                            break;
                            
                        default :
                            break;
                            
                    }
                    break;
                    
                case kOutputFlavourJSON :
                    if (sawInputs)
                    {
                        inputsAsString += ", ";
                    }
                    inputsAsString += T_("{ " CHAR_DOUBLEQUOTE_ "Port" CHAR_DOUBLEQUOTE_ ": "
                                         CHAR_DOUBLEQUOTE_);
                    inputsAsString += SanitizeString(walker->_portName);
                    inputsAsString += T_(CHAR_DOUBLEQUOTE_ ", " CHAR_DOUBLEQUOTE_ "Mode"
                                         CHAR_DOUBLEQUOTE_ ": " CHAR_DOUBLEQUOTE_);
                    switch (walker->_portMode)
                    {
                        case kChannelModeTCP :
                            inputsAsString += "TCP";
                            break;
                            
                        case kChannelModeUDP :
                            inputsAsString += "UDP";
                            break;
                            
                        case kChannelModeOther :
                            inputsAsString += "unknown";
                            break;
                            
                        default :
                            break;
                            
                    }
                    inputsAsString += T_(CHAR_DOUBLEQUOTE_ " }");
                    break;
                    
                case kOutputFlavourNormal :
                    inputsAsString += "   Input from ";
                    inputsAsString += SanitizeString(walker->_portName, true);
                    switch (walker->_portMode)
                    {
                        case kChannelModeTCP :
                            inputsAsString += " via TCP.";
                            break;
                            
                        case kChannelModeUDP :
                            inputsAsString += " via UDP.";
                            break;
                            
                        case kChannelModeOther :
                            inputsAsString += " via non-TCP/non-UDP.";
                            break;
                            
                        default :
                            break;
                            
                    }
                    inputsAsString += "\n";
                    break;
                    
                default :
                    break;
                    
            }
            sawInputs = true;
        }
    }
    if (0 < outputs.size())
    {
        for (ChannelVector::const_iterator walker(outputs.begin()); outputs.end() != walker;
             ++walker)
        {
            switch (flavour)
            {
                case kOutputFlavourTabs :
                    if (sawOutputs)
                    {
                        outputsAsString += ", ";
                    }
                    outputsAsString += SanitizeString(walker->_portName, true);
                    switch (walker->_portMode)
                    {
                        case kChannelModeTCP :
                            outputsAsString += " TCP";
                            break;
                            
                        case kChannelModeUDP :
                            outputsAsString += " UDP";
                            break;
                            
                        case kChannelModeOther :
                            outputsAsString += " unknown";
                            break;
                            
                        default :
                            break;
                            
                    }
                    break;
                    
                case kOutputFlavourJSON :
                    if (sawOutputs)
                    {
                        outputsAsString += ", ";
                    }
                    outputsAsString += T_("{ " CHAR_DOUBLEQUOTE_ "Port" CHAR_DOUBLEQUOTE_ ": "
                                          CHAR_DOUBLEQUOTE_);
                    outputsAsString += SanitizeString(walker->_portName);
                    outputsAsString += T_(CHAR_DOUBLEQUOTE_ ", " CHAR_DOUBLEQUOTE_ "Mode"
                                          CHAR_DOUBLEQUOTE_ ": " CHAR_DOUBLEQUOTE_);
                    switch (walker->_portMode)
                    {
                        case kChannelModeTCP :
                            outputsAsString += "TCP";
                            break;
                            
                        case kChannelModeUDP :
                            outputsAsString += "UDP";
                            break;
                            
                        case kChannelModeOther :
                            outputsAsString += "unknown";
                            break;
                            
                        default :
                            break;
                            
                    }
                    outputsAsString += T_(CHAR_DOUBLEQUOTE_ " }");
                    break;
                    
                case kOutputFlavourNormal :
                    outputsAsString += "   Output to ";
                    outputsAsString += SanitizeString(walker->_portName, true);
                    switch (walker->_portMode)
                    {
                        case kChannelModeTCP :
                            outputsAsString += " via TCP.";
                            break;
                            
                        case kChannelModeUDP :
                            outputsAsString += " via UDP.";
                            break;
                            
                        case kChannelModeOther :
                            outputsAsString += " via non-TCP/non-UDP.";
                            break;
                            
                        default :
                            break;
                            
                    }
                    outputsAsString += "\n";
                    break;
                    
                default :
                    break;
                    
            }
            sawOutputs = true;
        }
    }
    switch (flavour)
    {
        case kOutputFlavourTabs :
            cout << inputsAsString.c_str() << "\t" << outputsAsString.c_str();
            break;
            
        case kOutputFlavourJSON :
            cout << T_(CHAR_DOUBLEQUOTE_ "Inputs" CHAR_DOUBLEQUOTE_ ": [ ") <<
                    inputsAsString.c_str() << T_(" ], " CHAR_DOUBLEQUOTE_ "Outputs"
                                                 CHAR_DOUBLEQUOTE_ ": [ ") <<
                    outputsAsString.c_str() << " ]";
            break;
            
        case kOutputFlavourNormal :
            if (sawInputs || sawOutputs)
            {
                if (sawInputs)
                {
                    cout << inputsAsString.c_str();
                }
                if (sawOutputs)
                {
                    cout << outputsAsString.c_str();
                }
            }
            else
            {
                cout << "   No active connections." << endl;
            }
            break;
            
        default :
            break;
            
    }
    OD_LOG_EXIT(); //####
} // reportConnections

/*! @brief Print out connection information for a port.
 @param flavour The format for the output.
 @param aDescriptor The attributes of the port of interest.
 @param checkWithRegistry @c true if the %Registry Service is available for requests and @c false
 otherwise.
 @returns @c true if information was written out and @c false otherwise. */
static bool
reportPortStatus(const OutputFlavour               flavour,
                 const Utilities::PortDescriptor & aDescriptor,
                 const bool                        checkWithRegistry)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P1("aDescriptor = ", &aDescriptor); //####
    OD_LOG_B1("checkWithRegistry = ", checkWithRegistry); //####
    bool       result;
    YarpString portName;
    YarpString portClass;
    
    portName = SanitizeString(aDescriptor._portName, kOutputFlavourJSON != flavour);
    if (strncmp(portName.c_str(), HIDDEN_CHANNEL_PREFIX_, sizeof(HIDDEN_CHANNEL_PREFIX_) - 1))
    {
        // Only process non-hidden ports.
        switch (flavour)
        {
            case kOutputFlavourTabs :
                cout << portName.c_str() << "\t";
                break;
                
            case kOutputFlavourJSON :
                cout << T_("{ " CHAR_DOUBLEQUOTE_ "PortName" CHAR_DOUBLEQUOTE_ ": "
                           CHAR_DOUBLEQUOTE_) << portName.c_str() << T_(CHAR_DOUBLEQUOTE_ ", ");
                break;
                
            case kOutputFlavourNormal :
                cout << portName.c_str() << ": ";
                break;
                
            default :
                break;
                
        }
        if (checkWithRegistry)
        {
            YarpString request(MpM_REQREP_DICT_CHANNELNAME_KEY_ ":");
            
            request += aDescriptor._portName;
            yarp::os::Bottle matches(FindMatchingServices(request, true));
            
            OD_LOG_S1s("matches <- ", matches.toString()); //####
            if (MpM_EXPECTED_MATCH_RESPONSE_SIZE_ == matches.size())
            {
                YarpString matchesFirstString(matches.get(0).toString());
                
                if (strcmp(MpM_OK_RESPONSE_, matchesFirstString.c_str()))
                {
                    // Didn't match - use a simpler check, in case it's unregistered or is an
                    // adapter or client.
                    switch (Utilities::GetPortKind(aDescriptor._portName))
                    {
                        case Utilities::kPortKindAdapter :
                            portClass = "Adapter port";
                            break;
                            
                        case Utilities::kPortKindClient :
                            portClass = "Client port";
                            break;
                            
                        case Utilities::kPortKindRegistryService :
                            portClass = "Registry Service port";
                            break;
                            
                        case Utilities::kPortKindService :
                            portClass = "Unregistered service port";
                            break;
                            
                        case Utilities::kPortKindStandard :
                            portClass = "Standard port at ";
                            portClass += aDescriptor._portIpAddress;
                            portClass += ":";
                            portClass += aDescriptor._portPortNumber;
                            break;
                            
                        default :
                            break;
                            
                    }
                }
                else
                {
                    yarp::os::Value secondValue(matches.get(1));
                    
                    if (secondValue.isList())
                    {
                        yarp::os::Bottle * secondList = secondValue.asList();
                        
                        if (secondList && (0 < secondList->size()))
                        {
                            YarpString serviceName(matches.get(1).toString());
                            
                            if (aDescriptor._portName == MpM_REGISTRY_ENDPOINT_NAME_)
                            {
                                portClass = "Registry Service port for '";
                                portClass += serviceName;
                                portClass += "'";
                            }
                            else
                            {
                                portClass = "Service port for '";
                                portClass += serviceName;
                                portClass += "'";
                            }
                        }
                        else
                        {
                            // Didn't match - use a simpler check, in case it's unregistered or is
                            // an adapter or client.
                            switch (Utilities::GetPortKind(aDescriptor._portName))
                            {
                                case Utilities::kPortKindAdapter :
                                    portClass = "Adapter port";
                                    break;
                                    
                                case Utilities::kPortKindClient :
                                    portClass = "Client port";
                                    break;
                                    
                                case Utilities::kPortKindRegistryService :
                                    portClass = "Registry Service port";
                                    break;
                                    
                                case Utilities::kPortKindService :
                                    portClass = "Unregistered service port";
                                    break;
                                    
                                case Utilities::kPortKindStandard :
                                    portClass = "Standard port at ";
                                    portClass += aDescriptor._portIpAddress;
                                    portClass += ":";
                                    portClass += aDescriptor._portPortNumber;
                                    break;
                                    
                                default :
                                    break;
                                    
                            }
                        }
                    }
                }
            }
            switch (flavour)
            {
                case kOutputFlavourTabs :
                    cout << SanitizeString(portClass, true).c_str() << "\t";
                    break;
                    
                case kOutputFlavourJSON :
                    cout << T_(CHAR_DOUBLEQUOTE_ "PortClass" CHAR_DOUBLEQUOTE_ ": "
                               CHAR_DOUBLEQUOTE_) << SanitizeString(portClass).c_str() <<
                            T_(CHAR_DOUBLEQUOTE_ ", ");
                    break;
                    
                case kOutputFlavourNormal :
                    cout << SanitizeString(portClass, true).c_str() << ".";
                    break;
                    
                default :
                    break;
                    
            }
        }
        else
        {
            // We can't interrogate the Registry Service, so use a simple heuristic to identify
            // clients, services and adapters.
            // Didn't match - use a simpler check, in case it's unregistered or is an adapter or
            // client.
            switch (Utilities::GetPortKind(aDescriptor._portName))
            {
                case Utilities::kPortKindAdapter :
                    portClass = "Adapter port";
                    break;
                    
                case Utilities::kPortKindClient :
                    portClass = "Client port";
                    break;
                    
                case Utilities::kPortKindRegistryService :
                    portClass = "Registry Service port";
                    break;
                    
                case Utilities::kPortKindService :
                    portClass = "Unregistered service port";
                    break;
                    
                case Utilities::kPortKindStandard :
                    portClass = "Standard port at ";
                    portClass += aDescriptor._portIpAddress;
                    portClass += ":";
                    portClass += aDescriptor._portPortNumber;
                    break;
                    
                default :
                    break;
                    
            }
            switch (flavour)
            {
                case kOutputFlavourTabs :
                    cout << SanitizeString(portClass, true).c_str() << "\t";
                    break;
                    
                case kOutputFlavourJSON :
                    cout << T_(CHAR_DOUBLEQUOTE_ "PortClass" CHAR_DOUBLEQUOTE_ ": "
                               CHAR_DOUBLEQUOTE_) << SanitizeString(portClass).c_str() <<
                            T_(CHAR_DOUBLEQUOTE_ ", ");
                    break;
                    
                case kOutputFlavourNormal :
                    cout << SanitizeString(portClass, true).c_str() << ".";
                    break;
                    
                default :
                    break;
                    
            }
        }
        switch (flavour)
        {
            case kOutputFlavourTabs :
                break;
                
            case kOutputFlavourJSON :
                break;
                
            case kOutputFlavourNormal :
                cout << endl;
                break;
                
            default :
                break;
                
        }
        reportConnections(flavour, aDescriptor._portName, NULL, NULL);
        switch (flavour)
        {
            case kOutputFlavourTabs :
                break;
                
            case kOutputFlavourJSON :
                cout << " }";
                break;
                
            case kOutputFlavourNormal :
                break;
                
            default :
                break;
                
        }
        result = true;
    }
    else
    {
        result = false;
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // reportPortStatus

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

/*! @brief The entry point for listing the connection status of all visible YARP ports.
 
 There is no input and the output consists of a list of ports and what, if anything, is connected to
 them.
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
                                                   "List the connection status of YARP ports", 2014,
                                                   STANDARD_COPYRIGHT_NAME_, flavour))
    {
        try
        {
            Utilities::SetUpGlobalStatusReporter();
            Utilities::CheckForNameServerReporter();
            if (Utilities::CheckForValidNetwork())
            {
                bool                  found = false;
                yarp::os::Network     yarp; // This is necessary to establish any connections to the
                                            // YARP infrastructure
                Utilities::PortVector ports;
                
                Initialize(progName);
                Utilities::RemoveStalePorts();
                if (Utilities::GetDetectedPortList(ports, true))
                {
                    bool serviceRegistryPresent = Utilities::CheckListForRegistryService(ports);
                    
                    switch (flavour)
                    {
                        case kOutputFlavourTabs :
                            break;
                            
                        case kOutputFlavourJSON :
                            cout << "[ ";
                            break;
                            
                        case kOutputFlavourNormal :
                            cout << "Ports:" << endl;
                            break;
                            
                        default :
                            break;
                            
                    }
                    if (0 < ports.size())
                    {
                        for (Utilities::PortVector::const_iterator walker(ports.begin());
                             ports.end() != walker; ++walker)
                        {
                            switch (flavour)
                            {
                                case kOutputFlavourJSON :
                                    if (found)
                                    {
                                        cout << "," << endl;
                                    }
                                    break;
                                    
                                case kOutputFlavourTabs :
                                    if (found)
                                    {
                                        cout << endl;
                                    }
                                    break;
                                    
                                case kOutputFlavourNormal :
                                    break;
                                    
                                default :
                                    break;
                                    
                            }
                            found = reportPortStatus(flavour, *walker, serviceRegistryPresent);
                        }
                    }
                    switch (flavour)
                    {
                        case kOutputFlavourTabs :
                            if (found)
                            {
                                cout << endl;
                            }
                            break;
                            
                        case kOutputFlavourJSON :
                            cout << " ]" << endl;
                            break;
                            
                        case kOutputFlavourNormal :
                            if (found)
                            {
                                cout << endl;
                            }
                            else
                            {
                                cout << "   No ports found." << endl;
                            }
                            break;
                            
                        default :
                            break;
                            
                    }
                }
                else
                {
                    OD_LOG("! (Utilities::GetDetectedPortList(ports, true))"); //####
                    MpM_FAIL_("Could not get port list.");
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
