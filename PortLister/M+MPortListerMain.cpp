//--------------------------------------------------------------------------------------------------
//
//  File:       PortListerMain.cpp
//
//  Project:    M+M
//
//  Contains:   A utility application to list the available ports.
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
//  Created:    2014-03-28
//
//--------------------------------------------------------------------------------------------------

#include "M+MBaseClient.h"
#include "M+MRequests.h"
#include "M+MUtilities.h"

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
 
 @brief A utility application to list the available ports. */

/*! @dir PortLister
 @brief The set of files that implement the port lister application. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using std::cout;
using std::endl;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

/*! @brief Report the connections for a given port.
 @param flavour The format for the output.
 @param portName The port to be inspected.
 @param checker A function that provides for early exit from loops.
 @param checkStuff The private data for the early exit function. */
static void reportConnections(const MplusM::Common::OutputFlavour flavour,
                              const yarp::os::ConstString &       portName,
                              MplusM::Common::CheckFunction       checker,
                              void *                              checkStuff)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("portName = ", portName); //####
    OD_LOG_P1("checkStuff = ", checkStuff); //####
    bool                          sawInputs = false;
    bool                          sawOutputs = false;
    MplusM::Common::ChannelVector inputs;
    MplusM::Common::ChannelVector outputs;
    yarp::os::ConstString         inputsAsString;
    yarp::os::ConstString         outputsAsString;
    
    MplusM::Utilities::GatherPortConnections(portName, inputs, outputs,
                                             MplusM::Utilities::kInputAndOutputBoth, false, checker,
                                             checkStuff);
    for (MplusM::Common::ChannelVector::const_iterator walker(inputs.begin());
         inputs.end() != walker; ++walker)
    {
        switch (flavour)
        {
            case MplusM::Common::kOutputFlavourTabs :
                if (sawInputs)
                {
                    inputsAsString += ", ";
                }
                inputsAsString += MplusM::SanitizeString(walker->_portName, true);
                switch (walker->_portMode)
                {
                    case MplusM::Common::kChannelModeTCP :
                        inputsAsString += " TCP";
                        break;
                        
                    case MplusM::Common::kChannelModeUDP :
                        inputsAsString += " UDP";
                        break;
                        
                    default :
                        inputsAsString += " unknown";
                        break;
                        
                }
                break;
                
            case MplusM::Common::kOutputFlavourJSON :
                if (sawInputs)
                {
                    inputsAsString += ", ";
                }
                inputsAsString += T_("{ " CHAR_DOUBLEQUOTE "Port" CHAR_DOUBLEQUOTE ": "
                                     CHAR_DOUBLEQUOTE);
                inputsAsString += MplusM::SanitizeString(walker->_portName);
                inputsAsString += T_(CHAR_DOUBLEQUOTE ", " CHAR_DOUBLEQUOTE "Mode" CHAR_DOUBLEQUOTE
                                     ": " CHAR_DOUBLEQUOTE);
                switch (walker->_portMode)
                {
                    case MplusM::Common::kChannelModeTCP :
                        inputsAsString += "TCP";
                        break;
                        
                    case MplusM::Common::kChannelModeUDP :
                        inputsAsString += "UDP";
                        break;
                        
                    default :
                        inputsAsString += "unknown";
                        break;
                        
                }
                inputsAsString += T_(CHAR_DOUBLEQUOTE " }");
                break;
                
            default :
                inputsAsString += "   Input from ";
                inputsAsString += MplusM::SanitizeString(walker->_portName, true);
                switch (walker->_portMode)
                {
                    case MplusM::Common::kChannelModeTCP :
                        inputsAsString += " via TCP.";
                        break;
                        
                    case MplusM::Common::kChannelModeUDP :
                        inputsAsString += " via UDP.";
                        break;
                        
                    default :
                        inputsAsString += " via non-TCP/non-UDP.";
                        break;
                        
                }
                inputsAsString += "\n";
                break;
                
        }
        sawInputs = true;
    }
    for (MplusM::Common::ChannelVector::const_iterator walker(outputs.begin());
         outputs.end() != walker; ++walker)
    {
        switch (flavour)
        {
            case MplusM::Common::kOutputFlavourTabs :
                if (sawOutputs)
                {
                    outputsAsString += ", ";
                }
                outputsAsString += MplusM::SanitizeString(walker->_portName, true);
                switch (walker->_portMode)
                {
                    case MplusM::Common::kChannelModeTCP :
                        outputsAsString += " TCP";
                        break;
                        
                    case MplusM::Common::kChannelModeUDP :
                        outputsAsString += " UDP";
                        break;
                        
                    default :
                        outputsAsString += " unknown";
                        break;
                        
                }
                break;
                
            case MplusM::Common::kOutputFlavourJSON :
                if (sawOutputs)
                {
                    outputsAsString += ", ";
                }
                outputsAsString += T_("{ " CHAR_DOUBLEQUOTE "Port" CHAR_DOUBLEQUOTE ": "
                                      CHAR_DOUBLEQUOTE);
                outputsAsString += MplusM::SanitizeString(walker->_portName);
                outputsAsString += T_(CHAR_DOUBLEQUOTE ", " CHAR_DOUBLEQUOTE "Mode" CHAR_DOUBLEQUOTE
                                      ": " CHAR_DOUBLEQUOTE);
                switch (walker->_portMode)
                {
                    case MplusM::Common::kChannelModeTCP :
                        outputsAsString += "TCP";
                        break;
                        
                    case MplusM::Common::kChannelModeUDP :
                        outputsAsString += "UDP";
                        break;
                        
                    default :
                        outputsAsString += "unknown";
                        break;
                        
                }
                outputsAsString += T_(CHAR_DOUBLEQUOTE " }");
                break;
                
            default :
                outputsAsString += "   Output to ";
                outputsAsString += MplusM::SanitizeString(walker->_portName, true);
                switch (walker->_portMode)
                {
                    case MplusM::Common::kChannelModeTCP :
                        outputsAsString += " via TCP.";
                        break;
                        
                    case MplusM::Common::kChannelModeUDP :
                        outputsAsString += " via UDP.";
                        break;
                        
                    default :
                        outputsAsString += " via non-TCP/non-UDP.";
                        break;
                        
                }
                outputsAsString += "\n";
                break;
                
        }
        sawOutputs = true;
    }
    switch (flavour)
    {
	    case MplusM::Common::kOutputFlavourTabs :
            cout << inputsAsString.c_str() << "\t" << outputsAsString.c_str();
            break;
            
	    case MplusM::Common::kOutputFlavourJSON :
            cout << T_(CHAR_DOUBLEQUOTE "Inputs" CHAR_DOUBLEQUOTE ": [ ") <<
                    inputsAsString.c_str() << T_(" ], " CHAR_DOUBLEQUOTE "Outputs" CHAR_DOUBLEQUOTE
                                                 ": [ ") << outputsAsString.c_str() << " ]";
            break;
            
	    default :
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
            
    }
    OD_LOG_EXIT(); //####
} // reportConnections

/*! @brief Print out connection information for a port.
 @param flavour The format for the output.
 @param associates The associates of the port of interest. */
static void reportAssociates(const MplusM::Common::OutputFlavour        flavour,
                             const MplusM::Utilities::PortAssociation & associates)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P1("associates = ", &associates); //####
    if (associates._valid)
    {
        yarp::os::ConstString inputAssociates;
        yarp::os::ConstString outputAssociates;
        
        if (associates._primary)
        {
            bool sawInput = false;
            bool sawOutput = false;
            
            for (MplusM::Common::StringVector::const_iterator walker(associates._inputs.begin());
                 associates._inputs.end() != walker; ++walker)
            {
                if (sawInput)
                {
                    inputAssociates += ", ";
                }
                if (MplusM::Common::kOutputFlavourJSON == flavour)
                {
                    inputAssociates += CHAR_DOUBLEQUOTE;
                    inputAssociates += MplusM::SanitizeString(*walker);
                    inputAssociates += CHAR_DOUBLEQUOTE;
                }
                else
                {
                    inputAssociates += MplusM::SanitizeString(*walker, true);
                }
                sawInput = true;
            }
            for (MplusM::Common::StringVector::const_iterator walker(associates._outputs.begin());
                 associates._outputs.end() != walker; ++walker)
            {
                if (sawOutput)
                {
                    outputAssociates += ", ";
                }
                if (MplusM::Common::kOutputFlavourJSON == flavour)
                {
                    outputAssociates += CHAR_DOUBLEQUOTE;
                    outputAssociates += MplusM::SanitizeString(*walker);
                    outputAssociates += CHAR_DOUBLEQUOTE;
                }
                else
                {
                    outputAssociates += MplusM::SanitizeString(*walker, true);
                }
                sawOutput = true;
            }
            switch (flavour)
            {
                case MplusM::Common::kOutputFlavourTabs :
                    // Skip over the missing fields.
                    cout << "\tPrimary\t" << inputAssociates.c_str() << "\t" <<
                            outputAssociates.c_str();
                    break;
                    
                case MplusM::Common::kOutputFlavourJSON :
                    cout << T_(CHAR_DOUBLEQUOTE "Primary" CHAR_DOUBLEQUOTE ": true, "
                               CHAR_DOUBLEQUOTE "AssocInputs" CHAR_DOUBLEQUOTE ": [ ") <<
                            inputAssociates.c_str() << T_(" ], " CHAR_DOUBLEQUOTE "AssocOutputs"
                                                          CHAR_DOUBLEQUOTE ": [ ") <<
                            outputAssociates.c_str() << " ], ";
                    break;
                    
                default :
                    cout << " Primary port with inputs (" << inputAssociates.c_str() <<
                            ") and outputs (" <<
                    outputAssociates.c_str() << ").";
                    break;
                    
            }
        }
        else
        {
            inputAssociates = MplusM::SanitizeString(associates._inputs[0],
                                                     MplusM::Common::kOutputFlavourJSON != flavour);
            switch (flavour)
            {
                case MplusM::Common::kOutputFlavourTabs :
                    cout << "\tAssociate\t" << inputAssociates.c_str() << "\t";
                    break;
                    
                case MplusM::Common::kOutputFlavourJSON :
                    cout << T_(CHAR_DOUBLEQUOTE "Primary" CHAR_DOUBLEQUOTE ": false, "
                               CHAR_DOUBLEQUOTE "AssocInputs" CHAR_DOUBLEQUOTE ": [ "
                               CHAR_DOUBLEQUOTE) << inputAssociates.c_str() <<
                            T_(CHAR_DOUBLEQUOTE " ], " CHAR_DOUBLEQUOTE "AssocOutputs"
                               CHAR_DOUBLEQUOTE ": [ ], ");
                    break;
                    
                default :
                    cout << " Port associated with " << inputAssociates.c_str() << ".";
                    break;
                    
            }
        }
    }
    else
    {
        switch (flavour)
        {
            case MplusM::Common::kOutputFlavourTabs :
                // Skip over the missing fields.
                cout << "\t\t\t";
                break;
                
            case MplusM::Common::kOutputFlavourJSON :
                cout << T_(CHAR_DOUBLEQUOTE "Primary" CHAR_DOUBLEQUOTE ": " CHAR_DOUBLEQUOTE "null"
                           CHAR_DOUBLEQUOTE ", " CHAR_DOUBLEQUOTE "AssocInputs" CHAR_DOUBLEQUOTE
                           ": [ ], " CHAR_DOUBLEQUOTE "AssocOutputs" CHAR_DOUBLEQUOTE ": [ ], ");
                break;
                
            default :
                break;
                
        }
    }
    OD_LOG_EXIT(); //####
} // reportAssociates

/*! @brief Print out connection information for a port.
 @param flavour The format for the output.
 @param aDescriptor The attributes of the port of interest.
 @param checkWithRegistry @c true if the Service Registry is available for requests and @c false
 otherwise. */
static void reportPortStatus(const MplusM::Common::OutputFlavour       flavour,
                             const MplusM::Utilities::PortDescriptor & aDescriptor,
                             const bool                                checkWithRegistry)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P1("aDescriptor = ", &aDescriptor); //####
    OD_LOG_B1("checkWithRegistry = ", checkWithRegistry); //####
    MplusM::Utilities::PortAssociation associates;
    yarp::os::ConstString              portName;
    yarp::os::ConstString              portClass;
    
    portName = MplusM::SanitizeString(aDescriptor._portName,
                                      MplusM::Common::kOutputFlavourJSON != flavour);
    switch (flavour)
    {
	    case MplusM::Common::kOutputFlavourTabs :
            cout << portName.c_str() << "\t";
            break;
            
	    case MplusM::Common::kOutputFlavourJSON :
            cout << T_("{ " CHAR_DOUBLEQUOTE "PortName" CHAR_DOUBLEQUOTE ": " CHAR_DOUBLEQUOTE) <<
                    portName.c_str() << T_(CHAR_DOUBLEQUOTE ", ");
            break;
            
	    default :
            cout << portName.c_str() << ": ";
            break;
            
    }
    if (checkWithRegistry)
    {
        yarp::os::ConstString request(MpM_REQREP_DICT_CHANNELNAME_KEY ":");
        
        request += aDescriptor._portName;
        yarp::os::Bottle matches(MplusM::Common::FindMatchingServices(request, true, NULL, NULL));
        
        OD_LOG_S1s("matches <- ", matches.toString()); //####
        if (MpM_EXPECTED_MATCH_RESPONSE_SIZE == matches.size())
        {
            yarp::os::ConstString matchesFirstString(matches.get(0).toString());
            
            if (strcmp(MpM_OK_RESPONSE, matchesFirstString.c_str()))
            {
                // Didn't match - use a simpler check, in case it's unregistered or is an adapter or
                // client.
                switch (MplusM::Utilities::GetPortKind(aDescriptor._portName))
                {
                    case MplusM::Utilities::kPortKindAdapter :
                        portClass = "Adapter port";
                        break;
                        
                    case MplusM::Utilities::kPortKindClient :
                        portClass = "Client port";
                        break;
                        
                    case MplusM::Utilities::kPortKindService :
                        portClass = "Unregistered service port";
                        break;
                        
                    case MplusM::Utilities::kPortKindServiceRegistry :
                        portClass = "Service Registry port";
                        break;
                        
                    case MplusM::Utilities::kPortKindStandard :
                        portClass = "Standard port at ";
                        portClass += aDescriptor._portIpAddress;
                        portClass += ":";
                        portClass += aDescriptor._portPortNumber;
                        break;
                        
                }
            }
            else
            {
                yarp::os::Value secondValue(matches.get(1));
                
                if (secondValue.isList())
                {
                    yarp::os::Bottle * secondList = secondValue.asList();
                    
                    if (secondList && secondList->size())
                    {
                        yarp::os::ConstString serviceName(matches.get(1).toString());
                        
                        if (aDescriptor._portName == MpM_REGISTRY_CHANNEL_NAME)
                        {
                            portClass = "Service registry port for '";
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
                        // Didn't match - use a simpler check, in case it's unregistered or is an
                        // adapter or client.
                        switch (MplusM::Utilities::GetPortKind(aDescriptor._portName))
                        {
                            case MplusM::Utilities::kPortKindAdapter :
                                portClass = "Adapter port";
                                break;
                                
                            case MplusM::Utilities::kPortKindClient :
                                portClass = "Client port";
                                break;
                                
                            case MplusM::Utilities::kPortKindService :
                                portClass = "Unregistered service port";
                                break;
                                
                            case MplusM::Utilities::kPortKindServiceRegistry :
                                portClass = "Service Registry port";
                                break;
                                
                            case MplusM::Utilities::kPortKindStandard :
                                portClass = "Standard port at ";
                                portClass += aDescriptor._portIpAddress;
                                portClass += ":";
                                portClass += aDescriptor._portPortNumber;
                                break;
                                
                        }
                    }
                }
            }
        }
        switch (flavour)
        {
            case MplusM::Common::kOutputFlavourTabs :
                cout << MplusM::SanitizeString(portClass, true).c_str() << "\t";
                break;
                
            case MplusM::Common::kOutputFlavourJSON :
                cout << T_(CHAR_DOUBLEQUOTE "PortClass" CHAR_DOUBLEQUOTE ": " CHAR_DOUBLEQUOTE) <<
                        MplusM::SanitizeString(portClass).c_str() << T_(CHAR_DOUBLEQUOTE ", ");
                break;
                
            default :
                cout << MplusM::SanitizeString(portClass, true).c_str() << ".";
                break;
                
        }
        MplusM::Utilities::GetAssociatedPorts(aDescriptor._portName, associates, STANDARD_WAIT_TIME,
                                              true, NULL, NULL);
    }
    else
    {
        // We can't interrogate the service registry, so use a simple heuristic to identify clients,
        // services and adapters.
        // Didn't match - use a simpler check, in case it's unregistered or is an adapter or client.
        switch (MplusM::Utilities::GetPortKind(aDescriptor._portName))
        {
            case MplusM::Utilities::kPortKindAdapter :
                portClass = "Adapter port";
                break;
                
            case MplusM::Utilities::kPortKindClient :
                portClass = "Client port";
                break;
                
            case MplusM::Utilities::kPortKindService :
                portClass = "Unregistered service port";
                break;
                
            case MplusM::Utilities::kPortKindServiceRegistry :
                portClass = "Service Registry port";
                break;
                
            case MplusM::Utilities::kPortKindStandard :
                portClass = "Standard port at ";
                portClass += aDescriptor._portIpAddress;
                portClass += ":";
                portClass += aDescriptor._portPortNumber;
                break;
                
        }
        switch (flavour)
        {
            case MplusM::Common::kOutputFlavourTabs :
                cout << MplusM::SanitizeString(portClass, true).c_str() << "\t";
                break;
                
            case MplusM::Common::kOutputFlavourJSON :
                cout << T_(CHAR_DOUBLEQUOTE "PortClass" CHAR_DOUBLEQUOTE ": " CHAR_DOUBLEQUOTE) <<
                        MplusM::SanitizeString(portClass).c_str() << T_(CHAR_DOUBLEQUOTE ", ");
                break;
                
            default :
                cout << MplusM::SanitizeString(portClass, true).c_str() << ".";
                break;
                
        }
    }
    reportAssociates(flavour, associates);
    switch (flavour)
    {
	    case MplusM::Common::kOutputFlavourTabs :
            break;
            
	    case MplusM::Common::kOutputFlavourJSON :
            break;
            
	    default :
            cout << endl;
            break;
            
    }
    reportConnections(flavour, aDescriptor._portName, NULL, NULL);
    switch (flavour)
    {
	    case MplusM::Common::kOutputFlavourTabs :
            break;
            
	    case MplusM::Common::kOutputFlavourJSON :
            cout << " }";
            break;
            
	    default :
            break;
            
    }
    OD_LOG_EXIT(); //####
} // reportPortStatus

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

/*! @brief The entry point for listing the connection status of all visible YARP ports.
 
 There is no input and the output consists of a list of ports and what, if anything, is connected to
 them.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the example client.
 @returns @c 0 on a successful test and @c 1 on failure. */
int main(int     argc,
         char ** argv)
{
    OD_LOG_INIT(*argv, kODLoggingOptionIncludeProcessID | kODLoggingOptionIncludeThreadID | //####
                kODLoggingOptionEnableThreadSupport | kODLoggingOptionWriteToStderr); //####
    OD_LOG_ENTER(); //####
#if MAC_OR_LINUX_
    MplusM::Common::SetUpLogger(*argv);
#endif // MAC_OR_LINUX_
    MplusM::Common::OutputFlavour flavour = MplusM::Common::kOutputFlavourNormal;

	opterr = 0; // Suppress the error message resulting from an unknown option.
    for (int cc = getopt(argc, argv, STANDARD_OPTIONS); -1 != cc;
         cc = getopt(argc, argv, STANDARD_OPTIONS))
    {
        switch (cc)
        {
            case 'j' :
                flavour = MplusM::Common::kOutputFlavourJSON;
                break;
                
            case 't' :
                flavour = MplusM::Common::kOutputFlavourTabs;
                break;
                
            default :
                // Ignore unknown options.
                break;
                
        }
    }
    try
    {
#if CheckNetworkWorks_
        if (yarp::os::Network::checkNetwork())
#endif // CheckNetworkWorks_
        {
            yarp::os::Network yarp; // This is necessary to establish any connections to the YARP
                                    // infrastructure
            
            MplusM::Common::Initialize(*argv);
            bool                          found = false;
            MplusM::Utilities::PortVector ports;
            
            MplusM::Utilities::GetDetectedPortList(ports, true);
            bool serviceRegistryPresent = MplusM::Utilities::CheckForRegistryService(ports);
            if (MplusM::Common::kOutputFlavourJSON == flavour)
            {
                cout << "[ ";
            }
            for (MplusM::Utilities::PortVector::const_iterator walker(ports.begin());
                 ports.end() != walker; ++walker)
            {
                switch (flavour)
                {
                    case MplusM::Common::kOutputFlavourJSON :
                        if (found)
                        {
                            cout << "," << endl;
                        }
                        break;
                        
                    case MplusM::Common::kOutputFlavourTabs :
                        if (found)
                        {
                            cout << endl;
                        }
                        break;
                        
                    default :
                        if (! found)
                        {
                            cout << "Ports:" << endl << endl;
                        }
                        break;
                        
                }
                found = true;
                reportPortStatus(flavour, *walker, serviceRegistryPresent);
            }
            switch (flavour)
            {
                case MplusM::Common::kOutputFlavourTabs :
                    if (found)
                    {
                        cout << endl;
                    }
                    break;
                    
                case MplusM::Common::kOutputFlavourJSON :
                    cout << " ]" << endl;
                    break;
                    
                default :
                    if (found)
                    {
                        cout << endl;
                    }
                    else
                    {
                        cout << "No ports found." << endl;
                    }
                    break;
                    
            }
        }
#if CheckNetworkWorks_
        else
        {
            OD_LOG("! (yarp::os::Network::checkNetwork())"); //####
# if MAC_OR_LINUX_
            MplusM::Common::GetLogger().fail("YARP network not running.");
# endif // MAC_OR_LINUX_
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
