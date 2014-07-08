//--------------------------------------------------------------------------------------------------
//
//  File:       M+MUtilities.h
//
//  Project:    M+M
//
//  Contains:   The function and variable declarations for utilities for M+M clients and services.
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
//  Created:    2014-05-16
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMUtilities_H_))
# define MpMUtilities_H_  /* Header guard */

# include "M+MCommon.h"

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 
 @brief The function and variable declarations for utilities for M+M clients and services. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace Utilities
    {
        /*! @brief Which combination of input and output to use. */
        enum InputOutputFlag
        {
            /*! @brief Neither input nor output. */
            kInputAndOutputNone   = 0x0,
            
            /*! @brief Input. */
            kInputAndOutputInput  = 0x1,
            
            /*! @brief Output. */
            kInputAndOutputOutput = 0x2,
            
            /*! @brief Both input and output. */
            kInputAndOutputBoth   = 0x3
            
        }; // InputOutputFlag
        
        /*! @brief The kinds of ports. */
        enum PortKind
        {
            /*! @brief The port is an adapter port. */
            kPortKindAdapter,
            
            /*! @brief The port is a client port. */
            kPortKindClient,
            
            /*! @brief The port is a service port. */
            kPortKindService,
            
            /*! @brief The port is the service registry port. */
            kPortKindServiceRegistry,
            
            /*! @brief The port is s standard port. */
            kPortKindStandard
            
        }; // PortKind
        
        /*! @brief The associates of a port. */
        struct PortAssociation
        {
            /*! @brief The collected inputs associated with the port. */
            Common::StringVector _inputs;
            
            /*! @brief The collected outputs associated with the port. */
            Common::StringVector _outputs;
            
            /*! @brief @c true if the port is associated and @c false if it is an associate, in
             which case the first input port is the primary for the association. */
            bool _primary;
            
            /*! @brief @c true if the association data is valid and @c false otherwise. */
            bool _valid;
            
        }; // PortAssociation
        
        /*! @brief The attributes of a port. */
        struct PortDescriptor
        {
            /*! @brief The registered name of the port. */
            yarp::os::ConstString _portName;
            
            /*! @brief The IP address for the port. */
            yarp::os::ConstString _portIpAddress;
            
            /*! @brief The IP port number for the port. */
            yarp::os::ConstString _portPortNumber;
            
        }; // PortDescriptor
        
        /*! @brief The attributes of a service. */
        struct ServiceDescriptor
        {
            /*! @brief The standard name for the service. */
            yarp::os::ConstString _canonicalName;
            
            /*! @brief The standard channel for the service. */
            yarp::os::ConstString _channelName;
            
            /*! @brief The description of the service. */
            yarp::os::ConstString _description;
            
            /*! @brief The set of secondary input channels for the service. */
            Common::ChannelVector _inputChannels;
            
            /*! @brief The description of the behavioural model for the service. */
            yarp::os::ConstString _kind;
            
            /*! @brief The set of secondary output channels for the service. */
            Common::ChannelVector _outputChannels;
            
            /*! @brief The name of the input channel for the service. */
            yarp::os::ConstString _path;
            
            /*! @brief The description of the requests for the service. */
            yarp::os::ConstString _requestsDescription;
            
        }; // ServiceDescriptor
        
        /*! @brief A set of port descriptions. */
        typedef std::vector<PortDescriptor> PortVector;
        
        /*! @brief Add a connection between two ports.
         @param fromPortName The name of the source port.
         @param toPortName The name of the destination port.
         @param timeToWait The number of seconds allowed before a failure is considered.
         @param isUDP @c true if the connection is to be UDP and @c false otherwise.
         @returns @c true if successful and @c false otherwise. */
        bool AddConnection(const yarp::os::ConstString & fromPortName,
                           const yarp::os::ConstString & toPortName,
                           const double                  timeToWait,
                           const bool                    isUDP);
        
        /*! @brief Check if the Registry Service is active.
         @param ports The set of detected ports.
         @returns @c true if the Registry Service port is present and @c false otherwise. */
        bool CheckForRegistryService(const PortVector & ports);
        
        /*! @brief Collect the input and output connections for a port.
         @param portName The port to be inspected.
         @param inputs The collected inputs for the port.
         @param outputs The collected outputs for the port.
         @param which A flag to specify what is to be gathered.
         @param quiet @c true if status output is to be suppressed and @c false otherwise. */
        void GatherPortConnections(const yarp::os::ConstString & portName,
                                   Common::ChannelVector &       inputs,
                                   Common::ChannelVector &       outputs,
                                   const InputOutputFlag         which,
                                   const bool                    quiet = false);
        
        /*! @brief Collect the associated input and output connections for a port.
         @param portName The port to be inspected.
         @param associates The associated ports for the port.
         @param timeToWait The number of seconds allowed before a failure is considered.
         @param quiet @c true if status output is to be suppressed and @c false otherwise.
         @returns @c true if there is association data for the port and @c false otherwise. */
        bool GetAssociatedPorts(const yarp::os::ConstString & portName,
                                PortAssociation &             associates,
                                const double                  timeToWait,
                                const bool                    quiet = false);
        
        /*! @brief Get the set of detected ports.
         @param ports The set of detected ports.
         @param includeHiddenPorts @c true if all ports are returned and @c false is 'hidden' ports
         are ignored. */
        void GetDetectedPortList(PortVector & ports,
                                 const bool   includeHiddenPorts = false);
        
        /*! @brief Retrieve the details for a service.
         @param serviceChannelName The channel for the service.
         @param descriptor The details for a service.
         @param timeToWait The number of seconds allowed before a failure is considered.
         @returns @c true if the service returned the desired information and @c false otherwise. */
        bool GetNameAndDescriptionForService(const yarp::os::ConstString & serviceChannelName,
                                             ServiceDescriptor &           descriptor,
                                             const double                  timeToWait);
        
        /*! @brief Map a port name to the port kind.
         @param portName The name of the port.
         @returns The kind of the port. */
        PortKind GetPortKind(const yarp::os::ConstString & portName);
        
        /*! @brief Retrieve the set of known services.
         @param services The set of registered services.
         @param quiet @c true if status output is to be suppressed and @c false otherwise. */
        void GetServiceNames(Common::StringVector & services,
                             const bool             quiet = false);
        
        /*! @brief Return a string representation of a service kind.
         @param kind The value to be converted.
         @returns A string representation of the service kind. */
        const char * MapServiceKindToString(const Common::ServiceKind kind);
        
        /*! @brief Return the service kind corresponding to a string.
         @param kindString The value to be converted.
         @returns The service kind corresponding to a string. */
        Common::ServiceKind MapStringToServiceKind(const yarp::os::ConstString & kindString);
        
        /*! @brief Remove a connection between two ports.
         @param fromPortName The name of the source port.
         @param toPortName The name of the destination port. */
        bool RemoveConnection(const yarp::os::ConstString & fromPortName,
                              const yarp::os::ConstString & toPortName);
        
    } // Utilities
    
} // MplusM

#endif // ! defined(MpMUtilities_H_)
