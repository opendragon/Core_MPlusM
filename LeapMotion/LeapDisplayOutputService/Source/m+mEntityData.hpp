//--------------------------------------------------------------------------------------------------
//
//  File:       m+mEntityData.hpp
//
//  Project:    m+m
//
//  Contains:   The class declaration for an entity detected by the background scanner.
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
//  Created:    2014-08-06
//
//--------------------------------------------------------------------------------------------------

#if (! defined(mpmEntityData_HPP_))
# define mpmEntityData_HPP_ /* Header guard */

# include "m+mManagerDataTypes.hpp"

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file

 @brief The class declaration for an entity detected by the background scanner. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MPlusM_Manager
{
    /*! @brief An entity detected by the background scanner. */
    class EntityData
    {
    public :

    protected :

    private :

    public :

        /*! @brief The constructor.
         @param kind The kind of entity.
         @param name The name of the entity.
         @param behaviour The behavioural model if a service or adapter.
         @param description The description, if this is a service or adapter.
         @param extraInfo The extra information for the entity.
         @param requests The requests supported, if this is a service or adapter. */
        EntityData(const ContainerKind kind,
                   const YarpString &  name,
                   const YarpString &  behaviour,
                   const YarpString &  description,
                   const YarpString &  extraInfo,
                   const YarpString &  requests);

        /*! @brief The destructor. */
        virtual
        ~EntityData(void);

        /*! @brief Add an argument description to the entity.
         @param argDesc The argument descriptor to be added to the entity. */
        void
        addArgumentDescription(MplusM::Utilities::BaseArgumentDescriptor * argDesc);

        /*! @brief Add a port to the entity.
         @param portName The name of the port.
         @param portProtocol The protocol of the port.
         @param protocolDescription The description of the protocol.
         @param portKind What the port will be used for.
         @param direction The primary direction of the port.
         @returns The newly-created port. */
        PortData *
        addPort(const YarpString &  portName,
                const YarpString &  portProtocol = "",
                const YarpString &  protocolDescription = "",
                const PortUsage     portKind = kPortUsageOther,
                const PortDirection direction = kPortDirectionInputOutput);

        /*! @brief Return a particular argument descriptor.
         @param idx The index of the argument of interest.
         @returns The argument descriptor at the specified index. */
        MplusM::Utilities::BaseArgumentDescriptor *
        getArgumentDescriptor(const size_t idx)
        const;

        /*! @brief Return the behavioural model for the entity.
         @returns The behavioural model for the entity. */
        inline const YarpString &
        getBehaviour(void)
        const
        {
            return _behaviour;
        } // getBehaviour

        /*! @brief Return the description of the entity.
         @returns The description of the entity. */
        inline const YarpString &
        getDescription(void)
        const
        {
            return _description;
        } // getDescription

        /*! @brief Return the extra information for the entity.
         @returns The extra information for the entity. */
        inline const YarpString &
        getExtraInformation(void)
        const
        {
            return _extraInfo;
        } // getExtraInformation

        /*! @brief Return the kind of container.
         @returns The kind of container. */
        inline ContainerKind
        getKind(void)
        const
        {
            return _kind;
        } // getKind

        /*! @brief Return the IP address of the entity.
         @returns The IP address of the entity. */
        inline const YarpString &
        getIPAddress(void)
        const
        {
            return _IPAddress;
        } // getIPAddress

        /*! @brief Return the description of the entity.
         @returns The description of the entity. */
        inline const YarpString &
        getName(void)
        const
        {
            return _name;
        } // getName

        /*! @brief Returns the number of argument descriptions in this container.
         @returns The number of argument descriptions in this container. */
        inline size_t
        getNumArgumentDescriptors(void)
        const
        {
            return _argumentList.size();
        } // getNumArgumentDescriptors

        /*! @brief Returns the number of ports in this panel.
         @returns The number of ports in this panel. */
        int
        getNumPorts(void)
        const;

        /*! @brief Returns a port by index.
         @param num The zero-origin index of the port.
         @returns A port or @c NULL if the index is out of range. */
        PortData *
        getPort(const int num)
        const;

        /*! @brief Return the requests supported by the entity.
         @returns The requests supported by the entity. */
        inline const YarpString &
        getRequests(void)
        const
        {
            return _requests;
        } // getRequests

        /*! @brief Set the IP address of the entity.
         @param newAddress The IP address of the entity. */
        inline void
        setIPAddress(const YarpString & newAddress)
        {
            _IPAddress = newAddress;
        } // setIPAddress

    protected :

    private :

        /*! @brief The copy constructor.
         @param other The object to be copied. */
        EntityData(const EntityData & other);

        /*! @brief The assignment operator.
         @param other The object to be copied.
         @returns The updated object. */
        EntityData &
        operator =(const EntityData & other);

    public :

    protected :

    private :

        /*! @brief The collection of ports for the entity. */
        Ports _ports;

        /*! @brief The argument descriptions if it is a service or an adapter. */
        MplusM::Utilities::DescriptorVector _argumentList;

        /*! @brief The behavioural model if a service or an adapter. */
        YarpString _behaviour;

        /*! @brief The description of the entity, if it is a service or an adapter. */
        YarpString _description;

        /*! @brief The extra information for the entity, if it is a service or an adapter. */
        YarpString _extraInfo;

        /*! @brief The IP address of the primary channel. */
        YarpString _IPAddress;

        /*! @brief The name of the entity. */
        YarpString _name;

        /*! @brief The requests for the entity, if it is a service or an adapter. */
        YarpString _requests;

        /*! @brief The kind of entity. */
        ContainerKind _kind;

    }; // EntityData

} // MPlusM_Manager

#endif // ! defined(mpmEntityData_HPP_)
