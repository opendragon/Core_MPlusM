//--------------------------------------------------------------------------------------------------
//
//  File:       m+mEntityData.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for an entity detected by the background scanner.
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

#include "m+mEntityData.hpp"

#include "m+mPortData.hpp"

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file

 @brief The class definition for an entity detected by the background scanner. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MPlusM_Manager;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

EntityData::EntityData(const ContainerKind kind,
                       const YarpString &  name,
                       const YarpString &  behaviour,
                       const YarpString &  description,
                       const YarpString &  extraInfo,
                       const YarpString &  requests) :
    _behaviour(behaviour), _description(description), _extraInfo(extraInfo), _IPAddress(),
    _name(name), _requests(requests), _kind(kind)
{
    ODL_ENTER(); //####
    ODL_S4s("name = ", name, "behaviour = ", behaviour, "description = ", description, //####
            "extraInfo = ", extraInfo); //####
    ODL_S1s("requests = ", requests); //####
    ODL_EXIT_P(this); //####
} // EntityData::EntityData

EntityData::~EntityData(void)
{
    ODL_OBJENTER(); //####
    ODL_S1s("getName() = ", getName()); //####
    for (Ports::iterator walker(_ports.begin()); _ports.end() != walker; ++walker)
    {
        PortData * aPort = *walker;

        if (aPort)
        {
            delete aPort;
        }
    }
    _ports.clear();
    for (size_t ii = 0, mm = _argumentList.size(); mm > ii; ++ii)
    {
        Utilities::BaseArgumentDescriptor * argDesc = _argumentList[ii];

        delete argDesc;
    }
    _argumentList.clear();
    ODL_OBJEXIT(); //####
} // EntityData::~EntityData

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void
EntityData::addArgumentDescription(MplusM::Utilities::BaseArgumentDescriptor * argDesc)
{
    ODL_OBJENTER(); //####
    ODL_P1("argDesc = ", argDesc); //####
    _argumentList.push_back(argDesc->clone());
    ODL_OBJEXIT(); //####
} // EntityData::addArgumentDescription

PortData *
EntityData::addPort(const YarpString &  portName,
                    const YarpString &  portProtocol,
                    const YarpString &  protocolDescription,
                    const PortUsage     portKind,
                    const PortDirection direction)
{
    ODL_OBJENTER(); //####
    ODL_S3s("portName = ", portName, "portProtocol = ", portProtocol, //####
            "protocolDescription = ", protocolDescription); //####
    PortData * aPort = new PortData(portName, portProtocol, protocolDescription, portKind,
                                    direction);

    _ports.push_back(aPort);
    ODL_OBJEXIT_P(aPort); //####
    return aPort;
} // EntityData::addPort

MplusM::Utilities::BaseArgumentDescriptor *
EntityData::getArgumentDescriptor(const size_t idx)
const
{
    ODL_ENTER(); //####
    ODL_LL1(idx, idx); //####
    MplusM::Utilities::BaseArgumentDescriptor * result;

    if (_argumentList.size() > idx)
    {
        result = _argumentList[idx];
    }
    else
    {
        result = NULL;
    }
    ODL_EXIT_P(result); //####
    return result;
} // EntityData::getArgumentDescriptor

int
EntityData::getNumPorts(void)
const
{
    ODL_OBJENTER(); //####
    int result = static_cast<int>(_ports.size());

    ODL_OBJEXIT_L(result); //####
    return result;
} // EntityData::getNumPorts

PortData *
EntityData::getPort(const int num)
const
{
    ODL_OBJENTER(); //####
    PortData * result;

    if ((0 <= num) && (static_cast<int>(_ports.size()) > num))
    {
        result = _ports.at(static_cast<size_t>(num));
    }
    else
    {
        result = NULL;
    }
    ODL_OBJEXIT_P(result);
    return result;
} // EntityData::getPort

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
