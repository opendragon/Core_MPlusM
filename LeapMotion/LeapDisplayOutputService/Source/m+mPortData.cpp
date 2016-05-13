//--------------------------------------------------------------------------------------------------
//
//  File:       m+mPortData.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for a port detected by the background scanner.
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

#include "m+mPortData.hpp"

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file

 @brief The class definition for a port detected by the background scanner. */
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

PortData::PortData(const YarpString &  portName,
                   const YarpString &  portProtocol,
                   const YarpString &  protocolDescription,
                   const PortUsage     portKind,
                   const PortDirection direction) :
    _portName(portName), _portPortNumber(), _portProtocol(portProtocol),
    _protocolDescription(protocolDescription), _direction(direction), _usage(portKind)
{
    ODL_ENTER(); //####
    ODL_S3s("portName = ", portName, "portProtocol = ", portProtocol, //####
            "protocolDescription = ", protocolDescription); //####
    ODL_LL2("portKind = ", portKind, "direction = ", direction); //####
    ODL_EXIT_P(this); //####
} // PortData::PortData

PortData::~PortData(void)
{
    ODL_OBJENTER(); //####
    ODL_S1s("getPortName() = ", getPortName()); //####
    ODL_OBJEXIT(); //####
} // PortData::~PortData

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void
PortData::addInputConnection(PortData *          other,
                             Common::ChannelMode mode)
{
    ODL_OBJENTER(); //####
    ODL_P1("other = ", other); //####
    if (other)
    {
        bool canAdd = true;

        for (PortConnections::iterator walker(_inputConnections.begin());
             _inputConnections.end() != walker; ++walker)
        {
            PortInfo * candidate(&*walker);

            if (candidate)
            {
                if ((candidate->_otherPort == other) ||
                    (candidate->_otherPort->getPortName() == other->getPortName()))
                {
                    ODL_LOG("already present"); //####
                    candidate->_valid = true;
                    canAdd = false;
                    break;
                }

            }
        }
        if (canAdd)
        {
            PortInfo newConnection;

            newConnection._otherPort = other;
            newConnection._connectionMode = mode;
            newConnection._valid = true;
            _inputConnections.push_back(newConnection);
        }
    }
    ODL_OBJEXIT(); //####
} // PortData::addInputConnection

void
PortData::addOutputConnection(PortData *          other,
                              Common::ChannelMode mode)
{
    ODL_OBJENTER(); //####
    ODL_P1("other = ", other); //####
    if (other)
    {
        bool canAdd = true;

        for (PortConnections::iterator walker(_outputConnections.begin());
             _outputConnections.end() != walker; ++walker)
        {
            PortInfo * candidate(&*walker);

            if (candidate)
            {
                if ((candidate->_otherPort == other) ||
                    (candidate->_otherPort->getPortName() == other->getPortName()))
                {
                    ODL_LOG("already present"); //####
                    candidate->_valid = true;
                    canAdd = false;
                    break;
                }

            }
        }
        if (canAdd)
        {
            PortInfo newConnection;

            newConnection._otherPort = other;
            newConnection._connectionMode = mode;
            newConnection._valid = true;
            _outputConnections.push_back(newConnection);
        }
    }
    ODL_OBJEXIT(); //####
} // PortData::addOutputConnection

bool
PortData::hasOutgoingConnectionTo(const YarpString & otherPort)
const
{
    ODL_OBJENTER(); //####
    ODL_S1s("otherPort = ", otherPort); //####
    bool result = false;

    for (PortConnections::const_iterator walker(_outputConnections.begin());
         _outputConnections.end() != walker; ++walker)
    {
        const PortInfo * candidate(&*walker);

        if (candidate && candidate->_otherPort &&
            (candidate->_otherPort->getPortName() == otherPort))
        {
            result = true;
            break;
        }

    }
    ODL_OBJEXIT_B(result); //####
    return result;
} // PortData::hasOutgoingConnectionTo

void
PortData::invalidateConnections(void)
{
    ODL_OBJENTER(); //####
    for (PortConnections::iterator walker(_inputConnections.begin());
         _inputConnections.end() != walker; ++walker)
    {
        PortInfo * candidate(&*walker);

        if (candidate)
        {
            candidate->_valid = false;
        }
    }
    for (PortConnections::iterator walker(_outputConnections.begin());
         _outputConnections.end() != walker; ++walker)
    {
        PortInfo * candidate(&*walker);

        if (candidate)
        {
            candidate->_valid = false;
        }
    }
    ODL_OBJEXIT(); //####
} // PortData::invalidateConnections

void
PortData::removeInputConnection(PortData * other)
{
    ODL_OBJENTER(); //####
    ODL_P1("other = ", other); //####
    if (other)
    {
        PortConnections::iterator walker(_inputConnections.begin());

        for ( ; _inputConnections.end() != walker; ++walker)
        {
            PortInfo * candidate(&*walker);

            if (candidate && (candidate->_otherPort == other))
            {
                break;
            }

        }
        if (_inputConnections.end() != walker)
        {
            _inputConnections.erase(walker);
        }
    }
    ODL_OBJEXIT(); //####
} // PortData::removeInputConnection

void
PortData::removeInvalidConnections(void)
{
    ODL_OBJENTER(); //####
    bool keepGoing;

    do
    {
        keepGoing = false;
        PortConnections::iterator walker(_inputConnections.begin());

        for ( ; _inputConnections.end() != walker; ++walker)
        {
            PortInfo * candidate(&*walker);

            if (candidate && (! candidate->_valid))
            {
                break;
            }

        }
        if (_inputConnections.end() != walker)
        {
            _inputConnections.erase(walker);
            keepGoing = true;
        }
    }
    while (keepGoing);
    do
    {
        keepGoing = false;
        PortConnections::iterator walker(_outputConnections.begin());

        for ( ; _outputConnections.end() != walker; ++walker)
        {
            PortInfo * candidate(&*walker);

            if (candidate && (! candidate->_valid))
            {
                break;
            }

        }
        if (_outputConnections.end() != walker)
        {
            _outputConnections.erase(walker);
            keepGoing = true;
        }
    }
    while (keepGoing);
    ODL_OBJEXIT(); //####
} // PortData::removeInvalidConnections

void
PortData::removeOutputConnection(PortData * other)
{
    ODL_OBJENTER(); //####
    ODL_P1("other = ", other); //####
    if (other)
    {
        PortConnections::iterator walker(_outputConnections.begin());

        for ( ; _outputConnections.end() != walker; ++walker)
        {
            const PortInfo * candidate(&*walker);

            if (candidate && (candidate->_otherPort == other))
            {
                break;
            }

        }
        if (_outputConnections.end() != walker)
        {
            _outputConnections.erase(walker);
        }
    }
    ODL_OBJEXIT(); //####
} // PortData::removeOutputConnection

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
