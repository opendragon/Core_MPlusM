//--------------------------------------------------------------------------------------------------
//
//  File:       m+mEntitiesData.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the data collected by the background scanner.
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

#include "m+mEntitiesData.hpp"

#include "m+mEntityData.hpp"

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file

 @brief The class definition for the data collected by the background scanner. */
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

EntitiesData::EntitiesData(void)
{
    ODL_ENTER(); //####
    ODL_EXIT_P(this); //####
} // EntitiesData::EntitiesData

EntitiesData::~EntitiesData(void)
{
    ODL_OBJENTER(); //####
    clearOutData();
    ODL_OBJEXIT(); //####
} // EntitiesData::~EntitiesData

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void
EntitiesData::addConnection(const YarpString &  inName,
                            const YarpString &  outName,
                            Common::ChannelMode mode)
{
    ODL_OBJENTER(); //####
    ODL_S2s("inName = ", inName, "outName = ", outName); //####
    ConnectionDetails details;

    details._inPortName = inName;
    details._outPortName = outName;
    details._mode = mode;
    _connections.push_back(details);
    ODL_OBJEXIT(); //####
} // EntitiesData::addConnection

void
EntitiesData::addEntity(EntityData * anEntity)
{
    ODL_OBJENTER(); //####
    ODL_P1("anEntity = ", anEntity); //####
    _entities.push_back(anEntity);
    ODL_OBJEXIT(); //####
} // EntitiesData::addEntity

void
EntitiesData::clearConnections(void)
{
    ODL_OBJENTER(); //####
    _connections.clear();
    ODL_OBJEXIT(); //####
} // EntitiesData::clearConnections

void
EntitiesData::clearOutData(void)
{
    ODL_OBJENTER(); //####
    clearConnections();
    for (EntitiesList::const_iterator it(_entities.begin()); _entities.end() != it; ++it)
    {
        EntityData * anEntity = *it;

        if (anEntity)
        {
            delete anEntity;
        }
    }
    _entities.clear();
    ODL_OBJEXIT(); //####
} // EntitiesData::clearOutData

EntityData *
EntitiesData::getEntity(const size_t index)
const
{
    ODL_OBJENTER(); //####
    ODL_LL1("index = ", index); //####
    EntityData * result;

    if (_entities.size() > index)
    {
        result = _entities.at(index);
    }
    else
    {
        result = NULL;
    }
    ODL_OBJEXIT_P(result); //####
    return result;
} // EntitiesData::getEntity

size_t
EntitiesData::getNumberOfEntities(void)
const
{
    ODL_OBJENTER(); //####
    size_t result = _entities.size();

    ODL_OBJEXIT_LL(result); //####
    return result;
} // EntitiesData::getNumberOfEntities

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
