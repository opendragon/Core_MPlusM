//--------------------------------------------------------------------------------------------------
//
//  File:       m+mEntitiesData.hpp
//
//  Project:    m+m
//
//  Contains:   The class declaration for the data collected by the background scanner.
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

#if (! defined(mpmEntitiesData_HPP_))
# define mpmEntitiesData_HPP_ /* Header guard */

# include "m+mManagerDataTypes.hpp"

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file

 @brief The class declaration for the data collected by the background scanner. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MPlusM_Manager
{
    /*! @brief The data collected by the background scanner. */
    class EntitiesData
    {
    public :

    protected :

    private :

    public :

        /*! @brief The constructor. */
        EntitiesData(void);

        /*! @brief The destructor. */
        virtual
        ~EntitiesData(void);

        /*! @brief Record a connection between ports.
         @param inName The name of the destination port.
         @param outName The name of the source port.
         @param mode The mode of the connection. */
        void
        addConnection(const YarpString &          inName,
                      const YarpString &          outName,
                      MplusM::Common::ChannelMode mode);

        /*! @brief Add an entity to the list of known entities.
         @param anEntity The entity to be added. */
        void
        addEntity(EntityData * anEntity);

        /*! @brief Clear out connection information. */
        void
        clearConnections(void);

        /*! @brief Release all data held by the panel. */
        void
        clearOutData(void);

        /*! @brief Return the list of detected connections.
         @returns The list of detected connections. */
        inline const ConnectionList &
        getConnections(void)
        const
        {
            return _connections;
        } // getConnections

        /*! @brief Return an entity by index.
         @param index The zero-origin index of the entity.
         @returns The entity if the index is within range and @c NULL otherwise. */
        EntityData *
        getEntity(const size_t index)
        const;

        /*! @brief Return the number of entities.
         @returns The number of entities. */
        size_t
        getNumberOfEntities(void)
        const;

    protected :

    private :

        /*! @brief The copy constructor.
         @param other The object to be copied. */
        EntitiesData(const EntitiesData & other);

        /*! @brief The assignment operator.
         @param other The object to be copied.
         @returns The updated object. */
        EntitiesData &
        operator =(const EntitiesData & other);

    public :

    protected :

    private :

        /*! @brief A set of connections. */
        ConnectionList _connections;

        /*! @brief A set of entities. */
        EntitiesList _entities;

    }; // EntitiesData

} // MPlusM_Manager

#endif // ! defined(mpmEntitiesData_HPP_)
