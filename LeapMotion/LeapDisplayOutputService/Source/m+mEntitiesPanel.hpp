//--------------------------------------------------------------------------------------------------
//
//  File:       m+mEntitiesPanel.hpp
//
//  Project:    m+m
//
//  Contains:   The class declaration for the entities layer of the primary window of the m+m
//              manager application.
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
//  Created:    2014-07-14
//
//--------------------------------------------------------------------------------------------------

#if (! defined(mpmEntitiesPanel_HPP_))
# define mpmEntitiesPanel_HPP_ /* Header guard */

# include "m+mManagerDataTypes.hpp"

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file

 @brief The class declaration for the entities layer of the primary window of the m+mLeapDisplayOutputService
 application. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MPlusM_Manager
{
    class ChannelContainer;
    class ChannelEntry;
    class ContentPanel;

    /*! @brief The entities layer of the main window of the application. */
    class EntitiesPanel : public Component
    {
    public :

    protected :

    private :

        /*! @brief The class that this class is derived from. */
        typedef Component inherited;

    public :

        /*! @brief The constructor.
         @param container The container in which the panel is embedded.
         @param startingWidth The initial width to use, or zero to use a default width.
         @param startingHeight The initial height to use, or zero to uase a default height. */
        explicit
        EntitiesPanel(ContentPanel * container,
                      const int      startingWidth = 0,
                      const int      startingHeight = 0);

        /*! @brief The destructor. */
        virtual
        ~EntitiesPanel(void);

        /*! @brief Add an entity to the list of known entities.
         @param anEntity The entity to be added. */
        void
        addEntity(ChannelContainer * anEntity);

        /*! @brief Recalculate size based on entities present.
         @param andRepaint @c true if a repaint should be performed as well, and @c false if not. */
        void
        adjustSize(const bool andRepaint);

        /*! @brief Call the newly-created flags for all entities. */
        void
        clearAllNewlyCreatedFlags(void);

        /*! @brief Call the visited flags for all entities. */
        void
        clearAllVisitedFlags(void);

        /*! @brief Clear any active dragging information. */
        void
        clearDragInfo(void);

        /*! @brief Clear any connect / disconnect markers. */
        void
        clearMarkers(void);

# if defined(USE_OGDF_POSITIONING_)
        /*! @brief Clear any node values. */
        void
        clearNodeValues(void);
# endif // defined(USE_OGDF_POSITIONING_)

        /*! @brief Release all data held by the panel. */
        void
        clearOutData(void);

        /*! @brief Find an entity in the currently-displayed list by name.
         @param name The name of the entity.
         @returns @c NULL if the entity cannot be found and non-@c NULL if it is found. */
        ChannelContainer *
        findKnownEntity(const YarpString & name);

        /*! @brief Find a port in the to-be-displayed list by name.
         @param name The name of the port.
         @returns @c NULL if the port cannot be found and non-@c NULL if it is found. */
        ChannelEntry *
        findKnownPort(const YarpString & name);

        /*! @brief Remove a port from the set of known ports.
         @param aPort The port to be removed. */
        void
        forgetPort(ChannelEntry * aPort);

        /*! @brief Return the font to be used for bold text.
         @returns The font to be used for bold text. */
        inline Font &
        getBoldFont(void)
        {
            return *_defaultBoldFont;
        } // getBoldFont

        /*! @brief Return the container in which the panel is embedded.
         @returns The container in which the panel is embedded. */
        inline ContentPanel *
        getContent(void)
        const
        {
            return _container;
        } // getContent

        /*! @brief Return an entity by index.
         @param index The zero-origin index of the entity.
         @returns The entity if the index is within range and @c NULL otherwise. */
        ChannelContainer *
        getEntity(const size_t index)
        const;

        /*! @brief Return the starting point for a connection being added.
         @returns The starting point for a connection being added. */
        inline ChannelEntry *
        getFirstAddPoint(void)
        const
        {
            return _firstAddPoint;
        } // getFirstAddPoint

        /*! @brief Return the starting point for a connection being removed.
         @returns The starting point for a connection being removed. */
        inline ChannelEntry *
        getFirstRemovePoint(void)
        const
        {
            return _firstRemovePoint;
        } // getFirstRemovePoint

        /*! @brief Return the font to be used for normal text.
         @returns The font to be used for normal text. */
        inline Font &
        getNormalFont(void)
        {
            return *_defaultNormalFont;
        } // getNormalFont

        /*! @brief Return the number of entities.
         @returns The number of entities. */
        size_t
        getNumberOfEntities(void)
        const;

        /*! @brief Returns the number of hidden entities.
         @returns The number of hidden entities. */
        size_t
        getNumberOfHiddenEntities(void)
        const;

        /*! @brief Mark all connections as invalid. */
        void
        invalidateAllConnections(void);

        /*! @brief Return @c true if dragging a connection and @c false otherwise.
         @returns @c true if dragging a connection and @c false otherwise. */
        inline bool
        isDragActive(void)
        const
        {
            return _dragConnectionActive;
        } // isDragActive

        /*! @brief Returns an entry at the given location, if it exists.
         @param location The coordinates to check.
         @returns A pointer to the entry at the given location, or @c NULL if there is none. */
        ChannelEntry *
        locateEntry(const Position & location)
        const;

        /*! @brief Restore the positions of all the entities in the panel. */
        void
        recallPositions(void);

        /*! @brief Record the initial entry when adding or removing a connection.
         @param aPort The first entry selected.
         @param beingAdded @c true if the connection is being added and @c false if it is being
         removed. */
        void
        rememberConnectionStartPoint(ChannelEntry * aPort = NULL,
                                     const bool     beingAdded = false);

        /*! @brief Record a newly added port.
         @param aPort The port to be recorded. */
        void
        rememberPort(ChannelEntry * aPort);

        /*! @brief Record the positions of all the entities in the panel. */
        void
        rememberPositions(void);

        /*! @brief Remove connections that are invalid. */
        void
        removeInvalidConnections(void);

        /*! @brief Remove any entities that were not visited.
         @returns @c true if an entity was removed and @c false otherwise. */
        bool
        removeUnvisitedEntities(void);

        /*! @brief Update the dragging information.
         @param position The location of the dragging connection.
         @param isForced @c true if the drag line should show a forced connection and @c false
         otherwise. */
        void
        setDragInfo(const Position position,
                    const bool     isForced);

        /*! @brief Ignore the result of the next scan. */
        void
        skipScan(void);

        /* @brief Made hidden entities visible. */
        void
        unhideEntities(void);

    protected :

    private :

        /*! @brief Respond to a request for a popup menu. */
        void
        displayAndProcessPopupMenu(void);

        /*! @brief Display the connections between containers.
         @param gg The graphics context in which to draw. */
        void
        drawConnections(Graphics & gg);

        /*! @brief Called when a mouse button is pressed.
         @param ee Details about the position and status of the mouse event. */
        virtual void
        mouseDown(const MouseEvent & ee);

        /*! @brief Called when a mouse button is released.
         @param ee Details about the position and status of the mouse event. */
        virtual void
        mouseUp(const MouseEvent& ee);

        /*! @brief Draw the content of the component.
         @param gg The graphics context in which to draw. */
        virtual void
        paint(Graphics & gg);

        /*! @brief Called when the component size has been changed. */
        virtual void
        resized(void);

    public :

    protected :

    private :

        /*! @brief The set of known ports. */
        ChannelEntryMap _knownPorts;

        /*! @brief A collection of known services and ports. */
        ContainerList _knownEntities;

        /*! @brief The bold font to be used. */
        ScopedPointer<Font> _defaultBoldFont;

        /*! @brief The normal font to be used. */
        ScopedPointer<Font> _defaultNormalFont;

        /*! @brief The coordinates of the drag-connection operation. */
        Position _dragPosition;

        /*! @brief The starting port for a connection being added. */
        ChannelEntry * _firstAddPoint;

        /*! @brief The starting port for a connection being removed. */
        ChannelEntry * _firstRemovePoint;

        /*! @brief The container in which the panel is embedded. */
        ContentPanel * _container;

        /*! @brief @c true if a drag-connection operation is active. */
        bool _dragConnectionActive;

        /*! @brief @c true if the drag operation is for a forced connection. */
        bool _dragIsForced;

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunused-private-field"
# endif // defined(__APPLE__)
        /*! @brief Filler to pad to alignment boundary */
        char _filler[7];
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

        JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR(EntitiesPanel)

    }; // EntitiesPanel

} // MPlusM_Manager

#endif // ! defined(mpmEntitiesPanel_HPP_)
