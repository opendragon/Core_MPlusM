//--------------------------------------------------------------------------------------------------
//
//  File:       m+mChannelEntry.hpp
//
//  Project:    m+m
//
//  Contains:   The class declaration for a visible entity that represents a channel or a port.
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
//  Created:    2014-07-16
//
//--------------------------------------------------------------------------------------------------

#if (! defined(mpmChannelEntry_HPP_))
# define mpmChannelEntry_HPP_ /* Header guard */

# include "m+mManagerDataTypes.hpp"

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file

 @brief The class declaration for a visible entity that represents a channel or a port. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MPlusM_Manager
{
    class ChannelContainer;
    class EntitiesPanel;

    /*! @brief A port or channel. */
    class ChannelEntry : public Component
    {
    public :

    protected :

    private :

        /*! @brief The class that this class is derived from. */
        typedef Component inherited;

    public :

        /*! @brief The constructor.
         @param parent The GUI element containing this element.
         @param portName The port name for the entry.
         @param portNumber The port number for the entry.
         @param portProtocol The protocol of the port.
         @param protocolDescription The description of the protocol.
         @param portKind What the port will be used for.
         @param direction The primary direction of the port. */
        ChannelEntry(ChannelContainer *  parent,
                     const YarpString &  portName,
                     const YarpString &  portNumber,
                     const YarpString &  portProtocol,
                     const YarpString &  protocolDescription,
                     const PortUsage     portKind,
                     const PortDirection direction = kPortDirectionInput);

        /*! @brief The destructor. */
        virtual
        ~ChannelEntry(void);

        /*! @brief Add an input connection to the port.
         @param other The port that is to be connected.
         @param mode The mode of the connection.
         @param wasOverridden @c true if the protocol matching was overridden and @c false
         otherwise. */
        void
        addInputConnection(ChannelEntry *                    other,
                           const MplusM::Common::ChannelMode mode,
                           const bool                        wasOverridden);

        /*! @brief Add an output connection to the port.
         @param other The port that is to be connected.
         @param mode The mode of the connection.
         @param wasOverridden @c true if the protocol matching was overridden and @c false
         otherwise. */
        void
        addOutputConnection(ChannelEntry *                    other,
                            const MplusM::Common::ChannelMode mode,
                            const bool                        wasOverridden);

        /*! @brief Determine the anchor point that is the minimum distance from a given point.
         @param result The coordinates of the anchor point.
         @param isSource @c true if the anchor is for an outgoing line and @c false otherwise.
         @param disallowBottom @c true if the anchor cannot be bottom-centre.
         @param xx The horizontal coordinate for the point of interest.
         @param yy The vertical coordinate for the point of interest.
         @returns The side to which the anchor is attached. */
        inline AnchorSide
        calculateClosestAnchor(Position &  result,
                               const bool  isSource,
                               const bool  disallowBottom,
                               const float xx,
                               const float yy)
        const
        {
            return calculateClosestAnchor(result, isSource, disallowBottom, Position(xx, yy));
        } // calculateClosestAnchor

        /*! @brief Determine the anchor point that is the minimum distance from a given point.
         @param result The coordinates of the anchor point.
         @param isSource @c true if the anchor is for an outgoing line and @c false otherwise.
         @param disallowBottom @c true if the anchor cannot be bottom-centre.
         @param pp The point of interest.
         @returns The side to which the anchor is attached. */
        AnchorSide
        calculateClosestAnchor(Position &       result,
                               const bool       isSource,
                               const bool       disallowBottom,
                               const Position & pp)
        const;

        /*! @brief Stop displaying the connect marker. */
        void
        clearConnectMarker(void);

        /*! @brief Stop displaying the disconnect marker. */
        void
        clearDisconnectMarker(void);

        /*! @brief Display metrics for a channel. */
        void
        displayChannelMetrics(void);

        /*! @brief Display information for a port.
         @param isChannel @c true if the port is a channel and @c false otherwise.
         @param moreDetails @c true if more details are to be shown and @c false otherwise. */
        void
        displayInformation(const bool isChannel,
                           const bool moreDetails);

        /*! @brief Draw a drag line from an entry.
         @param gg The graphics context in which to draw.
         @param position The coordinates of the drag line endpoint.
         @param isUDP @c true if the connection is UDP and @c false otherwise.
         @param isForced @c true if the connection is forced and @c false otherwise. */
        void
        drawDragLine(Graphics &       gg,
                     const Position & position,
                     const bool       isUDP,
                     const bool       isForced);

        /*! @brief Display the connections between containers.
         @param gg The graphics context in which to draw. */
        void
        drawOutgoingConnections(Graphics & gg);

        /*! @brief Return the location of the centre of the port entry.
         @returns The location of the centre of the port entry. */
        Position
        getCentre(void)
        const;

        /*! @brief Return the direction of the port entry.
         @returns The direction of the port entry. */
        inline PortDirection
        getDirection(void)
        const
        {
            return _direction;
        } // getDirection

        /*! @brief Return the set of input connections to the port.
         @returns The set of input connections to the port. */
        inline const ChannelConnections &
        getInputConnections(void)
        const
        {
            return _inputConnections;
        } // getInputConnections

        /*! @brief Return the set of output connections to the port.
         @returns The set of output connections to the port. */
        inline const ChannelConnections &
        getOutputConnections(void)
        const
        {
            return _outputConnections;
        } // getOutputConnections

        /*! @brief Return the panel which contains the entry.
         @returns The panel which contains the entry. */
        EntitiesPanel &
        getOwningPanel(void)
        const;

        /*! @brief Return the container holding this entry.
         @returns The container holding this entry. */
        inline ChannelContainer *
        getParent(void)
        const
        {
            return _parent;
        } // getParent

        /*! @brief Return the name of the associated port.
         @returns The name of the associated port. */
        inline const YarpString &
        getPortName(void)
        const
        {
            return _portName;
        } // getPortName

        /*! @brief Return the port number of the associated port.
         @returns The port number of the associated port. */
        inline const YarpString &
        getPortNumber(void)
        const
        {
            return _portPortNumber;
        } // getPortNumber

        /*! @brief Return the position of the entity within it's containing panel.
         @returns The position of the entity within it's containing panel. */
        Position
        getPositionInPanel(void)
        const;

        /*! @brief Return the protocol of the associated port.
         @returns The protocol of the associated port. */
        inline const YarpString &
        getProtocol(void)
        const
        {
            return _portProtocol;
        } // getProtocol

        /*! @brief Return the description of the protocol for the associated port.
         @returns The description of the protocol for the associated port. */
        inline const YarpString &
        getProtocolDescription(void)
        const
        {
            return _protocolDescription;
        } // getProtocolDescription

        /*! @brief Return the usage of the port entry.
         @returns The usage of the port entry. */
        inline PortUsage
        getUsage(void)
        const
        {
            return _usage;
        } // getUsage

        /*! @brief Returns @c true if there is an outgoing connection to the named port.
         @param otherPort The name of the destination port.
         @returns @c true if there is an outgoing connection to the named port. */
        bool
        hasOutgoingConnectionTo(const YarpString & otherPort)
        const;

        /*! @brief Mark all the connections as invalid. */
        void
        invalidateConnections(void);

        /*! @brief Returns @c true if the channel is being monitored and @c false otherwise.
         @returns @c true if the channel is being monitored and @c false otherwise. */
        inline bool
        isBeingMonitored(void)
        const
        {
            return _beingMonitored;
        } // isBeingMonitored

        /*! @brief Returns @c true if the port is a channel and @c false if it is a standard port.
         @returns @c true if the port is a channel and @c false otherwise. */
        bool
        isChannel(void)
        const;

        /*! @brief Returns @c true if the port entry is a secondary port of a service and @c false
         otherwise.
         @returns @c true if the port is a secondary port of a service and @c false otherwise. */
        inline bool
        isInputOutput(void)
        const
        {
            return (kPortUsageInputOutput == _usage);
        } // isInputOutput

        /*! @brief Returns @c true if the port entry is the bottom-most (last) port entry in a
         panel and @c false otherwise.
         @returns @c true if the port is the last port entry in a panel and @c false
         otherwise. */
        inline bool
        isLastPort(void)
        const
        {
            return _isLastPort;
        } // isLastPort

        /*! @brief Returns @c true if the port entry is marked and @c false otherwise.
         @returns @c true if the port entry is marked and @c false otherwise. */
        inline bool
        isMarked(void)
        const
        {
            return (_drawConnectMarker || _drawDisconnectMarker);
        } // isMarked

        /*! @brief Returns @c true if the port entry is part of a service and @c false
         otherwise.
         @returns @c true if the port is part of a service and @c false otherwise. */
        inline bool
        isService(void)
        const
        {
            return (kPortUsageService == _usage);
        } // isService

        /*! @brief Remove an input connection from a port.
         @param other The port that is to be disconnected. */
        void
        removeInputConnection(ChannelEntry * other);

        /*! @brief Remove connections that are invalid. */
        void
        removeInvalidConnections(void);

        /*! @brief Remove an output connection from a port.
         @param other The port that is to be disconnected. */
        void
        removeOutputConnection(ChannelEntry * other);

        /*! @brief Mark the port entry as the bottom-most (last) port entry in a panel. */
        void
        setAsLastPort(void);

        /*! @brief Start displaying the connect marker. */
        void
        setConnectMarker(void);

        /*! @brief Start displaying the disconnect marker. */
        void
        setDisconnectMarker(void);

        /*! @brief Mark the port entry as not being the bottom-most port entry in a panel. */
        void
        unsetAsLastPort(void);

        /*! @brief Return @c true if the current connection request was UDP and @c false
         otherwise.
         @returns @c true if the current connection request was UDP and @c false otherwise. */
        inline bool
        wasUdpConnectionRequest(void)
        const
        {
            return _wasUdp;
        } // wasUdpConnectionRequest

    protected :

    private :

        /*! @brief Check if the connection is present.
         @param otherEnd The connection information.
         @param isOutgoing @c true if this is an outgoing connection and @c false otherwise.
         @returns @c true if the YARP connection exists and @c false otherwise. */
        bool
        checkConnection(ChannelInfo & otherEnd,
                        const bool    isOutgoing);

        /*! @brief Respond to a request for a popup menu. */
        void
        displayAndProcessPopupMenu(void);

        /*! @brief Called when a mouse button is pressed.
         @param ee Details about the position and status of the mouse event. */
        virtual void
        mouseDown(const MouseEvent & ee);

        /*! @brief Called when the mouse is moved while a button is held down.
         @param ee Details about the position and status of the mouse event. */
        virtual void
        mouseDrag(const MouseEvent & ee);

        /*! @brief Called when a mouse button is released.
         @param ee Details about the position and status of the mouse event. */
        virtual void
        mouseUp(const MouseEvent & ee);

        /*! @brief Draw the content of the component.
         @param gg The graphics context in which to draw. */
        virtual void
        paint(Graphics & gg);

        /*! @brief Remove all connections. */
        void
        removeAllConnections(void);

    public :

    protected :

    private :

        /*! @brief The connections to the port. */
        ChannelConnections _inputConnections;

        /*! @brief The connections to the port. */
        ChannelConnections _outputConnections;

        /*! @brief The name of the associated port. */
        YarpString _portName;

        /*! @brief The IP port number for the port. */
        YarpString _portPortNumber;

        /*! @brief The protocol of the associated port. */
        YarpString _portProtocol;

        /*! @brief The description of the protocol for the associated port. */
        YarpString _protocolDescription;

        /*! @brief The text to be displayed for the channel entry. */
        YarpString _title;

        /*! @brief The container in which this is embedded. */
        ChannelContainer * _parent;

        /*! @brief The primary direction for connections to the port. */
        PortDirection _direction;

        /*! @brief The primary usage for the port. */
        PortUsage _usage;

        /*! @brief @c true if activity on the channel is being monitored and @c false otherwise. */
        bool _beingMonitored;

        /*! @brief @c true if the activity marker is to be displayed and @c false otherwise. */
        bool _drawActivityMarker;

        /*! @brief @c true if the connect marker is to be displayed and @c false otherwise. */
        bool _drawConnectMarker;

        /*! @brief @c true if the disconnect marker is to be displayed and @c false otherwise. */
        bool _drawDisconnectMarker;

        /*! @brief @c true if the channel entry is the bottom-most (last) channel entry in a panel
         and @c false otherwise. */
        bool _isLastPort;

        /*! @brief @c true if the drag connection is for UDP and @c false otherwise. */
        bool _wasUdp;

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunused-private-field"
# endif // defined(__APPLE__)
        /*! @brief Filler to pad to alignment boundary */
        char _filler[2];
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

        JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR(ChannelEntry)

    }; // ChannelEntry

} // MPlusM_Manager

#endif // ! defined(mpmChannelEntry_HPP_)
