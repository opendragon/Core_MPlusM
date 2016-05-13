//--------------------------------------------------------------------------------------------------
//
//  File:       m+mChannelEntry.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for a visible entity that represents a channel or a port.
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

#include "m+mChannelEntry.hpp"

#include "m+mChannelContainer.hpp"
#include "m+mContentPanel.hpp"
#include "m+mEntitiesPanel.hpp"

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file

 @brief The class definition for a visible entity that represents a channel or a port. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MPlusM_Manager;
using namespace std;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief The colour to be used for the entry background. */
static const Colour & kEntryBackgroundColour(Colours::black);

/*! @brief The colour to be used for text in the entry. */
static const Colour & kEntryTextColour(Colours::white);

/*! @brief The first colour to be used for the activity marker. */
static const Colour & kFirstActivityMarkerColour(Colours::yellow);

/*! @brief The colour to be used for markers. */
static const Colour & kMarkerColour(Colours::yellow);

/*! @brief The colour to be used for non-TCP/non-UDP connection. */
static const Colour & kOtherConnectionColour(Colours::orange);

/*! @brief The second colour to be used for the activity marker. */
static const Colour & kSecondActivityMarkerColour(Colours::orange);

/*! @brief The colour to be used for TCP connections. */
static const Colour & kTcpConnectionColour(Colours::teal);

/*! @brief The colour to be used for UDP connections. */
static const Colour & kUdpConnectionColour(Colours::purple);

/*! @brief The inset for the activity indicator. */
static const float kActivityInset = 2;

/*! @brief The horizontal and vertical length of the arrow 'arm'. */
static const float kArrowSize = 7;

/*! @brief The scale factor to apply to get the length of the control vector. */
static const float kControlLengthScale = 0.25;

/*! @brief The width and height of the marker displayed during movement. */
static const float kMarkerSide = 10;

/*! @brief The line width for an input / output connection. */
static const float kInputOutputConnectionWidth = 4;

/*! @brief The line width for a normal connection. */
static const float kNormalConnectionWidth = 2;

/*! @brief The line width for a normal connection. */
static const float kServiceConnectionWidth = 6;

/*! @brief The scale factor to apply to get the size of the target box. */
static const float kTargetBoxScale = 0.25;

#if defined(__APPLE__)
# pragma mark Global constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

/*! @brief Determine if a new point is closer to a reference point than the previous point.
 @param distanceSoFar On input, the closest distance so far and on output, the new closest distance.
 @param refPoint The point to measure distance from.
 @param testPoint The point being checked.
 @param bestSoFar On input, the current closest point and output, the new closest point.
 @returns @c true if the new point is closer than the previous closest point. */
static bool
calculateMinDistance(float &          distanceSoFar,
                     const Position & refPoint,
                     const Position & testPoint,
                     Position &       bestSoFar)
{
    ODL_ENTER(); //####
    ODL_P4("distanceSoFar = ", &distanceSoFar, "refPoint = ", &refPoint, "testPoint = ", //####
           &testPoint, "bestSoFar = ", &bestSoFar); //####
    bool  result;
    float newDistance = refPoint.getDistanceFrom(testPoint);

    if (newDistance < distanceSoFar)
    {
        distanceSoFar = newDistance;
        bestSoFar = testPoint;
        result = true;
    }
    else
    {
        result = false;
    }
    ODL_EXIT_B(result); //####
    return result;
} // calculateMinDistance

/*! @brief Determine if a new point is closer to a reference point than the previous point.
 @param distanceSoFar On input, the closest distance so far and on output, the new closest distance.
 @param refPoint The point to measure distance from.
 @param testX The horizontal coordinate for the point being checked.
 @param testY The vertical coordinate for the point being checked.
 @param bestSoFar On input, the current closest point and output, the new closest point.
 @returns @c true if the new point is closer than the previous closest point. */
inline static bool
calculateMinDistance(float &          distanceSoFar,
                     const Position & refPoint,
                     const float      testX,
                     const float      testY,
                     Position &       bestSoFar)
{
    return calculateMinDistance(distanceSoFar, refPoint, Position(testX, testY), bestSoFar);
} // calculateMinDistance

/*! @brief Determine the anchor point that is the minimum distance from a given point.
 @param newCentre The synthesized centre for the target point.
 @param disallowBottom @c true if the anchor cannot be bottom-centre.
 @param targetPoint The target point.
 @param refCentre The reference point.
 @returns The side to which the anchor is attached. */
static AnchorSide
calculateAnchorForPoint(Position &       newCentre,
                        const bool       disallowBottom,
                        const Position & targetPoint,
                        const Position & refCentre)
{
    ODL_ENTER(); //####
    ODL_P3("newCentre = ", &newCentre, "targetPoint = ", &targetPoint, "refCentre = ", //####
           &refCentre); //####
    AnchorSide             anchor = kAnchorUnknown;
    float                  boxSize = (refCentre.getDistanceFrom(targetPoint) * kTargetBoxScale);
    float                  soFar = static_cast<float>(1e23); // Ridiculously big, just in case.
    Position               tempPoint;
    juce::Rectangle<float> box(targetPoint.getX() - (boxSize / 2),
                               targetPoint.getY() - (boxSize / 2), boxSize, boxSize);

    if (calculateMinDistance(soFar, refCentre, box.getX(), box.getY() + (boxSize / 2), tempPoint))
    {
        anchor = kAnchorLeft;
        newCentre = targetPoint + Position(boxSize, 0);
    }
    if (calculateMinDistance(soFar, refCentre, box.getX() + boxSize, box.getY() + (boxSize / 2),
                             tempPoint))
    {
        anchor = kAnchorRight;
        newCentre = targetPoint + Position(-boxSize, 0);
    }
    if ((! disallowBottom) && calculateMinDistance(soFar, refCentre, box.getX() + (boxSize / 2),
                                                   box.getY() + boxSize, tempPoint))
    {
        anchor = kAnchorBottomCentre;
        newCentre = targetPoint + Position(0, -boxSize);
    }
    if (calculateMinDistance(soFar, refCentre, box.getX() + (boxSize / 2), box.getY(), tempPoint))
    {
        anchor = kAnchorTopCentre;
        newCentre = targetPoint + Position(0, boxSize);
    }
    ODL_EXIT_L(static_cast<int>(anchor)); //####
    return anchor;
} // calculateAnchorForPoint

/*! @brief Displays an anchor leaving the given location.
 @param gg The graphics context in which to draw.
 @param anchor The side to which the anchor is attached.
 @param anchorPos The coordinates of the anchor point.
 @param thickness The line thickness to be used. */
static void
drawSourceAnchor(Graphics &       gg,
                 const AnchorSide anchor,
                 const Position & anchorPos,
                 const float      thickness)
{
    ODL_ENTER(); //####
    ODL_P2("gg = ", &gg, "anchorPos = ", &anchorPos); //####
    ODL_LL1("anchor = ", static_cast<int>(anchor)); //####
    ODL_D1("thickness = ", thickness); //####
    Position first;
    Position second;

    switch (anchor)
    {
        case kAnchorLeft :
            first = anchorPos + Position(kArrowSize, -kArrowSize);
            second = anchorPos + Position(kArrowSize, kArrowSize);
            break;

        case kAnchorRight :
            first = anchorPos + Position(-kArrowSize, -kArrowSize);
            second = anchorPos + Position(-kArrowSize, kArrowSize);
            break;

        case kAnchorBottomCentre :
            first = anchorPos + Position(-kArrowSize, -kArrowSize);
            second = anchorPos + Position(kArrowSize, -kArrowSize);
            break;

        case kAnchorTopCentre :
            first = anchorPos + Position(-kArrowSize, kArrowSize);
            second = anchorPos + Position(kArrowSize, kArrowSize);
            break;

        default :
            break;

    }
    if (kAnchorUnknown != anchor)
    {
        ODL_D4("anchor.x = ", anchorPos.getX(), "anchor.y = ", anchorPos.getY(), //####
               "first.x = ", first.getX(), "first.y = ", first.getY()); //####
        ODL_D2("second.x = ", second.getX(), "second.y = ", second.getY()); //####
        gg.drawLine(anchorPos.getX(), anchorPos.getY(), first.getX(), first.getY(), thickness);
        gg.drawLine(anchorPos.getX(), anchorPos.getY(), second.getX(), second.getY(), thickness);
    }
    ODL_EXIT(); //####
} // drawSourceAnchor

/*! @brief Displays an anchor arriving at the given location.
 @param gg The graphics context in which to draw.
 @param anchor The side to which the anchor is attached.
 @param anchorPos The coordinates of the anchor point.
 @param thickness The line thickness to be used. */
static void
drawTargetAnchor(Graphics &       gg,
                 const AnchorSide anchor,
                 const Position & anchorPos,
                 const float      thickness)
{
    ODL_ENTER(); //####
    ODL_P2("gg = ", &gg, "anchorPos = ", &anchorPos); //####
    ODL_LL1("anchor = ", static_cast<int>(anchor)); //####
    ODL_D1("thickness = ", thickness); //####
    Position first;
    Position second;

    switch (anchor)
    {
        case kAnchorLeft :
            first = anchorPos + Position(-kArrowSize, -kArrowSize);
            second = anchorPos + Position(-kArrowSize, kArrowSize);
            break;

        case kAnchorRight :
            first = anchorPos + Position(kArrowSize, -kArrowSize);
            second = anchorPos + Position(kArrowSize, kArrowSize);
            break;

        case kAnchorBottomCentre :
            first = anchorPos + Position(-kArrowSize, kArrowSize);
            second = anchorPos + Position(kArrowSize, kArrowSize);
            break;

        case kAnchorTopCentre :
            first = anchorPos + Position(-kArrowSize, -kArrowSize);
            second = anchorPos + Position(kArrowSize, -kArrowSize);
            break;

        default :
            break;

    }
    if (kAnchorUnknown != anchor)
    {
        ODL_D4("anchor.x = ", anchorPos.getX(), "anchor.y = ", anchorPos.getY(), //####
               "first.x = ", first.getX(), "first.y = ", first.getY()); //####
        ODL_D2("second.x = ", second.getX(), "second.y = ", second.getY()); //####
        gg.drawLine(anchorPos.getX(), anchorPos.getY(), first.getX(), first.getY(), thickness);
        gg.drawLine(anchorPos.getX(), anchorPos.getY(), second.getX(), second.getY(), thickness);
    }
    ODL_EXIT(); //####
} // drawTargetAnchor

/*! @brief Draw a bezier curve between two points.
 @param gg The graphics context in which to draw.
 @param startPoint The beginning of the curve.
 @param endPoint The end of the curve.
 @param startCentre A reference point for the beginning of the curve, used to calculate the
 beginning tangent.
 @param endCentre A reference point for the end of the curve, used to calculate the ending
 tangent.
 @param thickness The line thickness to be used.
 @param isDashed @c true if the line should be dashed and @c false otherwise. */
static void
drawBezier(Graphics &       gg,
           const Position & startPoint,
           const Position & endPoint,
           const Position & startCentre,
           const Position & endCentre,
           const float      thickness,
           const bool       isDashed)
{
    ODL_ENTER(); //####
    ODL_P4("gg = ", &gg, "startPoint = ", &startPoint, "endPoint = ", &endPoint, //####
           "startCentre = ", &startCentre); //####
    ODL_P1("endCentre = ", &endCentre); //####
    ODL_D1("thickness = ", thickness); //####
    Path     bezPath;
    float    controlLength = (startPoint.getDistanceFrom(endPoint) * kControlLengthScale);
    float    startAngle = atan2(startPoint.getY() - startCentre.getY(),
                                startPoint.getX() - startCentre.getX());
    float    endAngle = atan2(endPoint.getY() - endCentre.getY(),
                              endPoint.getX() - endCentre.getX());
    Position controlPoint1(controlLength * cos(startAngle), controlLength * sin(startAngle));
    Position controlPoint2(controlLength * cos(endAngle), controlLength * sin(endAngle));

    bezPath.startNewSubPath(startPoint);
    bezPath.cubicTo(startPoint + controlPoint1, endPoint + controlPoint2, endPoint);
    if (isDashed)
    {
        float          newThickness = sqrt(thickness);
        PathStrokeType strokeType(newThickness);
        const float    dashes[] = { 5, 10 };
        const int      numDashes = (sizeof(dashes) / sizeof(*dashes));
        Path           strokedPath;

        strokeType.createDashedStroke(strokedPath, bezPath, dashes, numDashes);
        gg.strokePath(strokedPath, PathStrokeType(newThickness));
    }
    else
    {
        gg.strokePath(bezPath, PathStrokeType(thickness));
    }
    ODL_EXIT(); //####
} // drawBezier

/*! @brief Draw a connection between entries.
 @param gg The graphics context in which to draw.
 @param source The originating entry.
 @param destination The terminating entry.
 @param mode The kind of connection.
 @param forced @c true if the protocols were overridden and @c false otherwise. */
static void
drawConnection(Graphics &                gg,
               ChannelEntry *            source,
               ChannelEntry *            destination,
               const Common::ChannelMode mode,
               const bool                forced)
{
    ODL_ENTER(); //####
    ODL_P3("gg = ", &gg, "source = ", source, "destination = ", destination); //####
    ODL_LL1("mode = ", static_cast<int>(mode)); //####
    ODL_B1("forced = ", forced); //####
    if (source && destination)
    {
        AnchorSide sourceAnchor;
        AnchorSide destinationAnchor;
        bool       isBidirectional = false;
        Position   sourcePosition(source->getPositionInPanel());
        Position   destinationPosition(destination->getPositionInPanel());
        Position   sourceCentre(source->getCentre() + sourcePosition);
        Position   destinationCentre(destination->getCentre() + destinationPosition);
        Position   startPoint;
        Position   endPoint;
        float      thickness;

        ODL_D4("sourcePosition.x = ", sourcePosition.getX(), "sourcePosition.y = ", //####
               sourcePosition.getY(), "destinationPosition.x = ", //####
               destinationPosition.getX(), "destinationPosition.y = ", //####
               destinationPosition.getY()); //####
        ODL_D4("sourceCentre.x = ", sourceCentre.getX(), "sourceCentre.y = ", //####
               sourceCentre.getY(), "destinationCentre.x = ", destinationCentre.getX(), //####
               "destinationCentre.y = ", destinationCentre.getY()); //####
        if (destination->isService())
        {
            isBidirectional = true;
            thickness = kServiceConnectionWidth;
        }
        else if (destination->isInputOutput())
        {
            thickness = kInputOutputConnectionWidth;
        }
        else
        {
            thickness = kNormalConnectionWidth;
        }
        ODL_B1("isBidirectional <- ", isBidirectional); //####
        ODL_D1("thickness <- ", thickness); //####
        // Check if the destination is above the source, in which case we determine the anchors in
        // the reverse order.
        if (sourceCentre.getY() < destinationCentre.getY())
        {
            sourceAnchor = source->calculateClosestAnchor(startPoint, ! isBidirectional, false,
                                                          destinationCentre);
            destinationAnchor = destination->calculateClosestAnchor(endPoint, false,
                                                                kAnchorBottomCentre == sourceAnchor,
                                                                    sourceCentre);
        }
        else
        {
            destinationAnchor = destination->calculateClosestAnchor(endPoint, false, false,
                                                                    sourceCentre);
            sourceAnchor = source->calculateClosestAnchor(startPoint, ! isBidirectional,
                                                          kAnchorBottomCentre == destinationAnchor,
                                                          destinationCentre);
        }
        ODL_D4("startPoint.x <- ", startPoint.getX(), "startPoint.y <- ", //####
               startPoint.getY(), "endPoint.x <- ", endPoint.getX(), "endPoint.y <- ", //####
               endPoint.getY()); //####
        switch (mode)
        {
            case Common::kChannelModeTCP :
                gg.setColour(kTcpConnectionColour);
                break;

            case Common::kChannelModeUDP :
                gg.setColour(kUdpConnectionColour);
                break;

            default :
                gg.setColour(kOtherConnectionColour);
                break;

        }
        drawBezier(gg, startPoint, endPoint, sourceCentre, destinationCentre, thickness, forced);
        if (isBidirectional)
        {
            drawTargetAnchor(gg, sourceAnchor, startPoint, 1);
        }
        else
        {
            drawSourceAnchor(gg, sourceAnchor, startPoint, 1);
        }
        drawTargetAnchor(gg, destinationAnchor, endPoint, 1);
    }
    ODL_EXIT(); //####
} // drawConnection

/*! @brief Determine whether a connection can be made, based on the port protocols.
 @param sourceProtocol The protocol of the source port.
 @param destinationProtocol The protocol of the destination port.
 @param ignoreConstraints @c true if the protocols don't have to match.
 @returns @c true if the protocols permit a connection to be made and @c false
 otherwise. */
static bool
protocolsMatch(const YarpString & sourceProtocol,
               const YarpString & destinationProtocol,
               const bool         ignoreConstraints)
{
    ODL_ENTER(); //####
    ODL_S2s("sourceProtocol = ", sourceProtocol, "destinationProtocol = ", //####
            destinationProtocol); //####
    ODL_B1("ignoreConstraints = ", ignoreConstraints); //####
    bool result = false;

    if (0 == destinationProtocol.length())
    {
        result = true;
    }
    else if (destinationProtocol == "*")
    {
        result = true;
    }
    else
    {
        result = (ignoreConstraints || (sourceProtocol == destinationProtocol));
    }
    ODL_EXIT_B(result); //####
    return result;
} // protocolsMatch

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

ChannelEntry::ChannelEntry(ChannelContainer *  parent,
                           const YarpString &  portName,
                           const YarpString &  portNumber,
                           const YarpString &  portProtocol,
                           const YarpString &  protocolDescription,
                           const PortUsage     portKind,
                           const PortDirection direction) :
    inherited(), _portName(portName), _portPortNumber(portNumber), _portProtocol(portProtocol),
    _protocolDescription(protocolDescription), _parent(parent), _direction(direction),
    _usage(portKind), _beingMonitored(false), _drawActivityMarker(false), _drawConnectMarker(false),
    _drawDisconnectMarker(false), _isLastPort(true), _wasUdp(false)
{
    ODL_ENTER(); //####
    ODL_P1("parent = ", parent); //####
    ODL_S4s("portName = ", portName, "portNumber = ", portNumber, "portProtocol = ", //####
            portProtocol, "protocolDescription = ", protocolDescription); //####
    ODL_LL2("portKind = ", portKind, "direction = ", direction); //####
    Font &             textFont = getOwningPanel().getNormalFont();
    YarpString prefix;

    switch (_direction)
    {
        case kPortDirectionInput :
            prefix = ((kPortUsageService == _usage) ? "S " : "In ");
            break;

        case kPortDirectionInputOutput :
            prefix = ((kPortUsageClient == _usage) ? "C " : "I/O ");
            break;

        case kPortDirectionOutput :
            prefix = "Out ";
            break;

        default :
            prefix = "Unk ";
            break;

    }
    _title = prefix + _portName;
    int entryHeight = static_cast<int>(textFont.getHeight());

    setSize(static_cast<int>(textFont.getStringWidthFloat((_title + " ").c_str()) +
                             _parent->getTextInset()) + entryHeight, entryHeight);
    setOpaque(true);
    setVisible(true);
    ODL_EXIT_P(this); //####
} // ChannelEntry::ChannelEntry

ChannelEntry::~ChannelEntry(void)
{
    ODL_OBJENTER(); //####
    ODL_S1s("getPortName() = ", getPortName()); //####
    removeAllConnections();
    ODL_OBJEXIT(); //####
} // ChannelEntry::~ChannelEntry

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void
ChannelEntry::addInputConnection(ChannelEntry *            other,
                                 const Common::ChannelMode mode,
                                 const bool                wasOverridden)
{
    ODL_OBJENTER(); //####
    ODL_P1("other = ", other); //####
    ODL_B1("wasOverridden = ", wasOverridden); //####
    if (other)
    {
        bool canAdd = true;

        for (ChannelConnections::iterator walker(_inputConnections.begin());
             _inputConnections.end() != walker; ++walker)
        {
            ChannelInfo * candidate(&*walker);

            if (candidate)
            {
                if ((candidate->_otherChannel == other) ||
                    (candidate->_otherChannel->getPortName() == other->getPortName()))
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
            ChannelInfo newConnection;

            newConnection._otherChannel = other;
            newConnection._connectionMode = mode;
            newConnection._forced = wasOverridden;
            newConnection._valid = true;
            _inputConnections.push_back(newConnection);
        }
    }
    ODL_OBJEXIT(); //####
} // ChannelEntry::addInputConnection

void
ChannelEntry::addOutputConnection(ChannelEntry *            other,
                                  const Common::ChannelMode mode,
                                  const bool                wasOverridden)
{
    ODL_OBJENTER(); //####
    ODL_P1("other = ", other); //####
    ODL_B1("wasOverridden = ", wasOverridden); //####
    if (other)
    {
        bool canAdd = true;

        for (ChannelConnections::iterator walker(_outputConnections.begin());
             _outputConnections.end() != walker; ++walker)
        {
            ChannelInfo * candidate(&*walker);

            if (candidate)
            {
                if ((candidate->_otherChannel == other) ||
                    (candidate->_otherChannel->getPortName() == other->getPortName()))
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
            ChannelInfo newConnection;

            newConnection._otherChannel = other;
            newConnection._connectionMode = mode;
            newConnection._forced = wasOverridden;
            newConnection._valid = true;
            _outputConnections.push_back(newConnection);
        }
    }
    ODL_OBJEXIT(); //####
} // ChannelEntry::addOutputConnection

AnchorSide
ChannelEntry::calculateClosestAnchor(Position &       result,
                                     const bool       isSource,
                                     const bool       disallowBottom,
                                     const Position & pp)
const
{
    ODL_OBJENTER(); //####
    ODL_P2("result = ", &result, "pp = ", &pp); //####
    ODL_B1("isSource = ", isSource); //####
    // Check each anchor point - the two side centres and optionally the bottom - to find the
    // shortest distance.
    AnchorSide anchor = kAnchorUnknown;
    float      soFar = static_cast<float>(1e23); // Ridiculously big, just in case.
    Position   location(getPositionInPanel());

    if (calculateMinDistance(soFar, pp, location.getX(), location.getY() + (getHeight() / 2),
                             result))
    {
        anchor = kAnchorLeft;
        if (isSource)
        {
            // Adjust the anchor position if an output.
            result.x -= kArrowSize;
        }
    }
    if (calculateMinDistance(soFar, pp, location.getX() + getWidth(),
                             location.getY() + (getHeight() / 2), result))
    {
        anchor = kAnchorRight;
        if (isSource)
        {
            // Adjust the anchor position if an output.
            result.x += kArrowSize;
        }
    }
    if (_isLastPort && (! disallowBottom))
    {
        if (calculateMinDistance(soFar, pp, location.getX() + (getWidth() / 2),
                                 location.getY() + getHeight(), result))
        {
            anchor = kAnchorBottomCentre;
            if (isSource)
            {
                // Adjust the anchor position if an output.
                result.y += kArrowSize;
            }
        }
    }
    ODL_D2("result.x = ", result.getX(), "result.y = ", result.getY()); //####
    ODL_OBJEXIT_L(static_cast<int>(anchor)); //####
    return anchor;
} // ChannelEntry::calculateClosestAnchor

bool
ChannelEntry::checkConnection(ChannelInfo & otherEnd,
                              const bool    isOutgoing)
{
    ODL_OBJENTER(); //####
    ODL_P1("otherEnd = ", &otherEnd); //####
    ODL_B1("isOutgoing = ", isOutgoing); //####
    bool result = false;

    if (isOutgoing)
    {
        result = Utilities::CheckConnection(getPortName(), otherEnd._otherChannel->getPortName());
    }
    else
    {
        result = Utilities::CheckConnection(otherEnd._otherChannel->getPortName(), getPortName());
    }
    ODL_OBJEXIT_B(result); //####
    return result;
} // ChannelEntry::checkConnection

void
ChannelEntry::clearConnectMarker(void)
{
    ODL_ENTER(); //####
    _drawConnectMarker = false;
    ODL_EXIT(); //####
} // ChannelEntry::clearConnectMarker

void
ChannelEntry::clearDisconnectMarker(void)
{
    ODL_ENTER(); //####
    _drawDisconnectMarker = false;
    ODL_EXIT(); //####
} // ChannelEntry::clearDisconnectMarker

void
ChannelEntry::displayAndProcessPopupMenu(void)
{
    ODL_OBJENTER(); //####
    PopupMenu mm;

    mm.setLookAndFeel(&getLookAndFeel());
    _parent->getOwner().getContent()->setUpChannelMenu(mm, *this);
    int result = mm.show();

    switch (result)
    {
        case kPopupAddScrollingMonitor :
            break;

        case kPopupAddSimpleMonitor :
            break;

        case kPopupDetailedDisplayPortInfo :
            displayInformation(isChannel(), true);
            break;

        case kPopupDisplayChannelMetrics :
            displayChannelMetrics();
            break;

        case kPopupDisplayPortInfo :
            displayInformation(isChannel(), false);
            break;

        default :
            break;

    }
    ODL_OBJEXIT(); //####
} // ChannelEntry::displayAndProcessPopupMenu

void
ChannelEntry::displayChannelMetrics(void)
{
    ODL_ENTER(); //####
    StringArray metricsArray;

    if (_parent)
    {
        metricsArray = _parent->getMetrics();
        // Find our line and prepare it.
        int numRows = metricsArray.size();

        for (int ii = 0; ii < numRows; ++ii)
        {
            const String & aRow = metricsArray[ii];
            int            firstTab = aRow.indexOfChar('\t');

            if (0 < firstTab)
            {
                String channelName(aRow.substring(0, firstTab));

                if (channelName == getPortName().c_str())
                {
                    DisplayInformationPanel(this, _parent->formatMetricRow(aRow) + "\n",
                                            String("Metrics for ") + getPortName().c_str());
                    break;
                }

            }
        }
    }
    ODL_EXIT(); //####
} // ChannelEntry::displayChannelMetrics

void
ChannelEntry::displayInformation(const bool isChannel,
                                 const bool moreDetails)
{
    ODL_ENTER(); //####
    ODL_B2("isChannel = ", isChannel, "moreDetails = ", moreDetails); //####
    YarpString dirText;
    YarpString prefix;
    YarpString suffix;

    switch (_direction)
    {
        case kPortDirectionInput :
            dirText = " input";
            break;

        case kPortDirectionOutput :
            dirText = " output";
            break;

        case kPortDirectionInputOutput :
            dirText = " input/ouput";
            break;

        default :
            break;

    }
    switch (Utilities::GetPortKind(getPortName()))
    {
        case Utilities::kPortKindAdapter :
            prefix = "Adapter";
            break;

        case Utilities::kPortKindClient :
            prefix = "Client";
            break;

        case Utilities::kPortKindRegistryService :
            prefix = "Registry Service";
            break;

        case Utilities::kPortKindService :
            prefix = "Service";
            break;

        case Utilities::kPortKindStandard :
            prefix = "Standard";
            break;

        default :
            break;

    }
    if (0 < getProtocol().length())
    {
        suffix = YarpString("\n\nProtocol = '") + getProtocol() + "'";
        if (moreDetails)
        {
            if (0 < getProtocolDescription().length())
            {
                suffix += "\n";
                suffix += getProtocolDescription().c_str();
            }
        }
    }
    YarpString bodyText("Port: ");

    bodyText += getPortNumber() + "\n";
    bodyText += prefix + dirText + (isChannel ? " channel" : " port") + suffix;
    DisplayInformationPanel(this, bodyText.c_str(), getPortName().c_str());
    ODL_EXIT(); //####
} // ChannelEntry::displayInformation

void
ChannelEntry::drawDragLine(Graphics &       gg,
                           const Position & position,
                           const bool       isUDP,
                           const bool       isForced)
{
    ODL_ENTER(); //####
    ODL_B2("isUDP = ", isUDP, "isForced = ", isForced); //####
    AnchorSide sourceAnchor;
    AnchorSide destinationAnchor;
    Position   sourceCentre(getCentre() + getPositionInPanel());
    Position   startPoint;
    Position   destinationCentre;

    // Check if the destination is above the source, in which case we determine the anchors in
    // the reverse order.
    if (sourceCentre.getY() < position.getY())
    {
        sourceAnchor = calculateClosestAnchor(startPoint, true, false, position);
        destinationAnchor = calculateAnchorForPoint(destinationCentre,
                                                    kAnchorBottomCentre == sourceAnchor, position,
                                                    sourceCentre);
    }
    else
    {
        destinationAnchor = calculateAnchorForPoint(destinationCentre, false, position,
                                                    sourceCentre);
        sourceAnchor = calculateClosestAnchor(startPoint, true,
                                              kAnchorBottomCentre == destinationAnchor, position);
    }
    ODL_D4("startPoint.x <- ", startPoint.getX(), "startPoint.y <- ", //####
           startPoint.getY(), "position.x <- ", position.getX(), "position.y <- ", //####
           position.getY()); //####
    if (isUDP)
    {
        gg.setColour(kUdpConnectionColour);
    }
    else
    {
        gg.setColour(kTcpConnectionColour);
    }
    drawBezier(gg, startPoint, position, sourceCentre, destinationCentre, kNormalConnectionWidth,
               isForced);
    drawSourceAnchor(gg, sourceAnchor, startPoint, 1);
    drawTargetAnchor(gg, destinationAnchor, position, 1);
    ODL_EXIT(); //####
} // ChannelEntry::drawDragLine

void
ChannelEntry::drawOutgoingConnections(Graphics & gg)
{
    ODL_OBJENTER(); //####
    ODL_P1("gg = ", &gg); //####
    bool selfIsVisible = _parent->isVisible();

    for (ChannelConnections::const_iterator walker(_outputConnections.begin());
         _outputConnections.end() != walker; ++walker)
    {
        const ChannelInfo * candidate(&*walker);

        if (candidate)
        {
            ChannelEntry * otherChannelEntry = candidate->_otherChannel;
            bool           otherIsVisible = otherChannelEntry->_parent->isVisible();

            if (selfIsVisible && otherIsVisible)
            {
                drawConnection(gg, this, candidate->_otherChannel, candidate->_connectionMode,
                               candidate->_forced);
            }
        }
    }
    ODL_OBJEXIT(); //####
} // ChannelEntry::drawOutgoingConnections

Position
ChannelEntry::getCentre(void)
const
{
    ODL_OBJENTER(); //####
    juce::Rectangle<float> outer(getLocalBounds().toFloat());

    ODL_OBJEXIT(); //####
    return outer.getCentre();
} // ChannelEntry::getCentre

EntitiesPanel &
ChannelEntry::getOwningPanel(void)
const
{
    ODL_OBJENTER(); //####
    EntitiesPanel & result(_parent->getOwner());

    ODL_OBJEXIT_P(&result); //####
    return result;
} // ChannelEntry::getOwningPanel

Position
ChannelEntry::getPositionInPanel(void)
const
{
    ODL_OBJENTER(); //####
    Position result(getPosition().toFloat() + _parent->getPositionInPanel());

    ODL_OBJEXIT(); //####
    return result;
} // ChannelEntry::getPositionInPanel

bool
ChannelEntry::hasOutgoingConnectionTo(const YarpString & otherPort)
const
{
    ODL_OBJENTER(); //####
    ODL_S1s("otherPort = ", otherPort); //####
    bool result = false;

    for (ChannelConnections::const_iterator walker(_outputConnections.begin());
         _outputConnections.end() != walker; ++walker)
    {
        const ChannelInfo * candidate(&*walker);

        if (candidate && candidate->_otherChannel &&
            (candidate->_otherChannel->getPortName() == otherPort))
        {
            result = true;
            break;
        }

    }
    ODL_OBJEXIT_B(result); //####
    return result;
} // ChannelEntry::hasOutgoingConnectionTo

void
ChannelEntry::invalidateConnections(void)
{
    ODL_OBJENTER(); //####
    for (ChannelConnections::iterator walker(_inputConnections.begin());
         _inputConnections.end() != walker; ++walker)
    {
        ChannelInfo * candidate(&*walker);

        if (candidate)
        {
            candidate->_valid = false;
        }
    }
    for (ChannelConnections::iterator walker(_outputConnections.begin());
         _outputConnections.end() != walker; ++walker)
    {
        ChannelInfo * candidate(&*walker);

        if (candidate)
        {
            candidate->_valid = false;
        }
    }
    ODL_OBJEXIT(); //####
} // ChannelEntry::invalidateConnections

bool
ChannelEntry::isChannel(void)
const
{
    ODL_ENTER(); //####
    bool result = false;

    if (_parent)
    {
        ContainerKind parentKind = _parent->getKind();

        if ((kContainerKindAdapter == parentKind) || (kContainerKindService == parentKind))
        {
            result = true;
        }
    }
    ODL_EXIT_B(result); //####
    return result;
} // ChannelEntry::isChannel

void
ChannelEntry::mouseDown(const MouseEvent & ee)
{
    ODL_OBJENTER(); //####
    bool            passOn = true;
    EntitiesPanel & owningPanel(getOwningPanel());
    ChannelEntry *  firstAddPort = owningPanel.getFirstAddPoint();
    ChannelEntry *  firstRemovePort = owningPanel.getFirstRemovePoint();

    if (firstRemovePort)
    {
        // We started a 'remove' operation.
        clearDisconnectMarker();
        repaint();
        owningPanel.rememberConnectionStartPoint();
        passOn = false;
        if (firstRemovePort != this)
        {
            // Check if we can end here.
            YarpString firstName(firstRemovePort->getPortName());

            // Check if we can end here.
            firstRemovePort->clearDisconnectMarker();
            firstRemovePort->repaint();
            if ((kPortDirectionOutput != _direction) && (kPortUsageService != _usage) &&
                firstRemovePort->hasOutgoingConnectionTo(getPortName()))
            {
                if (Utilities::RemoveConnection(firstName, getPortName(), CheckForExit))
                {
                    firstRemovePort->removeOutputConnection(this);
                    removeInputConnection(firstRemovePort);
                    owningPanel.skipScan();
                    owningPanel.repaint();
                }
            }
        }
    }
    else if (firstAddPort)
    {
        // We started an 'add' operation.
        clearConnectMarker();
        repaint();
        owningPanel.rememberConnectionStartPoint();
        passOn = false;
        if (firstAddPort != this)
        {
            // Check if we can end here.
            bool       protocolsOverridden = ee.mods.isCtrlDown();
            YarpString firstName(firstAddPort->getPortName());
            YarpString firstProtocol(firstAddPort->getProtocol());

            // Check if we can end here.
            firstAddPort->clearConnectMarker();
            firstAddPort->repaint();
            if ((kPortDirectionOutput != _direction) && (kPortUsageService != _usage) &&
                protocolsMatch(firstProtocol, _portProtocol, protocolsOverridden) &&
                (! firstAddPort->hasOutgoingConnectionTo(getPortName())))
            {
                if (Utilities::AddConnection(firstName, getPortName(), STANDARD_WAIT_TIME_,
                                             firstAddPort->_wasUdp, CheckForExit))
                {
                    Common::ChannelMode mode = (firstAddPort->_wasUdp ? Common::kChannelModeUDP :
                                                Common::kChannelModeTCP);

                    firstAddPort->addOutputConnection(this, mode, protocolsOverridden);
                    addInputConnection(firstAddPort, mode, protocolsOverridden);
                    owningPanel.skipScan();
                    owningPanel.repaint();
                }
            }
        }
    }
    else
    {
        // No active operation.
        if (ee.mods.isAltDown())
        {
            ODL_P2("originalComponent = ", ee.originalComponent, //####
                   "eventComponent = ", ee.eventComponent); //####
            ODL_D2("x = ", ee.position.getX(), "y = ", ee.position.getY()); //####
            // Check if Add is OK for this entry.
            if ((kPortDirectionInput != _direction) && (kPortUsageClient != _usage))
            {
                _wasUdp = ee.mods.isShiftDown();
                owningPanel.rememberConnectionStartPoint(this, true);
                setConnectMarker();
                repaint();
            }
            passOn = false;
        }
        else if (ee.mods.isCommandDown())
        {
            // Check if Remove is OK for this entry.
            if ((kPortDirectionInput != _direction) && (kPortUsageClient != _usage) &&
                (0 < _outputConnections.size()))
            {
                owningPanel.rememberConnectionStartPoint(this, false);
                setDisconnectMarker();
                repaint();
            }
            passOn = false;
        }
        else if (ee.mods.isPopupMenu())
        {
            _parent->getOwner().getContent()->setChannelOfInterest(this);
            displayAndProcessPopupMenu();
            passOn = false;
        }
        else
        {
            _parent->getOwner().getContent()->setChannelOfInterest(this);
        }
    }
    if (passOn)
    {
        _parent->mouseDown(ee);
    }
    ODL_OBJEXIT(); //####
} // ChannelEntry::mouseDown

void
ChannelEntry::mouseDrag(const MouseEvent & ee)
{
    ODL_OBJENTER(); //####
    bool passOn = true;

    if (ee.mods.isAltDown())
    {
        ODL_P2("originalComponent = ", ee.originalComponent, //####
               "eventComponent = ", ee.eventComponent); //####
        ODL_D2("x = ", ee.position.getX(), "y = ", ee.position.getY()); //####
        EntitiesPanel & owningPanel(getOwningPanel());

        owningPanel.setDragInfo(getPositionInPanel() + ee.position, ee.mods.isCtrlDown());
        owningPanel.repaint();
        passOn = false;
    }
    else if (ee.mods.isCommandDown() || ee.mods.isPopupMenu())
    {
        passOn = false;
    }
    if (passOn)
    {
        _parent->mouseDrag(ee);
    }
    ODL_OBJEXIT(); //####
} // ChannelEntry::mouseDrag

void
ChannelEntry::mouseUp(const MouseEvent & ee)
{
    ODL_OBJENTER(); //####
    bool            passOn = true;
    EntitiesPanel & owningPanel(getOwningPanel());

    if (ee.mods.isAltDown() || owningPanel.isDragActive())
    {
        // Check if we are processing an Add and this is OK.
        ODL_P2("originalComponent = ", ee.originalComponent, //####
               "eventComponent = ", ee.eventComponent); //####
        ODL_D2("x = ", ee.position.getX(), "y = ", ee.position.getY()); //####
        if (owningPanel.isDragActive())
        {
            Position       newLocation(getPositionInPanel() + ee.position);
            ChannelEntry * endEntry = owningPanel.locateEntry(newLocation);

            clearConnectMarker();
            repaint();
            if (endEntry && (endEntry != this))
            {
                // Check if we can end here.
                bool       protocolsOverridden = ee.mods.isCtrlDown();
                YarpString secondName(endEntry->getPortName());
                YarpString secondProtocol(endEntry->getProtocol());

                if ((kPortDirectionOutput != endEntry->getDirection()) &&
                    (kPortUsageService != endEntry->getUsage()) &&
                    protocolsMatch(getProtocol(), secondProtocol, protocolsOverridden) &&
                    (! hasOutgoingConnectionTo(secondName)))
                {
                    if (Utilities::AddConnection(getPortName(), secondName, STANDARD_WAIT_TIME_,
                                                 _wasUdp, CheckForExit))
                    {
                        Common::ChannelMode mode = (_wasUdp ? Common::kChannelModeUDP :
                                                    Common::kChannelModeTCP);

                        addOutputConnection(endEntry, mode, protocolsOverridden);
                        endEntry->addInputConnection(this, mode, protocolsOverridden);
                        owningPanel.skipScan();
                    }
                }
            }
            owningPanel.rememberConnectionStartPoint();
            owningPanel.clearDragInfo();
            owningPanel.repaint();
        }
        passOn = false;
    }
    else if (ee.mods.isCommandDown() || ee.mods.isPopupMenu())
    {
        passOn = false;
    }
    if (passOn)
    {
        _parent->mouseUp(ee);
    }
    ODL_OBJEXIT(); //####
} // ChannelEntry::mouseUp

void
ChannelEntry::paint(Graphics & gg)
{
    ODL_OBJENTER(); //####
    ODL_P1("gg = ", &gg); //####
    AttributedString as;

    as.setJustification(Justification::left);
    as.append(_title.c_str(), getOwningPanel().getNormalFont(), kEntryTextColour);
    juce::Rectangle<float> area(getLocalBounds().toFloat());

    ODL_D4("x <- ", area.getX(), "y <- ", area.getY(), "w <- ",area.getWidth(), "h <- ", //####
           area.getHeight()); //####
    gg.setColour(kEntryBackgroundColour);
    gg.fillRect(area);
    area.setLeft(area.getX() + _parent->getTextInset());
    as.draw(gg, area);
    if (_drawConnectMarker)
    {
        Position markerPos(getCentre() - Position(kMarkerSide / 2, kMarkerSide / 2));

        gg.setColour(kMarkerColour);
        gg.fillEllipse(markerPos.getX(), markerPos.getY(), kMarkerSide, kMarkerSide);
    }
    else if (_drawDisconnectMarker)
    {
        Position markerPos(getCentre() - Position(kMarkerSide / 2, kMarkerSide / 2));

        gg.setColour(kMarkerColour);
        gg.drawEllipse(markerPos.getX(), markerPos.getY(), kMarkerSide, kMarkerSide, 2);
    }
    if (_drawActivityMarker)
    {
        float          hh = getHeight() - (2 * kActivityInset);
        float          halfSize = static_cast<float>(hh / 2);
        Position       markerPos(getWidth() - static_cast<float>(hh + kActivityInset),
                                 kActivityInset);
        Position       markerCentre(markerPos + Position(halfSize, halfSize));
        ColourGradient theGradient(kFirstActivityMarkerColour, markerCentre.getX(),
                                   markerCentre.getY(), kSecondActivityMarkerColour,
                                   markerCentre.getX() + halfSize, markerCentre.getY(), true);
        FillType       theMarkerFill(theGradient);

        gg.setFillType(theMarkerFill);
        gg.fillEllipse(markerPos.getX(), markerPos.getY(), hh, hh);
    }
    ODL_OBJEXIT(); //####
} // ChannelEntry::paint

void
ChannelEntry::removeAllConnections(void)
{
    ODL_OBJENTER(); //####
    for (ChannelConnections::iterator walker(_inputConnections.begin());
         _inputConnections.end() != walker; ++walker)
    {
        ChannelInfo * candidate(&*walker);

        if (candidate && candidate->_otherChannel)
        {
            candidate->_otherChannel->removeOutputConnection(this);
        }
    }
    _inputConnections.clear();
    for (ChannelConnections::iterator walker(_outputConnections.begin());
         _outputConnections.end() != walker; ++walker)
    {
        ChannelInfo * candidate(&*walker);

        if (candidate && candidate->_otherChannel)
        {
            candidate->_otherChannel->removeInputConnection(this);
        }
    }
    _outputConnections.clear();
    ODL_EXIT(); //####
} // ChannelEntry::removeAllConnections

void
ChannelEntry::removeInputConnection(ChannelEntry * other)
{
    ODL_OBJENTER(); //####
    ODL_P1("other = ", other); //####
    if (other)
    {
        ChannelConnections::iterator walker(_inputConnections.begin());

        for ( ; _inputConnections.end() != walker; ++walker)
        {
            ChannelInfo * candidate(&*walker);

            if (candidate && (candidate->_otherChannel == other))
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
} // ChannelEntry::removeInputConnection

void
ChannelEntry::removeInvalidConnections(void)
{
    ODL_OBJENTER(); //####
    bool keepGoing;

    do
    {
        ChannelConnections::iterator walker(_inputConnections.begin());

        keepGoing = false;
        for ( ; _inputConnections.end() != walker; ++walker)
        {
            ChannelInfo * candidate(&*walker);

            if (candidate && (! candidate->_valid))
            {
                break;
            }

        }
        if (_inputConnections.end() != walker)
        {
            // Double-check the connection - if YARP says it's still there, don't delete it!
            if (checkConnection(*walker, false))
            {
                walker->_valid = true;
            }
            else
            {
                _inputConnections.erase(walker);
            }
            keepGoing = true;
        }
    }
    while (keepGoing);
    do
    {
        ChannelConnections::iterator walker(_outputConnections.begin());

        keepGoing = false;
        for ( ; _outputConnections.end() != walker; ++walker)
        {
            ChannelInfo * candidate(&*walker);

            if (candidate && (! candidate->_valid))
            {
                break;
            }

        }
        if (_outputConnections.end() != walker)
        {
            // Double-check the connection - if YARP says it's still there, don't delete it!
            if (checkConnection(*walker, true))
            {
                walker->_valid = true;
            }
            else
            {
                _outputConnections.erase(walker);
            }
            keepGoing = true;
        }
    }
    while (keepGoing);
    ODL_EXIT(); //####
} // ChannelEntry::removeInvalidConnections

void
ChannelEntry::removeOutputConnection(ChannelEntry * other)
{
    ODL_OBJENTER(); //####
    ODL_P1("other = ", other); //####
    if (other)
    {
        ChannelConnections::iterator walker(_outputConnections.begin());

        for ( ; _outputConnections.end() != walker; ++walker)
        {
            const ChannelInfo * candidate(&*walker);

            if (candidate && (candidate->_otherChannel == other))
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
} // ChannelEntry::removeOutputConnection

void
ChannelEntry::setAsLastPort(void)
{
    ODL_OBJENTER(); //####
    _isLastPort = true;
    ODL_OBJEXIT(); //####
} // ChannelEntry::setAsLastPort

void
ChannelEntry::setConnectMarker(void)
{
    ODL_OBJENTER(); //####
    _drawConnectMarker = true;
    ODL_OBJEXIT(); //####
} // ChannelEntry::setConnectMarker

void
ChannelEntry::setDisconnectMarker(void)
{
    ODL_OBJENTER(); //####
    _drawDisconnectMarker = true;
    ODL_OBJEXIT(); //####
} // ChannelEntry::setDisconnectMarker

void
ChannelEntry::unsetAsLastPort(void)
{
    ODL_OBJENTER(); //####
    _isLastPort = false;
    ODL_OBJEXIT(); //####
} // ChannelEntry::unsetAsLastPort

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
