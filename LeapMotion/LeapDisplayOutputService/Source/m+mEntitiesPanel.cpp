//--------------------------------------------------------------------------------------------------
//
//  File:       m+mEntitiesPanel.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the entities layer of the primary window of the m+m
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

#include "m+mEntitiesPanel.hpp"

#include "m+mChannelContainer.hpp"
#include "m+mChannelEntry.hpp"
#include "m+mContentPanel.hpp"
#include "m+mFormField.hpp"
#include "m+mManagerWindow.hpp"

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wc++11-extensions"
# pragma clang diagnostic ignored "-Wdocumentation"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# pragma clang diagnostic ignored "-Wpadded"
# pragma clang diagnostic ignored "-Wshadow"
# pragma clang diagnostic ignored "-Wunused-parameter"
# pragma clang diagnostic ignored "-Wweak-vtables"
#endif // defined(__APPLE__)
#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4996)
# pragma warning(disable: 4458)
#endif // ! MAC_OR_LINUX_
#include <yarp/os/Network.h>
#include <yarp/os/Port.h>
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file

 @brief The class definition for the entities layer of the primary window of the m+mLeapDisplayOutputService
 application. */
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

/*! @brief The name of the font to be used for text. */
static const char * kFontName = "Courier New";

/*! @brief The outer 'gutter' for the entities. */
static const int kGutter = 10;

/*! @brief The initial height of the displayed region. */
static const int kInitialPanelHeight = 768;

/*! @brief The initial width of the displayed region. */
static const int kInitialPanelWidth = 1024;

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

EntitiesPanel::EntitiesPanel(ContentPanel * theContainer,
                             const int      startingWidth,
                             const int      startingHeight) :
    inherited(), _knownPorts(), _knownEntities(), _defaultBoldFont(), _defaultNormalFont(),
    _firstAddPoint(NULL), _firstRemovePoint(NULL), _container(theContainer),
    _dragConnectionActive(false), _dragIsForced(false)
{
    ODL_ENTER(); //####
    _defaultBoldFont = new Font(kFontName, FormField::kFontSize - 1, Font::bold);
    _defaultNormalFont = new Font(kFontName, FormField::kFontSize - 1, Font::plain);
    setSize(startingWidth ? startingWidth : kInitialPanelWidth,
            startingHeight ? startingHeight : kInitialPanelHeight);
    setVisible(true);
    ODL_EXIT_P(this); //####
} // EntitiesPanel::EntitiesPanel

EntitiesPanel::~EntitiesPanel(void)
{
    ODL_OBJENTER(); //####
    clearOutData();
    _defaultBoldFont = NULL;
    _defaultNormalFont = NULL;
    ODL_OBJEXIT(); //####
} // EntitiesPanel::~EntitiesPanel

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void
EntitiesPanel::addEntity(ChannelContainer * anEntity)
{
    ODL_OBJENTER(); //####
    ODL_P1("anEntity = ", anEntity); //####
    char buffer1[DATE_TIME_BUFFER_SIZE_];
    char buffer2[DATE_TIME_BUFFER_SIZE_];

    Utilities::GetDateAndTime(buffer1, sizeof(buffer1), buffer2, sizeof(buffer2));
    std::cerr << buffer1 << " " << buffer2 << " Adding entity " << anEntity->getName() << std::endl;
    _knownEntities.push_back(anEntity);
    addChildComponent(anEntity);
    ODL_OBJEXIT(); //####
} // EntitiesPanel::addEntity

void
EntitiesPanel::adjustSize(const bool andRepaint)
{
    ODL_OBJENTER(); //####
    ODL_B1("andRepaint = ", andRepaint); //####
    ContentPanel * within = getContent();

    ODL_P1("within <- ", within); //####
    if (within)
    {
        ODL_LOG("(within)"); //####
        int  outerW = within->getMaximumVisibleWidth();
        int  outerH = within->getMaximumVisibleHeight();
        int  outerL = within->getViewPositionX();
        int  outerT = within->getViewPositionY();
        int  minX = -1;
        int  maxX = -1;
        int  minY = -1;
        int  maxY = -1;
        bool haveValues = false;

        ODL_LL4("outerL = ", outerL, "outerT = ", outerT, "outerW = ", outerW, //####
                "outerH = ", outerH); //####
        for (ContainerList::const_iterator it(_knownEntities.begin());
             _knownEntities.end() != it; ++it)
        {
            ChannelContainer * anEntity = *it;

            if (anEntity)
            {
                juce::Rectangle<int> entityBounds(anEntity->getBounds());
                int                  entityLeft = entityBounds.getX();
                int                  entityTop = entityBounds.getY();
                int                  entityRight = entityLeft + entityBounds.getWidth();
                int                  entityBottom = entityTop + entityBounds.getHeight();

                ODL_LL4("eB.x = ", entityBounds.getX(), "eB.y = ", entityBounds.getY(), //####
                        "eB.w = ", entityBounds.getWidth(), "eB.h = ", //####
                        entityBounds.getHeight()); //####
                if (haveValues)
                {
                    minX = jmin(minX, entityLeft);
                    maxX = jmax(maxX, entityRight);
                    minY = jmin(minY, entityTop);
                    maxY = jmax(maxY, entityBottom);
                }
                else
                {
                    minX = entityLeft;
                    maxX = entityRight;
                    minY = entityTop;
                    maxY = entityBottom;
                    haveValues = true;
                }
            }
        }
        if (haveValues)
        {
            ODL_LOG("(haveValues)"); //####
            ODL_LL4("minX = ", minX, "maxX = ", maxX, "minY = ", minY, "maxY = ", maxY); //####
            juce::Rectangle<int> oldBounds(getBounds());
            int                  minLeft = jmin(0, minX);
            int                  maxRight = jmax(jmax(0, maxX + kGutter), minLeft + outerW);
            int                  minTop = jmin(0, minY);
            int                  maxBottom = jmax(jmax(0, maxY + kGutter), minTop + outerH);
            juce::Rectangle<int> newBounds(minLeft, minTop, maxRight - minLeft, maxBottom - minTop);

            ODL_LL4("minLeft = ", minLeft, "minTop = ", minTop, "maxRight = ", maxRight, //####
                    "maxBottom = ", maxBottom); //####
            if (oldBounds != newBounds)
            {
                ODL_LOG("about to call setBounds()"); //####
                setBounds(newBounds);
            }
            ScrollBar *          horizBar = within->getHorizontalScrollBar();
            ScrollBar *          vertBar = within->getVerticalScrollBar();
            juce::Rectangle<int> currBounds(getBounds());
            int                  currX = currBounds.getX();
            int                  currY = currBounds.getY();
            int                  currW = currBounds.getWidth();
            int                  currH = currBounds.getHeight();

            ODL_LL4("currX = ", currX, "currY = ", currY, "currW = ", currW, "currH = ", //####
                    currH); //####
            if (vertBar)
            {
                ODL_LL2("CR.y = ", outerT, "CR.h = ", outerH); //####
                Range<double> currLimits(vertBar->getRangeLimit());
                Range<double> currRange(vertBar->getCurrentRange());
                Range<double> newLimits(currY, currY + currH);
                Range<double> newRange(outerT, outerH);

                if (currLimits != newLimits)
                {
                    vertBar->setRangeLimits(newLimits);
                }
                if (currRange != newRange)
                {
                    vertBar->setCurrentRange(newRange);
                }
            }
            if (horizBar)
            {
                ODL_LL2("CR.x = ", outerL, "CR.w = ", outerW); //####
                Range<double> currLimits(horizBar->getRangeLimit());
                Range<double> currRange(horizBar->getCurrentRange());
                Range<double> newLimits(currX, currX + currW);
                Range<double> newRange(outerL, outerW);

                if (currLimits != newLimits)
                {
                    horizBar->setRangeLimits(newLimits);
                }
                if (currRange != newRange)
                {
                    horizBar->setCurrentRange(newRange);
                }
            }
            within->setViewPosition(outerL, outerT);
            if (andRepaint)
            {
                within->repaint();
            }
        }
    }
    ODL_OBJEXIT(); //####
} // EntitiesPanel::adjustSize

void
EntitiesPanel::clearAllNewlyCreatedFlags(void)
{
    ODL_OBJENTER(); //####
    for (ContainerList::const_iterator it(_knownEntities.begin()); _knownEntities.end() != it; ++it)
    {
        ChannelContainer * anEntity = *it;

        if (anEntity)
        {
            anEntity->setOld();
        }
    }
    ODL_OBJEXIT(); //####
} // EntitiesPanel::clearAllNewlyCreatedFlags

void
EntitiesPanel::clearAllVisitedFlags(void)
{
    ODL_OBJENTER(); //####
    for (ContainerList::const_iterator it(_knownEntities.begin()); _knownEntities.end() != it; ++it)
    {
        ChannelContainer * anEntity = *it;

        if (anEntity)
        {
            anEntity->clearVisited();
        }
    }
    ODL_OBJEXIT(); //####
} // EntitiesPanel::clearAllVisitedFlags

void
EntitiesPanel::clearDragInfo(void)
{
    ODL_OBJENTER(); //####
    _dragConnectionActive = _dragIsForced = false;
    ODL_OBJEXIT(); //####
} // EntitiesPanel::clearDragInfo

void
EntitiesPanel::clearMarkers(void)
{
    ODL_OBJENTER(); //####
    for (ContainerList::const_iterator it(_knownEntities.begin()); _knownEntities.end() != it; ++it)
    {
        ChannelContainer * anEntity = *it;

        if (anEntity)
        {
            if (anEntity->isMarked())
            {
                anEntity->clearMarkers();
                anEntity->repaint();
            }
        }
    }
    ODL_OBJEXIT(); //####
} // EntitiesPanel::clearMarkers

#if defined(USE_OGDF_POSITIONING_)
void
EntitiesPanel::clearNodeValues(void)
{
    ODL_OBJENTER(); //####
    for (ContainerList::const_iterator it(_knownEntities.begin()); _knownEntities.end() != it; ++it)
    {
        ChannelContainer * anEntity = *it;

        if (anEntity)
        {
            anEntity->setNode(NULL);
        }
    }

    ODL_OBJEXIT(); //####
} // EntitiesPanel::clearNodeValues
#endif // defined(USE_OGDF_POSITIONING_)

void
EntitiesPanel::clearOutData(void)
{
    ODL_OBJENTER(); //####
    for (ContainerList::const_iterator it(_knownEntities.begin()); _knownEntities.end() != it; ++it)
    {
        ChannelContainer * anEntity = *it;

        if (anEntity)
        {
            delete anEntity;
        }
    }
    removeAllChildren();
    _knownPorts.clear();
    _knownEntities.clear();
    ODL_OBJEXIT(); //####
} // EntitiesPanel::clearOutData

void
EntitiesPanel::displayAndProcessPopupMenu(void)
{
    ODL_OBJENTER(); //####
    if (_container)
    {
        ApplicationCommandManager * commandManager = &ManagerWindow::getApplicationCommandManager();
        PopupMenu                   mm;

        mm.setLookAndFeel(&getLookAndFeel());
        _container->setUpViewMenu(mm);
        mm.addSeparator();
        mm.addCommandItem(commandManager, ManagerWindow::kCommandLaunchRegistryService);
        mm.addCommandItem(commandManager, ManagerWindow::kCommandLaunchExecutables);
        // Note that all the menu items are commands, so they are handled by the content panel.
        mm.show();
    }
    ODL_OBJEXIT(); //####
} // EntitiesPanel::displayAndProcessPopupMenu

void
EntitiesPanel::drawConnections(Graphics & gg)
{
    ODL_OBJENTER(); //####
    ODL_P1("gg = ", &gg); //####
    for (ContainerList::const_iterator it(_knownEntities.begin()); _knownEntities.end() != it; ++it)
    {
        ChannelContainer * anEntity = *it;

        if (anEntity)
        {
            anEntity->drawOutgoingConnections(gg);
        }
    }
    if (_dragConnectionActive && _firstAddPoint)
    {
        _firstAddPoint->drawDragLine(gg, _dragPosition, _firstAddPoint->wasUdpConnectionRequest(),
                                     _dragIsForced);
    }
    ODL_OBJEXIT(); //####
} // EntitiesPanel::drawConnections

ChannelContainer *
EntitiesPanel::findKnownEntity(const YarpString & name)
{
    ODL_OBJENTER(); //####
    ODL_S1s("name = ", name); //####
    ChannelContainer * result = NULL;

    for (ContainerList::const_iterator it(_knownEntities.begin()); _knownEntities.end() != it; ++it)
    {
        ChannelContainer * anEntity = *it;

        if (anEntity)
        {
            YarpString entityName(anEntity->getName().toStdString().c_str());

            if (name == entityName)
            {
                result = anEntity;
                break;
            }

        }
    }
    ODL_OBJEXIT_P(result); //####
    return result;
} // EntitiesPanel::findKnownEntity

ChannelEntry *
EntitiesPanel::findKnownPort(const YarpString & name)
{
    ODL_OBJENTER(); //####
    ODL_S1s("name = ", name); //####
    ChannelEntry *                  result = NULL;
    ChannelEntryMap::const_iterator match(_knownPorts.find(name));

    if (_knownPorts.end() == match)
    {
        result = NULL;
    }
    else
    {
        result = match->second;
    }
    ODL_OBJEXIT_P(result); //####
    return result;
} // EntitiesPanel::findKnownPort

void
EntitiesPanel::forgetPort(ChannelEntry * aPort)
{
    ODL_OBJENTER(); //####
    ODL_P1("aPort = ", aPort); //####
    if (aPort)
    {
        YarpString                aPortName(aPort->getName().toStdString().c_str());
        ChannelEntryMap::iterator match(_knownPorts.find(aPortName));

        if (_knownPorts.end() == match)
        {
            // We couldn't find a match by the key, so instead search by value
            for (match = _knownPorts.begin(); _knownPorts.end() != match; ++match)
            {
                if (match->second == aPort)
                {
                    _knownPorts.erase(match);
                    break;
                }

            }
        }
        else
        {
            _knownPorts.erase(match);
        }
    }
    ODL_OBJEXIT(); //####
} // EntitiesPanel::forgetPort

ChannelContainer *
EntitiesPanel::getEntity(const size_t index)
const
{
    ODL_OBJENTER(); //####
    ODL_LL1("index = ", index); //####
    ChannelContainer * result;

    if (_knownEntities.size() > index)
    {
        result = _knownEntities.at(index);
    }
    else
    {
        result = NULL;
    }
    ODL_OBJEXIT_P(result); //####
    return result;
} // EntitiesPanel::getEntity

size_t
EntitiesPanel::getNumberOfEntities(void)
const
{
    ODL_OBJENTER(); //####
    size_t result = _knownEntities.size();

    ODL_OBJEXIT_LL(result); //####
    return result;
} // EntitiesPanel::getNumberOfEntities

size_t
EntitiesPanel::getNumberOfHiddenEntities(void)
const
{
    ODL_OBJENTER(); //####
    size_t count = 0;

    for (ContainerList::const_iterator it(_knownEntities.begin()); _knownEntities.end() != it; ++it)
    {
        ChannelContainer * anEntity = *it;

        if (anEntity && (! anEntity->isVisible()))
        {
            ++count;
        }
    }
    ODL_OBJEXIT_LL(count); //####
    return count;
} // EntitiesPanel::getNumberOfHiddenEntities

void
EntitiesPanel::invalidateAllConnections(void)
{
    ODL_OBJENTER(); //####
    for (ContainerList::const_iterator it(_knownEntities.begin()); _knownEntities.end() != it; ++it)
    {
        ChannelContainer * anEntity = *it;

        if (anEntity)
        {
            anEntity->invalidateConnections();
        }
    }
    ODL_OBJEXIT(); //####
} // EntitiesPanel::invalidateAllConnections

ChannelEntry *
EntitiesPanel::locateEntry(const Position & location)
const
{
    ODL_OBJENTER(); //####
    ChannelEntry * result = NULL;

    for (ContainerList::const_iterator it(_knownEntities.begin());
         (! result) && (_knownEntities.end() != it); ++it)
    {
        ChannelContainer * anEntity = *it;

        if (anEntity)
        {
            result = anEntity->locateEntry(location);
        }
    }
    ODL_OBJEXIT_P(result); //####
    return result;
} // EntitiesPanel::locateEntry

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
void
EntitiesPanel::mouseDown(const MouseEvent & ee)
{
#if (! defined(OD_ENABLE_LOGGING_))
# if MAC_OR_LINUX_
#  pragma unused(ee)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING_)
    ODL_OBJENTER(); //####
    ODL_P1("ee = ", &ee); //####
    _container->setChannelOfInterest(NULL);
    _container->setContainerOfInterest(NULL);
    rememberConnectionStartPoint();
    clearMarkers();
    repaint();
    ODL_OBJEXIT(); //####
} // EntitiesPanel::mouseDown
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
void
EntitiesPanel::mouseUp(const MouseEvent & ee)
{
#if (! defined(OD_ENABLE_LOGGING_))
# if MAC_OR_LINUX_
#  pragma unused(ee)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING_)
    ODL_OBJENTER(); //####
    ODL_P1("ee = ", &ee); //####
    rememberConnectionStartPoint();
    clearMarkers();
    if (ee.mods.isPopupMenu())
    {
        displayAndProcessPopupMenu();
    }
    ODL_OBJEXIT(); //####
} // EntitiesPanel::mouseUp
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

void
EntitiesPanel::paint(Graphics & gg)
{
    ODL_OBJENTER(); //####
    ODL_P1("gg = ", &gg); //####
    drawConnections(gg);
    ODL_OBJEXIT(); //####
} // EntitiesPanel::paint

void
EntitiesPanel::recallPositions(void)
{
    ODL_OBJENTER(); //####
    _container->recallEntityPositions();
    ODL_OBJEXIT(); //####
} // EntitiesPanel::recallPositions

void
EntitiesPanel::rememberConnectionStartPoint(ChannelEntry * aPort,
                                            const bool     beingAdded)
{
    ODL_OBJENTER(); //####
    ODL_P1("aPort = ", aPort); //####
    ODL_B1("beingAdded = ", beingAdded); //####
    if (beingAdded)
    {
        _firstAddPoint = aPort;
        _firstRemovePoint = NULL;
    }
    else
    {
        _firstAddPoint = NULL;
        _firstRemovePoint = aPort;
    }
    ODL_OBJEXIT(); //####
} // EntitiesPanel::rememberConnectionStartPoint

void
EntitiesPanel::rememberPort(ChannelEntry * aPort)
{
    ODL_OBJENTER(); //####
    ODL_P1("aPort = ", aPort); //####
    if (aPort)
    {
        _knownPorts.insert(ChannelEntryMap::value_type(aPort->getPortName(), aPort));
    }
    ODL_OBJEXIT(); //####
} // EntitiesPanel::rememberPort

void
EntitiesPanel::rememberPositions(void)
{
    ODL_OBJENTER(); //####
    for (ContainerList::const_iterator it(_knownEntities.begin()); _knownEntities.end() != it; ++it)
    {
        ChannelContainer * anEntity = *it;

        if (anEntity)
        {
            _container->rememberPositionOfEntity(anEntity);
        }
    }
    _container->saveEntityPositions();
    ODL_OBJEXIT(); //####
} // EntitiesPanel::rememberPositions

void
EntitiesPanel::removeInvalidConnections(void)
{
    ODL_OBJENTER(); //####
    for (ContainerList::iterator it(_knownEntities.begin()); _knownEntities.end() != it; ++it)
    {
        ChannelContainer * anEntity = *it;

        if (anEntity)
        {
            anEntity->removeInvalidConnections();
        }
    }
    ODL_OBJEXIT(); //####
} // EntitiesPanel::removeInvalidConnections

bool
EntitiesPanel::removeUnvisitedEntities(void)
{
    ODL_OBJENTER(); //####
    bool didRemove = false;
    bool keepGoing;

    do
    {
        keepGoing = false;
        ContainerList::iterator walker(_knownEntities.begin());
        ChannelContainer *      anEntity = NULL;

        for ( ; _knownEntities.end() != walker; ++walker)
        {
            anEntity = *walker;
            if (anEntity && (! anEntity->wasVisited()))
            {
                char buffer1[DATE_TIME_BUFFER_SIZE_];
                char buffer2[DATE_TIME_BUFFER_SIZE_];

                Utilities::GetDateAndTime(buffer1, sizeof(buffer1), buffer2, sizeof(buffer2));
                std::cerr << buffer1 << " " << buffer2 << " Removing unvisited entity " <<
                            anEntity->getName() << std::endl;
                break;
            }

        }
        if ((_knownEntities.end() != walker) && anEntity)
        {
            ODL_LOG("((_knownEntities.end() != walker) && anEntity)"); //####
            _container->rememberPositionOfEntity(anEntity);
            removeChildComponent(anEntity);
            delete anEntity;
            _knownEntities.erase(walker);
            keepGoing = didRemove = true;
        }
    }
    while (keepGoing);
    ODL_OBJEXIT_B(didRemove); //####
    return didRemove;
} // EntitiesPanel::removeUnvisitedEntities

void
EntitiesPanel::resized(void)
{
    ODL_OBJENTER(); //####
    ODL_LOG("about to call adjustSize()"); //####
    adjustSize(true);
    ODL_OBJEXIT(); //####
} // EntitiesPanel::resized

void
EntitiesPanel::setDragInfo(const Position position,
                           const bool     isForced)
{
    ODL_OBJENTER(); //####
    ODL_B1("isForced = ", isForced); //####
    if (_firstAddPoint)
    {
        _dragConnectionActive = true;
        _dragIsForced = isForced;
        _dragPosition = position;
    }
    ODL_OBJEXIT(); //####
} // EntitiesPanel::setDragInfo

void
EntitiesPanel::skipScan(void)
{
    ODL_OBJENTER(); //####
    if (_container)
    {
        _container->skipScan();
    }
    ODL_OBJEXIT(); //####
} // EntitiesPanel::skipScan

void
EntitiesPanel::unhideEntities(void)
{
    ODL_OBJENTER(); //####
    for (ContainerList::const_iterator it(_knownEntities.begin()); _knownEntities.end() != it; ++it)
    {
        ChannelContainer * anEntity = *it;

        if (anEntity && (! anEntity->isVisible()))
        {
            anEntity->setHidden();
        }
    }
    _container->requestWindowRepaint();
    ODL_OBJEXIT(); //####
} // EntitiesPanel::unhideEntities

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
