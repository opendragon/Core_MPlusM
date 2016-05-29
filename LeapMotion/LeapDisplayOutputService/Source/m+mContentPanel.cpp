//--------------------------------------------------------------------------------------------------
//
//  File:       m+mContentPanel.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the content area of the primary window of the Leap Motion
//              display output service application.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2016 by OpenDragon.
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
//  Created:    2016-05-12
//
//--------------------------------------------------------------------------------------------------

#include "m+mContentPanel.hpp"
#include "m+mGraphicsFrame.hpp"
//#include "m+mGraphicsPanel.hpp"
#include "m+mLeapDisplayApplication.hpp"

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file

 @brief The class definition for the content area of the primary window of the %Leap Motion display
 output service application. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if (! MAC_OR_LINUX_)
# include <Windows.h>
#endif //! MAC_OR_LINUX_

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace LeapDisplay;
using namespace MplusM;
using namespace std;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief The initial thickness of the horizontal and vertical scrollbars. */
static const int kDefaultScrollbarThickness = 16;

/*! @brief The initial single-step size of the horizontal and vertical scrollbars. */
static const int kDefaultSingleStepSize = 10;

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

ContentPanel::ContentPanel(LeapDisplayWindow * containingWindow) :
    inherited1(), inherited2(), inherited3(), _graphicsFrame(new GraphicsFrame(this)),//_graphicsPanel(new GraphicsPanel(this)),
    _menuBar(new MenuBarComponent(this)), _containingWindow(containingWindow),
    _invertBackground(false), _whiteBackground(false)
{
    ODL_ENTER(); //####
    ODL_P1("containingWindow = ", containingWindow); //####
    addAndMakeVisible(_menuBar);
    _graphicsFrame->setSize(_graphicsFrame->getWidth(), _graphicsFrame->getHeight() -
                            _containingWindow->getTitleBarHeight());
    setSize(_graphicsFrame->getWidth(), _graphicsFrame->getHeight());
    setScrollBarsShown(true, true);
    setScrollBarThickness(kDefaultScrollbarThickness);
    setSingleStepSizes(kDefaultSingleStepSize, kDefaultSingleStepSize);
    setViewedComponent(_graphicsFrame);
    setApplicationCommandManagerToWatch(&containingWindow->getApplicationCommandManager());
    ODL_EXIT_P(this); //####
} // ContentPanel::ContentPanel

ContentPanel::~ContentPanel(void)
{
    ODL_OBJENTER(); //####
    setApplicationCommandManagerToWatch(NULL);
    PopupMenu::dismissAllActiveMenus();
    ODL_OBJEXIT(); //####
} // ContentPanel::~ContentPanel

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void
ContentPanel::getAllCommands(Array<CommandID> & commands)
{
    ODL_OBJENTER(); //####
    static const CommandID ids[] =
    {
        LeapDisplayWindow::kCommandInvertBackground,
        LeapDisplayWindow::kCommandWhiteBackground,
        LeapDisplayWindow::kCommandConfigureService,
        LeapDisplayWindow::kCommandRestartService,
        LeapDisplayWindow::kCommandStartService,
        LeapDisplayWindow::kCommandStopService
    };

    commands.addArray(ids, numElementsInArray(ids));
    ODL_OBJEXIT(); //####
} // ContentPanel::getAllCommands

void
ContentPanel::getCommandInfo(CommandID                commandID,
                             ApplicationCommandInfo & result)
{
    ODL_OBJENTER(); //####
    LeapDisplayApplication * ourApp = LeapDisplayApplication::getApp();

    switch (commandID)
    {
        case LeapDisplayWindow::kCommandInvertBackground :
            result.setInfo("Invert", "Invert the background gradient", "View", 0);
            result.addDefaultKeypress('I', ModifierKeys::commandModifier);
            result.setTicked(backgroundIsInverted());
            break;

        case LeapDisplayWindow::kCommandWhiteBackground :
            result.setInfo("White", "Use a white background", "View", 0);
            result.addDefaultKeypress('W', ModifierKeys::commandModifier);
            result.setTicked(backgroundIsWhite());
            break;

        case LeapDisplayWindow::kCommandConfigureService :
            result.setInfo("Configure service", "Configure the service", "View", 0);
            result.addDefaultKeypress('K', ModifierKeys::commandModifier);
            result.setActive(false); // This service has no configurable settings...
            break;

        case LeapDisplayWindow::kCommandRestartService :
            result.setInfo("Restart service", "Restart the service", "View", 0);
            result.addDefaultKeypress('R', ModifierKeys::commandModifier);
            result.setActive(ourApp->serviceIsRunning());
            break;

        case LeapDisplayWindow::kCommandStartService :
            result.setInfo("Start service", "Start the service", "View", 0);
            result.addDefaultKeypress('B', ModifierKeys::commandModifier);
            result.setActive(! ourApp->serviceIsRunning());
            break;
            
        case LeapDisplayWindow::kCommandStopService :
            result.setInfo("Stop service", "Stop the service", "View", 0);
            result.addDefaultKeypress('E', ModifierKeys::commandModifier);
            result.setActive(ourApp->serviceIsRunning());
            break;

        default :
            break;

    }
    ODL_OBJEXIT(); //####
} // ContentPanel::getCommandInfo

StringArray
ContentPanel::getMenuBarNames(void)
{
    ODL_OBJENTER(); //####
    const char * const names[] = { "m+mLeapDisplayOutputService", "View", "Operation", NULL };

    ODL_OBJEXIT(); //####
    return StringArray(names);
} // ContentPanel::getMenuBarNames

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
PopupMenu
ContentPanel::getMenuForIndex(int            menuIndex,
                              const String & menuName)
{
#if (! defined(OD_ENABLE_LOGGING_))
# if MAC_OR_LINUX_
#  pragma unused(menuName)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING_)
    ODL_OBJENTER(); //####
    ODL_LL1("menuIndex = ", menuIndex); //####
    ODL_S1s("menuName = ", menuName.toStdString()); //####
    PopupMenu menu;

    menu.setLookAndFeel(&getLookAndFeel());
    switch (menuIndex)
    {
        case 0 :
            // Main
            setUpMainMenu(menu);
            break;
            
        case 1 :
            // View
            setUpViewMenu(menu);
            break;

        case 2 :
            // Operation
            setUpOperationMenu(menu);
            break;

        default :
            break;

    }
    ODL_OBJEXIT(); //####
    return menu;
} // ContentPanel::getMenuForIndex
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

ApplicationCommandTarget *
ContentPanel::getNextCommandTarget(void)
{
    ODL_OBJENTER(); //####
    ApplicationCommandTarget * nextOne = findFirstTargetParentComponent();

    ODL_OBJEXIT_P(nextOne); //####
    return nextOne;
} // ContentPanel::getNextCommandTarget

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
void
ContentPanel::menuItemSelected(int menuItemID,
                               int topLevelMenuIndex)
{
#if (! defined(OD_ENABLE_LOGGING_))
# if MAC_OR_LINUX_
#  pragma unused(topLevelMenuIndex)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING_)
    ODL_OBJENTER(); //####
    ODL_LL2("menuItemID = ", menuItemID, "topLevelMenuIndex = ", topLevelMenuIndex); //####
    switch (menuItemID)
    {
            // Container menu items
        case kPopupConfigureService :
            //TBD!!!
            break;

        case kPopupRestartService :
            // TBD
            break;

        case kPopupStopService :
            // TBD
            break;

        default :
            break;

    }
    ODL_OBJEXIT(); //####
} // ContentPanel::menuItemSelected
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
void
ContentPanel::mouseDown(const MouseEvent & ee)
{
#if (! defined(OD_ENABLE_LOGGING_))
# if MAC_OR_LINUX_
#  pragma unused(ee)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING_)
    ODL_OBJENTER(); //####
    ODL_P1("ee = ", &ee); //####
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
ContentPanel::mouseDrag(const MouseEvent & ee)
{
#if (! defined(OD_ENABLE_LOGGING_))
# if MAC_OR_LINUX_
#  pragma unused(ee)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING_)
    ODL_OBJENTER(); //####
    ODL_P1("ee = ", &ee); //####
    ODL_OBJEXIT(); //####
} // ChannelContainer::mouseDrag
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
void
ContentPanel::mouseUp(const MouseEvent & ee)
{
#if (! defined(OD_ENABLE_LOGGING_))
# if MAC_OR_LINUX_
#  pragma unused(ee)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING_)
    ODL_OBJENTER(); //####
    ODL_P1("ee = ", &ee); //####
    ODL_OBJEXIT(); //####
} // ContentPanel::mouseUp
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

bool
ContentPanel::perform(const InvocationInfo & info)
{
    ODL_OBJENTER(); //####
    bool                     wasProcessed = false;
    LeapDisplayApplication * ourApp = LeapDisplayApplication::getApp();

    switch (info.commandID)
    {
        case LeapDisplayWindow::kCommandInvertBackground :
            flipBackground();
            requestWindowRepaint();
            wasProcessed = true;
            break;

        case LeapDisplayWindow::kCommandWhiteBackground :
            changeBackgroundColour();
            requestWindowRepaint();
            wasProcessed = true;
            break;

        case LeapDisplayWindow::kCommandConfigureService :
#if 0
            ODL_LOG("LeapDisplayWindow::kCommandConfigureService"); //####
            ourApp->configureAssociatedService();
#endif//0
            wasProcessed = true;
            break;
            
        case LeapDisplayWindow::kCommandRestartService :
            ODL_LOG("LeapDisplayWindow::kCommandRestartService"); //####
            ourApp->restartAssociatedService();
            wasProcessed = true;
            break;
            
        case LeapDisplayWindow::kCommandStartService :
            ODL_LOG("LeapDisplayWindow::kCommandStartService"); //####
            ourApp->startAssociatedService();
            wasProcessed = true;
            break;
            
        case LeapDisplayWindow::kCommandStopService :
            ODL_LOG("LeapDisplayWindow::kCommandStopService"); //####
            ourApp->stopAssociatedService();
            wasProcessed = true;
            break;
            
        default :
            break;

    }
    ODL_OBJEXIT_B(wasProcessed); //####
    return wasProcessed;
} // ContentPanel::perform

void
ContentPanel::requestWindowRepaint(void)
{
    ODL_OBJENTER(); //####
    _containingWindow->repaint();
    ODL_OBJEXIT(); //####
} // ContentPanel::requestWindowRepaint

void
ContentPanel::resized(void)
{
    ODL_OBJENTER(); //####
    juce::Rectangle<int> area(getLocalBounds());
    int                  offset = LookAndFeel::getDefaultLookAndFeel().getDefaultMenuBarHeight();

    _menuBar->setBounds(area.removeFromTop(offset));
    _graphicsFrame->setBounds(area);
    ODL_OBJEXIT(); //####
} // ContentPanel::resized

void
ContentPanel::setUpMainMenu(PopupMenu & aMenu)
{
    ODL_OBJENTER(); //####
    ODL_P1("aMenu = ", &aMenu); //####
    ApplicationCommandManager * commandManager = &LeapDisplayWindow::getApplicationCommandManager();
    
    aMenu.addCommandItem(commandManager, StandardApplicationCommandIDs::quit);
    ODL_OBJEXIT(); //####
} // ContentPanel::setUpMainMenu

void
ContentPanel::setUpOperationMenu(PopupMenu & aMenu)
{
    ODL_OBJENTER(); //####
    ODL_P1("aMenu = ", &aMenu); //####
    ApplicationCommandManager * commandManager = &LeapDisplayWindow::getApplicationCommandManager();
    
    aMenu.addCommandItem(commandManager, LeapDisplayWindow::kCommandConfigureService);
    aMenu.addSeparator();
    aMenu.addCommandItem(commandManager, LeapDisplayWindow::kCommandRestartService);
    aMenu.addCommandItem(commandManager, LeapDisplayWindow::kCommandStartService);
    aMenu.addCommandItem(commandManager, LeapDisplayWindow::kCommandStopService);
    ODL_OBJEXIT(); //####
} // ContentPanel::setUpOperationMenu

void
ContentPanel::setUpViewMenu(PopupMenu & aMenu)
{
    ODL_OBJENTER(); //####
    ODL_P1("aMenu = ", &aMenu); //####
    ApplicationCommandManager * commandManager = &LeapDisplayWindow::getApplicationCommandManager();

    aMenu.addCommandItem(commandManager, LeapDisplayWindow::kCommandInvertBackground);
    aMenu.addCommandItem(commandManager, LeapDisplayWindow::kCommandWhiteBackground);
    ODL_OBJEXIT(); //####
} // ContentPanel::setUpViewMenu

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
void
ContentPanel::visibleAreaChanged(const juce::Rectangle<int> & newVisibleArea)
{
#if (! defined(OD_ENABLE_LOGGING_))
# if MAC_OR_LINUX_
#  pragma unused(newVisibleArea)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING_)
    ODL_OBJENTER(); //####
    ODL_LL4("nVA.x = ", newVisibleArea.getX(), "nVA.y = ", newVisibleArea.getY(), //####
            "nVA.w = ", newVisibleArea.getWidth(), "nVA.h = ", //####
            newVisibleArea.getHeight()); //####
    ODL_OBJEXIT(); //####
} // ContentPanel::visibleAreaChanged
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
