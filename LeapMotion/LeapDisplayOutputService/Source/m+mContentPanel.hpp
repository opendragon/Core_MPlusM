//--------------------------------------------------------------------------------------------------
//
//  File:       m+mContentPanel.hpp
//
//  Project:    m+m
//
//  Contains:   The class declaration for the content area of the primary window of the m+mLeapDisplayOutputService
//              application.
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
//  Created:    2014-07-21
//
//--------------------------------------------------------------------------------------------------

#if (! defined(mpmContentPanel_HPP_))
# define mpmContentPanel_HPP_ /* Header guard */

# include "m+mManagerDataTypes.hpp"

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file

 @brief The class declaration for the content area of the primary window of the m+mLeapDisplayOutputService
 application. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

/*! @brief Set to @c TRUE to lock settings in order to do screen captures for the manual and
 @c FALSE for normal behaviour. */
# define SETTINGS_FOR_MANUAL_ FALSE

namespace MPlusM_Manager
{
    class EntitiesPanel;
    class ManagerWindow;
    class ScannerThread;

    /*! @brief The content area of the main window of the application. */
    class ContentPanel : public ApplicationCommandTarget, public MenuBarModel,
                         public Viewport
    {
    public :

    protected :

    private :

        /*! @brief The first class that this class is derived from. */
        typedef ApplicationCommandTarget inherited1;

        /*! @brief The second class that this class is derived from. */
        typedef MenuBarModel inherited2;

        /*! @brief The third class that this class is derived from. */
        typedef Viewport inherited3;

    public :

        /*! @brief The constructor.
         @param containingWindow The window in which the panel is embedded. */
        explicit
        ContentPanel(ManagerWindow * containingWindow);

        /*! @brief The destructor. */
        virtual
        ~ContentPanel(void);

        /*! @brief Returns @c true if the background is inverted.
         @returns @c true if the background is inverted. */
        inline bool
        backgroundIsInverted(void)
        const
        {
            return _invertBackground;
        } // backgroundIsInverted

        /*! @brief Returns @c true if the background is white.
         @returns @c true if the background is white. */
        inline bool
        backgroundIsWhite(void)
        const
        {
            return _whiteBackground;
        } // backgroundIsWhite

        /*! @brief Change the background colour. */
        inline void
        changeBackgroundColour(void)
        {
            _whiteBackground = ! _whiteBackground;
        } // changeBackgroundColour

        /*! @brief Change the background inversion state. */
        inline void
        flipBackground(void)
        {
            _invertBackground = ! _invertBackground;
        } // flipBackground

        /*! @brief Returns the entities panel.
         @returns The entities panel. */
        inline EntitiesPanel &
        getEntitiesPanel(void)
        const
        {
            return *_entitiesPanel;
        } // getEntitiesPanel

        /*! @brief Restore the positions of all the entities in the panel. */
        void
        recallEntityPositions(void);

        /*! @brief Record the position of an entity before it is removed from the panel. */
        void
        rememberPositionOfEntity(ChannelContainer * anEntity);

        /*! @brief Ask the containing window to do a repaint. */
        void
        requestWindowRepaint(void);

        /*! @brief Record the positions of all the entities in the panel. */
        void
        saveEntityPositions(void);

        /*! @brief Record the ChannelEntry that is selected. */
        void
        setChannelOfInterest(ChannelEntry * aChannel);

        /*! @brief Record the ChannelContainer that is selected. */
        void
        setContainerOfInterest(ChannelContainer * aContainer);

        /*! @brief Prepare the channel menu for use.
         @param aMenu The popup menu to be configured.
         @param aChannel The selected channel. */
        void
        setUpChannelMenu(PopupMenu &    aMenu,
                         ChannelEntry & aChannel);

        /*! @brief Prepare the container menu for use.
         @param aMenu The popup menu to be configured.
         @param aContainer The selected container. */
        void
        setUpContainerMenu(PopupMenu &        aMenu,
                           ChannelContainer & aContainer);

        /*! @brief Prepare the view menu for use.
         @param aMenu The popup menu to be configured. */
        void
        setUpViewMenu(PopupMenu & aMenu);

        /*! @brief Ignore the result of the next scan. */
        void
        skipScan(void);

    protected :

    private :

        /*! @brief Return a list of commands that this target can handle.
         @param commands The list of commands to be added to. */
        virtual void
        getAllCommands(Array<CommandID> & commands);

        /*! @brief Provide details about one of the commands that this target can perform.
         @param commandID The identifier for the command.
         @param result The details about the command. */
        virtual void
        getCommandInfo(CommandID                commandID,
                       ApplicationCommandInfo & result);

        /*! @brief Return a list of the names of the menus.
         @returns A list of the names of the menus. */
        virtual StringArray
        getMenuBarNames(void);

        /*! @brief Return the popup menu to display for a given top-level menu.
         @param menuIndex The index of the top-level menu to show.
         @param menuName The name of the top-level menu item to show.
         @returns The popup menu corresponding to the given index and name. */
        virtual PopupMenu
        getMenuForIndex(int            menuIndex,
                        const String & menuName);

        /*! @brief Return the next target to try after this one.
         @returns The next target to try after this one. */
        virtual ApplicationCommandTarget *
        getNextCommandTarget(void);

        /*! @brief Perform the selected menu item action.
         @param menuItemID The item ID of the menu item that was selected.
         @param topLevelMenuIndex The index of the top-level menu from which the item was
         selected. */
        virtual void
        menuItemSelected(int menuItemID,
                         int topLevelMenuIndex);

        /*! @brief Draw the content of the component.
         @param gg The graphics context in which to draw. */
        virtual void
        paint(Graphics & gg);

        /*! @brief Perform the specified command.
         @param info The details for the command.
         @returns @c true if the command was handled and @c false if it was not. */
        virtual bool
        perform(const InvocationInfo & info);

        /*! @brief Called when the component size has been changed. */
        virtual void
        resized(void);

        /*! @brief Set the entity positions, based on the scanned entities. */
        void
        setEntityPositions(void);

        /*! @brief Prepare the main menu for use.
         @param aMenu The popup menu to be configured. */
        void
        setUpMainMenu(PopupMenu & aMenu);

        /*! @brief Refresh the displayed entities and connections, based on the scanned entities.
         @param scanner The background scanning thread. */
        void
        updatePanels(ScannerThread & scanner);

        /*! @brief Called when the visible area changes.
         @param newVisibleArea The new visible area. */
        virtual void
        visibleAreaChanged(const juce::Rectangle<int> & newVisibleArea);

    public :

    protected :

    private :

        /*! @brief The positions that entities were last seen at. */
        PositionMap _rememberedPositions;

        /*! @brief The entities panel. */
        ScopedPointer<EntitiesPanel> _entitiesPanel;

        /*! @brief The menubar for the panel. */
        ScopedPointer<MenuBarComponent> _menuBar;

        /*! @brief The window in which the panel is embedded. */
        ManagerWindow * _containingWindow;

        /*! @brief The selected channel. */
        ChannelEntry * _selectedChannel;

        /*! @brief The selected container. */
        ChannelContainer * _selectedContainer;

        /*! @brief @c true if a channel was clicked and @c false otherwise. */
        bool _channelClicked;

        /*! @brief @c true if a container was clicked and @c false otherwise. */
        bool _containerClicked;

# if (defined(USE_OGDF_POSITIONING_) && defined(USE_OGDF_FOR_FIRST_POSITIONING_ONLY_))
        bool _initialPositioningDone;
# endif // defined(USE_OGDF_POSITIONING_) && defined(USE_OGDF_FOR_FIRST_POSITIONING_ONLY_)

        /*! @brief @c true if the background is inverted and @c false otherwise. */
        bool _invertBackground;

        /*! @brief @c true if the next scan result is to be ignored and @c false otherwise. */
        bool _skipNextScan;

        /*! @brief @c true if the background is white and @c false otherwise. */
        bool _whiteBackground;

        JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR(ContentPanel)

    }; // ContentPanel

    /*! @brief Create and display an information panel.
     @param above The visible component that will be below the panel.
     @param bodyText The text to display in the panel.
     @param title The title of the panel. */
    void DisplayInformationPanel(Component *    above,
                                 const String & bodyText,
                                 const String & title);

} // MPlusM_Manager

#endif // ! defined(mpmContentPanel_HPP_)
