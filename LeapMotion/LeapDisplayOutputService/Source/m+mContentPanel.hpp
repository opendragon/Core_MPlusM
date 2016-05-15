//--------------------------------------------------------------------------------------------------
//
//  File:       m+mContentPanel.hpp
//
//  Project:    m+m
//
//  Contains:   The class declaration for the content area of the primary window of the Leap Motion
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

#if (! defined(mpmContentPanel_HPP_))
# define mpmContentPanel_HPP_ /* Header guard */

# include "m+mLeapDisplayDataTypes.hpp"

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file

 @brief The class declaration for the content area of the primary window of the %Leap Motion display
 output service application. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

/*! @brief Set to @c TRUE to lock settings in order to do screen captures for the manual and
 @c FALSE for normal behaviour. */
# define SETTINGS_FOR_MANUAL_ FALSE

namespace LeapDisplay
{
    class GraphicsPanel;
    class LeapDisplayWindow;

    /*! @brief The content area of the main window of the application. */
    class ContentPanel : public ApplicationCommandTarget,
                         public MenuBarModel,
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
        ContentPanel(LeapDisplayWindow * containingWindow);

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

        /*! @brief Ask the containing window to do a repaint. */
        void
        requestWindowRepaint(void);

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
        mouseUp(const MouseEvent& ee);
                
        /*! @brief Perform the specified command.
         @param info The details for the command.
         @returns @c true if the command was handled and @c false if it was not. */
        virtual bool
        perform(const InvocationInfo & info);

        /*! @brief Called when the component size has been changed. */
        virtual void
        resized(void);

        /*! @brief Prepare the operation menu for use.
         @param aMenu The popup menu to be configured. */
        void
        setUpOperationMenu(PopupMenu & aMenu);

        /*! @brief Prepare the view menu for use.
         @param aMenu The popup menu to be configured. */
        void
        setUpViewMenu(PopupMenu & aMenu);
        
        /*! @brief Called when the visible area changes.
         @param newVisibleArea The new visible area. */
        virtual void
        visibleAreaChanged(const juce::Rectangle<int> & newVisibleArea);

    public :

    protected :

    private :

        /*! @brief The entities panel. */
        ScopedPointer<GraphicsPanel> _graphicsPanel;
        
        /*! @brief The menubar for the panel. */
        ScopedPointer<MenuBarComponent> _menuBar;

        /*! @brief The window in which the panel is embedded. */
        LeapDisplayWindow * _containingWindow;

        /*! @brief @c true if the background is inverted and @c false otherwise. */
        bool _invertBackground;

        /*! @brief @c true if the background is white and @c false otherwise. */
        bool _whiteBackground;

        JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR(ContentPanel)

    }; // ContentPanel

} // LeapDisplay

#endif // ! defined(mpmContentPanel_HPP_)
