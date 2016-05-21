//--------------------------------------------------------------------------------------------------
//
//  File:       m+mLeapDisplayWindow.hpp
//
//  Project:    m+m
//
//  Contains:   The class declaration for the primary window of the Leap Motion display output
//              service application.
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

#if (! defined(mpmLeapDisplayWindow_HPP_))
# define mpmLeapDisplayWindow_HPP_ /* Header guard */

# include "m+mLeapDisplayDataTypes.hpp"

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file

 @brief The class declaration for the primary window of the Leap Motion display output service
 application. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace LeapDisplay
{
    class ContentPanel;
    class EntitiesPanel;

    /*! @brief The main window of the application. */
    class LeapDisplayWindow : private AsyncUpdater,
                              public DocumentWindow
    {
    public :

        /*! @brief The commands that we respond to.

         Note that this must use the 'old-style' @c enum form, as it's referenced in code that
         uses 'old-style' values. */
        enum CommandIDs
        {
            /*! @brief The command to invert the background gradient. */
            kCommandInvertBackground = 0x2000,

            /*! @brief The command to set the background to white. */
            kCommandWhiteBackground,
            
            /*! @brief The command to configure the service. */
            kCommandConfigureService,
            
            /*! @brief The command to restart the service. */
            kCommandRestartService,
            
            /*! @brief The command to start the service. */
            kCommandStartService,
            
            /*! @brief The command to stop the service. */
            kCommandStopService

        }; // CommandIDs

    protected :

    private :

        /*! @brief The second class that this class is derived from. */
        typedef AsyncUpdater inherited1;

        /*! @brief The first class that this class is derived from. */
        typedef DocumentWindow inherited2;

    public :

        /*! @brief The constructor.
         @param[in] title The window title. */
        explicit
        LeapDisplayWindow(const YarpString & title);

        /*! @brief The destructor. */
        virtual
        ~LeapDisplayWindow(void);

        /*! @brief Returns the command manager object used to dispatch command events.
         @returns The command manager object used to dispatch command events. */
        static ApplicationCommandManager &
        getApplicationCommandManager(void);

    protected :

    private :

        /*! @brief This method is called when the user tries to close the window. */
        virtual void
        closeButtonPressed(void);

        /*! @brief Called back to perform operations. */
        virtual void
        handleAsyncUpdate(void);

    public :

    protected :

    private :

        /*! @brief The content area of the window. */
        ScopedPointer<ContentPanel> _contentPanel;

        JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR(LeapDisplayWindow)

    }; // LeapDisplayWindow

} // LeapDisplay

#endif // ! defined(mpmLeapDisplayWindow_HPP_)
