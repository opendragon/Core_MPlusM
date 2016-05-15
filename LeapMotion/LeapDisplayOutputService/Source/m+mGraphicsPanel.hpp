//--------------------------------------------------------------------------------------------------
//
//  File:       m+mGraphicsPanel.hpp
//
//  Project:    m+m
//
//  Contains:   The class declaration for the graphics layer of the primary window of the Leap
//              Motion display output service application.
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
//  Created:    2016-05-14
//
//--------------------------------------------------------------------------------------------------

#if (! defined(mpmGraphicsPanel_HPP_))
# define mpmGraphicsPanel_HPP_ /* Header guard */

# include "m+mLeapDisplayDataTypes.hpp"

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file

 @brief The class declaration for the graphics layer of the primary window of the %Leap Motion
 display output service application. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace LeapDisplay
{
    class ContentPanel;

    /*! @brief The entities layer of the main window of the application. */
    class GraphicsPanel : public Component,
                          private OpenGLRenderer
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
        GraphicsPanel(ContentPanel * container,
                      const int      startingWidth = 0,
                      const int      startingHeight = 0);

        /*! @brief The destructor. */
        virtual
        ~GraphicsPanel(void);

        /*! @brief Clear any active dragging information. */
        void
        clearDragInfo(void);

        /*! @brief Release all data held by the panel. */
        void
        clearOutData(void);

        /*! @brief Return the container in which the panel is embedded.
         @returns The container in which the panel is embedded. */
        inline ContentPanel *
        getContent(void)
        const
        {
            return _container;
        } // getContent

        /*! @brief Return @c true if dragging a connection and @c false otherwise.
         @returns @c true if dragging a connection and @c false otherwise. */
        inline bool
        isDragActive(void)
        const
        {
            return _dragConnectionActive;
        } // isDragActive

        /*! @brief Called when a new OpenGL context has been created. */
        virtual void
        newOpenGLContextCreated(void);

        /*! @brief Called when the current OpenGL context is about to close. */
        virtual void
        openGLContextClosing(void);

        /*! @brief Called when hte next OpenGL frame should be rendered. */
        virtual void
        renderOpenGL(void);

        /*! @brief Update the dragging information.
         @param position The location of the dragging connection.
         @param isForced @c true if the drag line should show a forced connection and @c false
         otherwise. */
        void
        setDragInfo(const Position position,
                    const bool     isForced);

        /*! @brief Update the finger data for each hand.
         
         Note that the access to the data is guarded by a critical section, so this is meant to be
         called from the input handler thread.
         @param leftHand The new data for the left hand.
         @param rightHand The new data for the right hand. */
        void
        updateFingerData(const HandData & leftHand,
                         const HandData & rightHand);

    protected :

    private :
        
        /*! @brief Draw the background.
         @param desktopScale The drawing scale to be used. */
        void
        drawBackground2DStuff(const float desktopScale);
        
        /*! @brief Release the objects attached to the current OpenGL context. */
        void
        freeContextObjects(void);

        /*! @brief Called when a mouse button is pressed.
         @param ee Details about the position and status of the mouse event. */
        virtual void
        mouseDown(const MouseEvent & ee);

        /*! @brief Called when a mouse button is released.
         @param ee Details about the position and status of the mouse event. */
        virtual void
        mouseUp(const MouseEvent& ee);

        /*! @brief Called when the component size has been changed. */
        virtual void
        resized(void);

        /*! @brief Refresh the shader program. */
        void
        updateShader(void);
 
    public :

    protected :

    private :
        
        /*! @brief The access control for the finger coordinates. */
        CriticalSection _csect;
        
        /*! @brief The OpenGL rendering context. */
        OpenGLContext _context;

        /*! @brief The shader program to use for rendering. */
        ScopedPointer<OpenGLShaderProgram> _shaderProgram;
        
        /*! @brief The most recent data for the left hand. */
        HandData _leftHand;
        
        /*! @brief The most recent data for the right hand. */
        HandData _rightHand;
        
        /*! @brief The source code for the fragment shader. */
        String _fragmentShaderSource;
        
        /*! @brief The source code for the vertex shader. */
        String _vertexShaderSource;
        
        /*! @brief The coordinates of the drag-connection operation. */
        Position _dragPosition;

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
        char _filler[6];
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

        JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR(GraphicsPanel)

    }; // GraphicsPanel

} // LeapDisplay

#endif // ! defined(mpmGraphicsPanel_HPP_)
