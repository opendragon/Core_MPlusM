//--------------------------------------------------------------------------------------------------
//
//  File:       m+mPlatonicDisplayGraphicsPanel.hpp
//
//  Project:    m+m
//
//  Contains:   The class declaration for the graphics layer of the primary window of the platonic
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
//  Created:    2016-06-04
//
//--------------------------------------------------------------------------------------------------

#if (! defined(mpmPlatonicDisplayGraphicsPanel_HPP_))
# define mpmPlatonicDisplayGraphicsPanel_HPP_ /* Header guard */

# include "m+mPlatonicDisplayDataTypes.hpp"
# include "m+mVertexBuffer.hpp"

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file

 @brief The class declaration for the graphics layer of the primary window of the platonic display
 output service application. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace PlatonicDisplay
{
    class ContentPanel;

    /*! @brief The graphics layer of the main window of the application. */
    class GraphicsPanel : public Component,
                          private OpenGLRenderer
    {
    public :

    protected :

    private :

        /*! @brief The shape to use when drawing the pixies. */
        enum Shape
        {
            /*! @brief Draw a cube. */
            kShapeCube,

            /*! @brief Draw a tetrahedron. */
            kShapeTetrahedron,

            /*! @brief Draw an octahedron. */
            kShapeOctahedron

        }; // Shape

        /*! @brief The class that this class is derived from. */
        typedef Component inherited;

    public :

        /*! @brief The constructor.
         @param[in] container The container in which the panel is embedded.
         @param[in] startingWidth The initial width to use, or zero to use a default width.
         @param[in] startingHeight The initial height to use, or zero to uase a default height. */
        explicit
        GraphicsPanel(ContentPanel * container,
                      const int      startingWidth = 0,
                      const int      startingHeight = 0);

        /*! @brief The destructor. */
        virtual
        ~GraphicsPanel(void);

        /*! @brief Release all data held by the panel. */
        void
        clearOutData(void);

        /*! @brief Return the container in which the panel is embedded.
         @return The container in which the panel is embedded. */
        inline ContentPanel *
        getContent(void)
        const
        {
            return _container;
        } // getContent

        /*! @brief Called when a new OpenGL context has been created. */
        virtual void
        newOpenGLContextCreated(void);

        /*! @brief Called when the current OpenGL context is about to close. */
        virtual void
        openGLContextClosing(void);

        /*! @brief Called when hte next OpenGL frame should be rendered. */
        virtual void
        renderOpenGL(void);

        /*! @brief Update the pixie data.
         
         Note that the access to the data is guarded by a critical section, so this is meant to be
         called from the input handler thread.
         @param[in] newX The new X coordinate.
         @param[in] newY The new Y coordinate.
         @param[in] newZ The new Z coordinate. */
        void
        updatePixieData(const double newX,
                        const double newY,
                        const double newZ);

        /*! @brief Move closer to the images. */
        void
        zoomIn(void);
        
        /*! @brief Move away from the images. */
        void
        zoomOut(void);
        
    protected :

    private :
        
        /*! @brief Disable the active vertex attributes. */
        void
        disableVertexAttributes(void);
        
        /*! @brief Draw the background.
         @param[in] desktopScale The drawing scale to be used. */
        void
        drawBackground(const float desktopScale);

        /*! @brief Draw a pixie.
         @param[in] stuff The pixie information.
         @param[in] theShape The shape to use when drawing. */
        void
        drawPixie(const Pixie & stuff,
                  const Shape   theShape);

        /*! @brief Draw some pixies.
         @param[in] toBeDrawn The pixies to draw. */
        void
        drawPixies(const Array<Pixie> & toBeDrawn);

        /*! @brief Enable the active vertex attributes. */
        void
        enableVertexAttributes(void);
        
        /*! @brief Release the objects attached to the current OpenGL context. */
        void
        freeContextObjects(void);

        /*! @brief Return the current projection matrix.
         @return The current projection matrix. */
        Matrix3D<float>
        getProjectionMatrix(void)
        const;
        
        /*! @brief Return the current view matrix.
         @return The current view matrix. */
        Matrix3D<float>
        getViewMatrix(void)
        const;

        /*! @brief Called when a mouse button is pressed.
         @param[in] ee Details about the position and status of the mouse event. */
        virtual void
        mouseDown(const MouseEvent & ee);

        /*! @brief Called when the mouse is moved while a button is held down.
         @param[in] ee Details about the position and status of the mouse event. */
        virtual void
        mouseDrag(const MouseEvent & ee);

        /*! @brief Called when the component size has been changed. */
        virtual void
        resized(void);

        /*! @brief Set the shader program source strings.
         @param[in] vertexShader The source for the vertex shader.
         @param[in] fragmentShader The source for the fragment shader. */
        void
        setShaderProgram(const String & vertexShader,
                         const String & fragmentShader);
        
        /*! @brief Set up the cube data. */
        void
        setUpCube(void);
        
        /*! @brief Set up the octahedron data. */
        void
        setUpOctahedron(void);
        
        /*! @brief Set up the tetrahedron data. */
        void
        setUpTetrahedron(void);
        
        /*! @brief Set up the texture for use. */
        void
        setUpTexture(void);
        
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

        /*! @brief The texture to be used for rendering. */
        OpenGLTexture _texture;
        
        /*! @brief Support for changing the visual orientation with the mouse. */
        Draggable3DOrientation _draggableOrientation;
 
        /*! @brief The vertex data for a cube. */
        Array<CommonVisuals::Vertex> _cubeVertices;
        
        /*! @brief The vertex data for an octahedron. */
        Array<CommonVisuals::Vertex> _octahedronVertices;
        
        /*! @brief The vertex data for a tetrahedron. */
        Array<CommonVisuals::Vertex> _tetrahedronVertices;

        /*! @brief The pixies to be drawn. */
        Array<Pixie> _pixies;
        
        /*! @brief The shader program to use for rendering. */
        ScopedPointer<OpenGLShaderProgram> _shaderProgram;
        
        /*! @brief The uniform for the offset. */
        ScopedPointer<OpenGLShaderProgram::Uniform> _offsetUniform;

        /*! @brief The uniform for the projection matrix. */
        ScopedPointer<OpenGLShaderProgram::Uniform> _projectionMatrixUniform;

        /*! @brief The uniform for the view matrix. */
        ScopedPointer<OpenGLShaderProgram::Uniform> _viewMatrixUniform;
        
        /*! @brief The uniform for the texture. */
        ScopedPointer<OpenGLShaderProgram::Uniform> _textureUniform;
        
        /*! @brief The uniform for the light position. */
        ScopedPointer<OpenGLShaderProgram::Uniform> _lightPositionUniform;
        
        /*! @brief The vertex attribute for its position. */
        ScopedPointer<OpenGLShaderProgram::Attribute> _positionAttribute;
        
        /*! @brief The vertex attribute for its surface normal. */
        ScopedPointer<OpenGLShaderProgram::Attribute> _normalAttribute;
        
        /*! @brief The vertex attribute for its colour. */
        ScopedPointer<OpenGLShaderProgram::Attribute> _sourceColourAttribute;
        
        /*! @brief The vertex attribute for its texture coordinate. */
        ScopedPointer<OpenGLShaderProgram::Attribute> _textureCoordInAttribute;
        
        /*! @brief The vertices, normals, colour and texture coordinates for a cube. */
        ScopedPointer<CommonVisuals::VertexBuffer> _cubeData;
        
        /*! @brief The vertices, normals, colour and texture coordinates for an octahedron. */
        ScopedPointer<CommonVisuals::VertexBuffer> _octahedronData;
        
        /*! @brief The vertices, normals, colour and texture coordinates for a tetrahedron. */
        ScopedPointer<CommonVisuals::VertexBuffer> _tetrahedronData;
        
        /*! @brief The source code for the fragment shader. */
        String _fragmentShaderSource;
        
        /*! @brief The source code for the vertex shader. */
        String _vertexShaderSource;
        
        /*! @brief The container in which the panel is embedded. */
        ContentPanel * _container;

        /*! @brief The rotation angle to apply to the image. */
        float _rotation;
        
        /*! @brief The scaling factor to apply to the image. */
        float _scale;

        /*! @brief The next pixie to be updated. */
        int _nextPixie;
        
        JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR(GraphicsPanel)

    }; // GraphicsPanel

} // PlatonicDisplay

#endif // ! defined(mpmPlatonicDisplayGraphicsPanel_HPP_)
