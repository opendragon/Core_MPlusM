//--------------------------------------------------------------------------------------------------
//
//  File:       m+mLeapDisplayGraphicsPanel.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the graphics layer of the primary window of the Leap Motion
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
//  Created:    2016-05-14
//
//--------------------------------------------------------------------------------------------------

#include "m+mLeapDisplayGraphicsPanel.hpp"
#include "m+mLeapDisplayApplication.hpp"
#include "m+mLeapDisplayContentPanel.hpp"
#include "m+mVertexBUffer.hpp"

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file

 @brief The class definition for the graphics layer of the primary window of the %Leap Motion
 display output service application. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace CommonVisuals;
using namespace LeapDisplay;
using namespace MplusM;
using namespace std;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

//#define DO_AUTOROTATE /* set for autorotation */

/*! @brief The first colour to be used for the panel background. */
static const Colour & kFirstBackgroundColour(Colours::darkgrey);

/*! @brief The second colour to be used for the panel background. */
static const Colour & kSecondBackgroundColour(Colours::lightgrey);

/*! @brief The scale to be applied to the finger tip shapes. */
static const double kTipScale = 0.125;

#if defined(DO_AUTOROTATE)
/*! @brief The rotation rate. */
static const float kRotationSpeed = 0.01f;
#endif // defined(DO_AUTOROTATE)

/*! @brief The initial height of the displayed region. */
static const int kInitialPanelHeight = 512;

/*! @brief The initial width of the displayed region. */
static const int kInitialPanelWidth = 512;

/*! @brief The vertex shader source code. */
static const String kVertexShaderSource =
    "attribute vec4 position;\n"
    "attribute vec4 normal;\n"
    "attribute vec4 sourceColour;\n"
    "attribute vec2 texureCoordIn;\n"
    "\n"
    "uniform vec4 offset;\n"
    "uniform mat4 projectionMatrix;\n"
    "uniform mat4 viewMatrix;\n"
    "uniform vec4 lightPosition;\n"
    "\n"
    "varying vec4 destinationColour;\n"
    "varying vec2 textureCoordOut;\n"
    "varying float lightIntensity;\n"
    "\n"
    "void main()\n"
    "{\n"
    "    destinationColour = sourceColour;\n"
    "    textureCoordOut = texureCoordIn;\n"
    "\n"
    "    vec4 light = (viewMatrix * lightPosition);\n"
    "\n"
    "    lightIntensity = dot(light, normal);\n"
    "\n"
    "    gl_Position = (projectionMatrix * viewMatrix * (position + offset));\n"
    "}\n";

/*! @brief The fragment shader source code. */
static const String kFragmentShaderSource =
    "varying vec4 destinationColour;\n"
    "varying vec2 textureCoordOut;\n"
    "varying float lightIntensity;\n"
    "\n"
    "uniform sampler2D theTexture;\n"
    "\n"
    "void main()\n"
    "{\n"
    "   float ll = max(0.5, lightIntensity * 0.5);\n"
    "\n"
    "   vec4 colour = vec4(ll, ll, ll, 1.0);\n"
    "\n"
    " gl_FragColor = (colour * destinationColour * texture2D(theTexture, textureCoordOut));\n"
    "}\n";

#if defined(__APPLE__)
# pragma mark Global constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

/*! @brief Create an Attribute from the shader program.
 @param[in] context The active OpenGL context.
 @param[in] shader The shader program containing the vertex attribute.
 @param[in] attributeName The name of the desired vertex attribute.
 @returns An Attribute representing the vertex attribute from the shader program. */
static OpenGLShaderProgram::Attribute *
createAttribute(OpenGLContext &       context,
                OpenGLShaderProgram & shader,
                const char *          attributeName)
{
    ODL_ENTER(); //####
    ODL_P2("context = ", &context, "shader = ", &shader); //####
    ODL_S1("attributeName = ", attributeName); //####
    OpenGLShaderProgram::Attribute * result;
    
    if (0 > context.extensions.glGetAttribLocation(shader.getProgramID(), attributeName))
    {
        result = NULL;
    }
    else
    {
        result = new OpenGLShaderProgram::Attribute(shader, attributeName);
    }
    ODL_EXIT_P(result); //####
    return result;
} // createAttribute

/*! @brief Create a Uniform from the shader program.
 @param[in] context The active OpenGL context.
 @param[in] shader The shader program containing the uniform.
 @param[in] uniformName The name of the desired uniform.
 @returns A Uniform representing the uniform from the shader program. */
static OpenGLShaderProgram::Uniform *
createUniform(OpenGLContext &       context,
              OpenGLShaderProgram & shader,
              const char *          uniformName)
{
    ODL_ENTER(); //####
    ODL_P2("context = ", &context, "shader = ", &shader); //####
    ODL_S1("uniformName ", uniformName); //####
    OpenGLShaderProgram::Uniform * result;
    
    if (0 > context.extensions.glGetUniformLocation(shader.getProgramID(), uniformName))
    {
        result = NULL;
    }
    else
    {
        result = new OpenGLShaderProgram::Uniform(shader, uniformName);
    }
    ODL_EXIT_P(result); //####
    return result;
} // createUniform

/*! @brief Fill in the values for a vertex.
 @param[in,out] vertexList The vertex data list to be updated.
 @param[in] px The x coordinate (and x normal) of the vertex.
 @param[in] py The y coordinate (and y normal) of the vertex.
 @param[in] pz The z coordinate (and z normal) of the vertex.
 @param[in] colr The colour of the vertex.
 @param[in] ts The s texture coordinate of the vertex.
 @param[in] tt The t texture coordinate of the vertex. */
static void
setVertexData(Array<Vertex> & vertexList,
              const double    px,
              const double    py,
              const double    pz,
              const Colour &  colr,
              const float     ts,
              const float     tt)
{
    ODL_ENTER();
    ODL_P2("vertexList = ", &vertexList, "color = ", &colr); //####
    ODL_D4("px = ", px, "py = ", py, "pz = ", pz, "ts = ", ts);//####
    ODL_D1("tt = ", tt); //####
    Vertex aVertex;
    
    aVertex._position[0] = static_cast<float>(kTipScale * px);
    aVertex._position[1] = static_cast<float>(kTipScale * py);
    aVertex._position[2] = static_cast<float>(kTipScale * pz);
    aVertex._normal[0] = static_cast<float>(px);
    aVertex._normal[1] = static_cast<float>(py);
    aVertex._normal[2] = static_cast<float>(pz);
    aVertex._colour[0] = colr.getFloatRed();
    aVertex._colour[1] = colr.getFloatGreen();
    aVertex._colour[2] = colr.getFloatBlue();
    aVertex._colour[3] = colr.getFloatAlpha();
    aVertex._texCoord[0] = ts;
    aVertex._texCoord[1] = tt;
    vertexList.add(aVertex);
    ODL_EXIT();
} // setVertexData

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

GraphicsPanel::GraphicsPanel(ContentPanel * theContainer,
                             const int      startingWidth,
                             const int      startingHeight) :
    inherited(), _container(theContainer), _rotation(0.0f), _scale(0.5f)
{
    ODL_ENTER(); //####
    ODL_P1("theContainer = ", theContainer); //####
    ODL_LL2("startingWidth = ", startingWidth, "startingHeight = ", startingHeight); //####
    setSize((0 < startingWidth) ? startingWidth : kInitialPanelWidth,
            (0 < startingHeight) ? startingHeight : kInitialPanelHeight);
    _context.setRenderer(this);
    _context.attachTo(*this);
    _context.setContinuousRepainting(true);
    _context.setComponentPaintingEnabled(false);
    setShaderProgram(kVertexShaderSource, kFragmentShaderSource);
    ODL_EXIT_P(this); //####
} // GraphicsPanel::GraphicsPanel

GraphicsPanel::~GraphicsPanel(void)
{
    ODL_OBJENTER(); //####
    _context.detach();
    clearOutData();
    ODL_OBJEXIT(); //####
} // GraphicsPanel::~GraphicsPanel

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void
GraphicsPanel::clearOutData(void)
{
    ODL_OBJENTER(); //####
    removeAllChildren();
    ODL_OBJEXIT(); //####
} // GraphicsPanel::clearOutData

void
GraphicsPanel::disableVertexAttributes(void)
{
    ODL_OBJENTER(); //####
    if (NULL != _positionAttribute)
    {
        _context.extensions.glDisableVertexAttribArray(_positionAttribute->attributeID);
    }
    if (NULL != _normalAttribute)
    {
        _context.extensions.glDisableVertexAttribArray(_normalAttribute->attributeID);
    }
    if (NULL != _sourceColourAttribute)
    {
        _context.extensions.glDisableVertexAttribArray(_sourceColourAttribute->attributeID);
    }
    if (NULL != _textureCoordInAttribute)
    {
        _context.extensions.glDisableVertexAttribArray(_textureCoordInAttribute->attributeID);
    }
    ODL_OBJEXIT(); //####
} // GraphicsPanel::disableVertexAttributes

void
GraphicsPanel::drawBackground(const float desktopScale)
{
    ODL_OBJENTER(); //####
    ODL_D1("desktopScale = ", desktopScale); //####
    int hh = getHeight();
    int ww = getWidth();
    ODL_LL2("hh <- ", hh, "ww <- ", ww); //####
    ScopedPointer<LowLevelGraphicsContext>
                            glRenderer(createOpenGLGraphicsContext(_context,
                                                                   roundToInt(desktopScale * ww),
                                                                   roundToInt(desktopScale * hh)));
    
    if (NULL == glRenderer)
    {
        ODL_LOG("(NULL == glRenderer)"); //####
    }
    else
    {
        Graphics gg(*glRenderer);
        
        gg.addTransform(AffineTransform::scale(desktopScale));
        if (_container->backgroundIsWhite())
        {
            gg.setFillType(_container->backgroundIsInverted() ? kFirstBackgroundColour :
                           kSecondBackgroundColour);
        }
        else
        {
            // Set up a gradient background, using a radial gradient from the centre to the furthest
            // edge.
            float halfH = static_cast<float>(hh / 2.0);
            float halfW = static_cast<float>(ww / 2.0);
            
            if (_container->backgroundIsInverted())
            {
                ColourGradient theGradient2(kFirstBackgroundColour, halfW, halfH,
                                            kSecondBackgroundColour,
                                            static_cast<float>((hh > ww) ? 0 : ww),
                                            static_cast<float>((hh > ww) ? hh : 0), true);
                FillType       theBackgroundFill2(theGradient2);
                
                gg.setFillType(theBackgroundFill2);
            }
            else
            {
                ColourGradient theGradient1(kSecondBackgroundColour, halfW, halfH,
                                            kFirstBackgroundColour,
                                            static_cast<float>((hh > ww) ? 0 : ww),
                                            static_cast<float>((hh > ww) ? hh : 0), true);
                FillType       theBackgroundFill1(theGradient1);
                
                gg.setFillType(theBackgroundFill1);
            }
        }
        gg.fillAll();
    }
    ODL_OBJEXIT(); //####
} // GraphicsPanel::drawBackground

void
GraphicsPanel::drawFingertip(const FingerTip & stuff,
                             const Shape       theShape)
{
    ODL_OBJENTER(); //####
    ODL_P1("stuff = ", &stuff); //####
    ODL_LL1("theShape = ", theShape); //####
    if (stuff._valid)
    {
        const Location & where = stuff._where;
        VertexBuffer *   selected;

        switch (theShape)
        {
            case kShapeCube :
                if (! _cubeData)
                {
                    setUpCube();
                }
                selected = _cubeData;
                break;

            case kShapeTetrahedron :
                if (! _tetrahedronData)
                {
                    setUpTetrahedron();
                }
                selected = _tetrahedronData;
                break;

            case kShapeOctahedron :
                if (! _octahedronData)
                {
                    setUpOctahedron();
                }
                selected = _octahedronData;
                break;

            default :
                selected = NULL;
                break;

        }
        if (selected)
        {
            ODL_LOG("(selected)"); //####
            if (NULL != _offsetUniform)
            {
                _offsetUniform->set(static_cast<float>(where.x), static_cast<float>(where.y),
                                    static_cast<float>(where.z), 0.0f);
            }
            selected->bind();
            enableVertexAttributes();
            glDrawElements(GL_TRIANGLES, selected->numberOfIndices(), GL_UNSIGNED_INT, 0);
            disableVertexAttributes();
        }
    }
    ODL_OBJEXIT(); //####
} // GraphicsPanel::drawFingertip

void
GraphicsPanel::drawHand(const HandData & aHand)
{
    ODL_OBJENTER(); //####
    ODL_P1("aHand = ", &aHand); //####
    drawFingertip(aHand._palm, kShapeCube);
    drawFingertip(aHand._thumb, kShapeTetrahedron);
    drawFingertip(aHand._index, kShapeOctahedron);
    drawFingertip(aHand._middle, kShapeCube);
    drawFingertip(aHand._ring, kShapeOctahedron);
    drawFingertip(aHand._pinky, kShapeTetrahedron);
    ODL_OBJEXIT(); //####
} // GraphicsPanel::drawHand

void
GraphicsPanel::enableVertexAttributes(void)
{
    ODL_OBJENTER(); //####
    if (NULL != _positionAttribute)
    {
        _context.extensions.glVertexAttribPointer(_positionAttribute->attributeID, 3, GL_FLOAT,
                                                  GL_FALSE, sizeof(Vertex),
                                                  reinterpret_cast<GLvoid *>(0));
        _context.extensions.glEnableVertexAttribArray(_positionAttribute->attributeID);
    }
    if (NULL != _normalAttribute)
    {
        _context.extensions.glVertexAttribPointer(_normalAttribute->attributeID, 3, GL_FLOAT,
                                                  GL_FALSE, sizeof(Vertex),
                                                  reinterpret_cast<GLvoid *>(sizeof(float) * 3));
        _context.extensions.glEnableVertexAttribArray(_normalAttribute->attributeID);
    }
    
    if (NULL != _sourceColourAttribute)
    {
        _context.extensions.glVertexAttribPointer(_sourceColourAttribute->attributeID, 4, GL_FLOAT,
                                                  GL_FALSE, sizeof(Vertex),
                                                  reinterpret_cast<GLvoid *>(sizeof(float) * 6));
        _context.extensions.glEnableVertexAttribArray(_sourceColourAttribute->attributeID);
    }
    
    if (NULL != _textureCoordInAttribute)
    {
        _context.extensions.glVertexAttribPointer(_textureCoordInAttribute->attributeID, 2,
                                                  GL_FLOAT, GL_FALSE, sizeof(Vertex),
                                                  reinterpret_cast<GLvoid *>(sizeof(float) * 10));
        _context.extensions.glEnableVertexAttribArray(_textureCoordInAttribute->attributeID);
    }
    ODL_OBJEXIT(); //####
} // GraphicsPanel::enableVertexAttributes

void
GraphicsPanel::freeContextObjects(void)
{
    ODL_OBJENTER(); //####
    _shaderProgram = NULL;
    ODL_P1("_shaderProgram <- ", _shaderProgram); //####
    _offsetUniform = NULL;
    _projectionMatrixUniform = NULL;
    _viewMatrixUniform = NULL;
    _textureUniform = NULL;
    _lightPositionUniform = NULL;
    ODL_P4("_offsetUniform <- ", _offsetUniform, "_projectionMatrixUniform <- ", //####
           _projectionMatrixUniform, "_viewMatrixUniform <- ", _viewMatrixUniform, //####
           "_textureUniform <- ", _textureUniform); //####
    ODL_P1("_lightPositionUniform <- ", _lightPositionUniform); //####
    _positionAttribute = NULL;
    _normalAttribute = NULL;
    _sourceColourAttribute = NULL;
    _textureCoordInAttribute = NULL;
    _texture.release();
    ODL_P4("_positionAttribute <- ", _positionAttribute, "_normalAttribute <- ", //####
           _normalAttribute, "_sourceColourAttribute <- ", _sourceColourAttribute, //####
           "_textureCoordInAttribute <- ", _textureCoordInAttribute); //####
    ODL_OBJEXIT(); //####
} // GraphicsPanel::freeContextObjects

Matrix3D<float>
GraphicsPanel::getProjectionMatrix(void)
const
{
    float ww = (1.0f / (_scale + 0.1f));
    float hh = (ww * getLocalBounds().toFloat().getAspectRatio(false));
    
    return Matrix3D<float>::fromFrustum(-ww, ww, -hh, hh, 4.0f, 30.0f);
} // GraphicsPanel::getProjectionMatrix

Matrix3D<float>
GraphicsPanel::getViewMatrix(void)
const
{
    Matrix3D<float> viewMatrix = (_draggableOrientation.getRotationMatrix() *
                                  Vector3D<float>(0.0f, 1.0f, -10.0f));
    Matrix3D<float> rotationMatrix = viewMatrix.rotated(Vector3D<float>(_rotation, _rotation,
                                                                        -0.3f));
    
    return (rotationMatrix * viewMatrix);
} // GraphicsPanel::getViewMatrix

void
GraphicsPanel::mouseDown(const MouseEvent & ee)
{
    ODL_OBJENTER(); //####
    ODL_P1("ee = ", &ee); //####
    _draggableOrientation.mouseDown(ee.getPosition());
    ODL_OBJEXIT(); //####
} // GraphicsPanel::mouseDown

void
GraphicsPanel::mouseDrag(const MouseEvent & ee)
{
    ODL_OBJENTER(); //####
    ODL_P1("ee = ", &ee); //####
    _draggableOrientation.mouseDrag(ee.getPosition());
    ODL_OBJEXIT(); //####
} // GraphicsPanel::mouseDrag

void
GraphicsPanel::newOpenGLContextCreated(void)
{
    ODL_OBJENTER(); //####
    freeContextObjects();
    ODL_OBJEXIT(); //####
} // GraphicsPanel::newOpenGLContextCreated

void
GraphicsPanel::openGLContextClosing(void)
{
    ODL_OBJENTER(); //####
    freeContextObjects();
    ODL_OBJEXIT(); //####
} // GraphicsPanel::openGLContextClosing

void
GraphicsPanel::renderOpenGL(void)
{
    ODL_OBJENTER(); //####
    if (OpenGLHelpers::isContextActive())
    {
        const float desktopScale = static_cast<float>(_context.getRenderingScale());
        HandData    leftCopy;
        HandData    rightCopy;
        
        _csect.enter();
        memcpy(&leftCopy, &_leftHand, sizeof(leftCopy));
        memcpy(&rightCopy, &_rightHand, sizeof(rightCopy));
        _csect.exit();
        OpenGLHelpers::clear(Colours::lightblue); // FOR NOW
        setUpTexture();
        drawBackground(desktopScale);
        updateShader();
        if (NULL != _shaderProgram)
        {
            ODL_LOG("(NULL != _shaderProgram)"); //####
            glEnable(GL_DEPTH_TEST);
            glDepthFunc(GL_LESS);
            glEnable(GL_BLEND);
            glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
            _context.extensions.glActiveTexture(GL_TEXTURE0);
            glEnable(GL_TEXTURE_2D);
            glViewport(0, 0, roundToInt(desktopScale * getWidth()),
                       roundToInt(desktopScale * getHeight()));
            _texture.bind();
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
            _shaderProgram->use();
            if (NULL != _projectionMatrixUniform)
            {
                _projectionMatrixUniform->setMatrix4(getProjectionMatrix().mat, 1, false);
            }
            if (NULL != _viewMatrixUniform)
            {
                _viewMatrixUniform->setMatrix4(getViewMatrix().mat, 1, false);
            }
            if (NULL != _textureUniform)
            {
                _textureUniform->set(static_cast<GLint>(0));
            }
            if (NULL != _lightPositionUniform)
            {
                _lightPositionUniform->set(-15.0f, 10.0f, 15.0f, 0.0f);
            }
            drawHand(leftCopy);
            drawHand(rightCopy);
            // Reset the element buffers so child Components draw correctly
            _context.extensions.glBindBuffer(GL_ARRAY_BUFFER, 0);
            _context.extensions.glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
#if defined(DO_AUTOROTATE)
            if (! isMouseButtonDown())
            {
                _rotation += kRotationSpeed;
            }
#endif // defined(DO_AUTOROTATE)
        }
    }
    else
    {
        ODL_LOG("! (OpenGLHelpers::isContextActive())"); //####
    }
    ODL_OBJEXIT(); //####
} // GraphicsPanel::renderOpenGL

void
GraphicsPanel::resized(void)
{
    ODL_OBJENTER(); //####
    _draggableOrientation.setViewport(getLocalBounds());
    ODL_OBJEXIT(); //####
} // GraphicsPanel::resized

void
GraphicsPanel::setShaderProgram(const String & vertexShader,
                                const String & fragmentShader)
{
    ODL_OBJENTER(); //####
    ODL_S2s("vertexShader = ", vertexShader.toStdString(), "fragmentShader = ", //####
            fragmentShader.toStdString()); //####
    _vertexShaderSource = vertexShader;
    _fragmentShaderSource = fragmentShader;
    ODL_OBJEXIT(); //####
} // GraphicsPanel::setShaderProgram

void
GraphicsPanel::setUpCube(void)
{
    ODL_OBJENTER(); //####
    double              root3inv = (1.0 / sqrt(3.0));
    static juce::uint32 indices[] =
    {
        3, 2, 1, // A
        1, 0, 3,
        2, 5, 4, // B
        4, 1, 2,
        0, 1, 4, // C
        4, 7, 0,
        3, 0, 7, // D
        7, 6, 3,
        4, 5, 6, // E
        6, 7, 4,
        2, 3, 6, // F
        6, 5, 2
    };
    const size_t numIndices = (sizeof(indices) / sizeof(*indices));
    
    setVertexData(_cubeVertices, - root3inv, root3inv, root3inv, Colours::purple, 0.0, 1.0); // P0
    setVertexData(_cubeVertices, - root3inv, - root3inv, root3inv, Colours::red, 1.0, 1.0); // P1
    setVertexData(_cubeVertices, - root3inv, - root3inv, - root3inv, Colours::blue, 1.0, 0.0); // P2
    setVertexData(_cubeVertices, - root3inv, root3inv, - root3inv, Colours::yellow, 0.0, 0.0); // P3
    setVertexData(_cubeVertices, root3inv, - root3inv, root3inv, Colours::yellow, 0.0, 1.0); // P4
    setVertexData(_cubeVertices, root3inv, - root3inv, - root3inv, Colours::purple, 0.0, 0.0); // P5
    setVertexData(_cubeVertices, root3inv, root3inv, - root3inv, Colours::red, 1.0, 0.0); // P6
    setVertexData(_cubeVertices, root3inv, root3inv, root3inv, Colours::blue, 1.0, 1.0); // P7
    _cubeData = new VertexBuffer(_context, _cubeVertices, indices, numIndices);
    ODL_P1("_cubeData <- ", _cubeData); //####
    ODL_OBJEXIT(); //####
} // GraphicsPanel::setUpCube

void
GraphicsPanel::setUpOctahedron(void)
{
    ODL_OBJENTER(); //####
    double              root2inv = (1.0 / sqrt(2.0));
    static juce::uint32 indices[] =
    {
        0, 1, 4, // A
        1, 2, 4, // B
        0, 4, 3, // C
        3, 4, 2, // D
        5, 1, 0, // E
        5, 2, 1, // F
        5, 3, 2, // G
        5, 0, 3 // H
    };
    const size_t numIndices = (sizeof(indices) / sizeof(*indices));
    
    setVertexData(_octahedronVertices, - root2inv, root2inv, 0, Colours::lightblue, 0, 0.5); // P0
    setVertexData(_octahedronVertices, - root2inv, - root2inv, 0, Colours::lightblue, 0.25,
                  0.5); // P1
    setVertexData(_octahedronVertices, root2inv, - root2inv, 0, Colours::lightblue, 0.5, 0.5); // P2
    setVertexData(_octahedronVertices, root2inv, root2inv, 0, Colours::lightblue, 0.75, 0.5); // P3
    setVertexData(_octahedronVertices, 0, 0, 1, Colours::purple, 0.5, 1.0); // P4
    setVertexData(_octahedronVertices, 0, 0, -1, Colours::green, 0.5, 0.0); // P5
    _octahedronData = new VertexBuffer(_context, _octahedronVertices, indices, numIndices);
    ODL_P1("_octahedronData <- ", _octahedronData); //####
    ODL_OBJEXIT(); //####
} // GraphicsPanel::setUpOctahedron

void
GraphicsPanel::setUpTetrahedron(void)
{
    ODL_OBJENTER(); //####
    double root3inv = (1.0 / sqrt(3.0));
    double root2 = sqrt(2.0);
    static juce::uint32 indices[] =
    {
        0, 1, 2, // A
        0, 2, 3, // B
        0, 3, 1, // C
        1, 3, 2 // D
    };
    const size_t numIndices = (sizeof(indices) / sizeof(*indices));
    
    setVertexData(_tetrahedronVertices, 0, 0, 1, Colours::aqua, 0.5, 1.0); // P0
    setVertexData(_tetrahedronVertices, - (root2 * root3inv), - (root2 / 3.0), - 1.0 / 3.0,
                  Colours::lightblue, 0.0, 0.0); // P1
    setVertexData(_tetrahedronVertices, root2 * root3inv, - (root2  / 3.0), - 1.0 / 3.0,
                  Colours::skyblue, 0.5, 0.0); // P2
    setVertexData(_tetrahedronVertices, 0, 2.0 * root2 * root3inv, - 1.0 / 3.0, Colours::aliceblue,
                  1.0, 0.0); // P3
    _tetrahedronData = new VertexBuffer(_context, _tetrahedronVertices, indices, numIndices);
    ODL_P1("_tetrahedronData <- ", _tetrahedronData); //####
    ODL_OBJEXIT(); //####
} // GraphicsPanel::setUpTetrahedron

void
GraphicsPanel::setUpTexture(void)
{
    ODL_OBJENTER(); //####
    const int size = 128;
    Image     image(Image::ARGB, size, size, true);
    Graphics  gg(image);
    
    gg.fillAll(Colours::white);
    gg.setColour(Colours::hotpink);
    gg.drawRect(0, 0, size, size, 2);
    gg.setColour(Colours::black);
    gg.setFont(40);
    gg.drawFittedText("m+m", image.getBounds(), Justification::centred, 1);
    _texture.loadImage(image);
    ODL_OBJEXIT(); //####
} // GraphicsPanel::setUpTexture

void
GraphicsPanel::updateFingerData(const HandData & leftHand,
                                const HandData & rightHand)
{
    ODL_OBJENTER(); //####
    ODL_P2("leftHand = ", &leftHand, "rightHand = ", &rightHand); //####
    _csect.enter();
    memcpy(&_leftHand, &leftHand, sizeof(_leftHand));
    memcpy(&_rightHand, &rightHand, sizeof(_rightHand));
    _csect.exit();
    ODL_OBJEXIT(); //####
} // GraphicsPanel::updateFingerData

void
GraphicsPanel::updateShader(void)
{
    ODL_OBJENTER(); //####
    if (_vertexShaderSource.isNotEmpty() || _fragmentShaderSource.isNotEmpty())
    {
        ScopedPointer<OpenGLShaderProgram> aProg(new OpenGLShaderProgram(_context));
        
        if (aProg->addVertexShader(_vertexShaderSource) &&
            aProg->addFragmentShader(_fragmentShaderSource) && aProg->link())
        {
            _shaderProgram = aProg;
            ODL_P1("_shaderProgram <- ", _shaderProgram); //####
            _shaderProgram->use();
            // Remember the vertex attributes
            _positionAttribute = createAttribute(_context, *_shaderProgram, "position");
            _normalAttribute = createAttribute(_context, *_shaderProgram, "normal");
            _sourceColourAttribute = createAttribute(_context, *_shaderProgram, "sourceColour");
            _textureCoordInAttribute = createAttribute(_context, *_shaderProgram, "texureCoordIn");
            ODL_P4("_positionAttribute <- ", _positionAttribute, "_normalAttribute <- ", //####
                   _normalAttribute, "_sourceColourAttribute <- ", _sourceColourAttribute, //####
                   "_textureCoordInAttribute <- ", _textureCoordInAttribute); //####
  
            // Remember the uniform attributes
            _offsetUniform = createUniform(_context, *_shaderProgram, "offset");
            _projectionMatrixUniform = createUniform(_context, *_shaderProgram, "projectionMatrix");
            _viewMatrixUniform = createUniform(_context, *_shaderProgram, "viewMatrix");
            _textureUniform = createUniform(_context, *_shaderProgram, "theTexture");
            _lightPositionUniform = createUniform(_context, *_shaderProgram, "lightPostion");
            ODL_P4("_projectionMatrixUniform <- ", _projectionMatrixUniform, //####
                   "_viewMatrixUniform <- ", _viewMatrixUniform, "_textureUniform <- ", //####
                   _textureUniform, "_lightPositionUniform <- ", _lightPositionUniform); //####
        }
        else
        {
            String lastError = aProg->getLastError();
            
            ODL_LOG("! (aProg->addVertexShader(_vertexShaderSource) && " //####
                    "aProg->addFragmentShader(_fragmentShaderSource) && aProg->link())"); //####
            ODL_S1s("lastError = ", lastError.toStdString()); //####
        }
        _vertexShaderSource = String::empty;
        _fragmentShaderSource = String::empty;
    }
    ODL_OBJEXIT(); //####
} // GraphicsPanel::updateShader

void
GraphicsPanel::zoomIn(void)
{
    ODL_OBJENTER(); //####
    _scale = static_cast<float>(_scale * 1.1);
    ODL_OBJEXIT(); //####
} // GraphicsPanel::zoomIn

void
GraphicsPanel::zoomOut(void)
{
    ODL_OBJENTER(); //####
    _scale = static_cast<float>(_scale * 0.9);
    ODL_OBJEXIT(); //####
} // GraphicsPanel::zoomOut

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
