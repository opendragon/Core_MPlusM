//--------------------------------------------------------------------------------------------------
//
//  File:       m+mGraphicsPanel.cpp
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

#include "m+mGraphicsPanel.hpp"
#include "m+mContentPanel.hpp"

#include <odl/ODEnableLogging.h>
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

using namespace LeapDisplay;
using namespace MplusM;
using namespace std;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief The first colour to be used for the panel background. */
static const Colour & kFirstBackgroundColour(Colours::darkgrey);

/*! @brief The second colour to be used for the panel background. */
static const Colour & kSecondBackgroundColour(Colours::lightgrey);

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

GraphicsPanel::GraphicsPanel(ContentPanel * theContainer,
                             const int      startingWidth,
                             const int      startingHeight) :
    inherited(), _container(theContainer), _dragConnectionActive(false), _dragIsForced(false)
{
    ODL_ENTER(); //####
    ODL_P1("theContainer = ", theContainer); //####
    ODL_LL2("startingWidth = ", startingWidth, "startingHeight = ", startingHeight); //####
    setSize((0 < startingWidth) ? startingWidth : kInitialPanelWidth,
            (0 < startingHeight) ? startingHeight : kInitialPanelHeight);
    setVisible(true);
    _context.setRenderer(this);
    _context.attachTo(*this);
    _context.setContinuousRepainting(true);
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
GraphicsPanel::clearDragInfo(void)
{
    ODL_OBJENTER(); //####
    _dragConnectionActive = _dragIsForced = false;
    ODL_OBJEXIT(); //####
} // GraphicsPanel::clearDragInfo

void
GraphicsPanel::clearOutData(void)
{
    ODL_OBJENTER(); //####
    removeAllChildren();
    ODL_OBJEXIT(); //####
} // GraphicsPanel::clearOutData

void
GraphicsPanel::drawBackground2DStuff(const float desktopScale)
{
    ODL_OBJENTER(); //####
    ODL_D1("desktopScale = ", desktopScale); //####
    ScopedPointer<LowLevelGraphicsContext>
                    glRenderer(createOpenGLGraphicsContext(_context,
                                                           roundToInt(desktopScale * getWidth()),
                                                           roundToInt(desktopScale * getHeight())));
    
    if (NULL != glRenderer)
    {
        Graphics gg(*glRenderer);
        
        if (_container->backgroundIsWhite())
        {
            gg.setFillType(_container->backgroundIsInverted() ? kFirstBackgroundColour :
                           kSecondBackgroundColour);
        }
        else
        {
            // Set up a gradient background, using a radial gradient from the centre to the furthest
            // edge.
            int   hh = getHeight();
            int   ww = getWidth();
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
} // GraphicsPanel::drawBackground2DStuff

void
GraphicsPanel::freeContextObjects(void)
{
    ODL_OBJENTER(); //####
    _shaderProgram = NULL;
    ODL_P1("_shaderProgram <- ", _shaderProgram); //####
    ODL_OBJEXIT(); //####
} // GraphicsPanel::freeContextObjects

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
void
GraphicsPanel::mouseDown(const MouseEvent & ee)
{
#if (! defined(OD_ENABLE_LOGGING_))
# if MAC_OR_LINUX_
#  pragma unused(ee)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING_)
    ODL_OBJENTER(); //####
    ODL_P1("ee = ", &ee); //####
    ODL_OBJEXIT(); //####
} // GraphicsPanel::mouseDown
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
void
GraphicsPanel::mouseUp(const MouseEvent & ee)
{
#if (! defined(OD_ENABLE_LOGGING_))
# if MAC_OR_LINUX_
#  pragma unused(ee)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING_)
    ODL_OBJENTER(); //####
    ODL_P1("ee = ", &ee); //####
    if (ee.mods.isPopupMenu())
    {
    }
    ODL_OBJEXIT(); //####
} // GraphicsPanel::mouseUp
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

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
        
        OpenGLHelpers::clear(Colours::lightblue); // FOR NOW
        _csect.enter();
        memcpy(&leftCopy, &_leftHand, sizeof(leftCopy));
        memcpy(&rightCopy, &_rightHand, sizeof(rightCopy));
        _csect.exit();
        drawBackground2DStuff(desktopScale);
        updateShader();
        if (NULL != _shaderProgram)
        {
#if 0
            glEnable (GL_DEPTH_TEST);
            glDepthFunc (GL_LESS);
            glEnable (GL_BLEND);
            glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
            openGLContext.extensions.glActiveTexture (GL_TEXTURE0);
            glEnable (GL_TEXTURE_2D);
            
            glViewport (0, 0, roundToInt (desktopScale * getWidth()), roundToInt (desktopScale * getHeight()));
            
            texture.bind();
            
            glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
            glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
            
            shader->use();
            
            if (uniforms->projectionMatrix != nullptr)
                uniforms->projectionMatrix->setMatrix4 (getProjectionMatrix().mat, 1, false);
            
            if (uniforms->viewMatrix != nullptr)
                uniforms->viewMatrix->setMatrix4 (getViewMatrix().mat, 1, false);
            
            if (uniforms->texture != nullptr)
                uniforms->texture->set ((GLint) 0);
            
            if (uniforms->lightPosition != nullptr)
                uniforms->lightPosition->set (-15.0f, 10.0f, 15.0f, 0.0f);
            
            if (uniforms->bouncingNumber != nullptr)
                uniforms->bouncingNumber->set (bouncingNumber.getValue());
            
            shape->draw (openGLContext, *attributes);
            
            // Reset the element buffers so child Components draw correctly
            openGLContext.extensions.glBindBuffer (GL_ARRAY_BUFFER, 0);
            openGLContext.extensions.glBindBuffer (GL_ELEMENT_ARRAY_BUFFER, 0);
            
            if (! controlsOverlay->isMouseButtonDown())
                rotation += (float) rotationSpeed;
#endif//0
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
    ODL_OBJEXIT(); //####
} // GraphicsPanel::resized

void
GraphicsPanel::setDragInfo(const Position position,
                           const bool     isForced)
{
    ODL_OBJENTER(); //####
    ODL_B1("isForced = ", isForced); //####
        _dragConnectionActive = true;
        _dragIsForced = isForced;
        _dragPosition = position;
    ODL_OBJEXIT(); //####
} // GraphicsPanel::setDragInfo

void
GraphicsPanel::updateFingerData(const HandData & leftHand,
                                const HandData & rightHand)
{
    ODL_OBJENTER(); //####
    ODL_P2("leftHand = ", &leftHand, "rightHand = ", &rightHand); //####
    _csect.enter();
    if (leftHand._thumb._valid)
    {
        memcpy(&_leftHand._thumb, &leftHand._thumb, sizeof(_leftHand._thumb));
    }
    if (leftHand._index._valid)
    {
        memcpy(&_leftHand._index, &leftHand._index, sizeof(_leftHand._index));
    }
    if (leftHand._middle._valid)
    {
        memcpy(&_leftHand._middle, &leftHand._middle, sizeof(_leftHand._middle));
    }
    if (leftHand._ring._valid)
    {
        memcpy(&_leftHand._ring, &leftHand._ring, sizeof(_leftHand._ring));
    }
    if (leftHand._pinky._valid)
    {
        memcpy(&_leftHand._pinky, &leftHand._pinky, sizeof(_leftHand._pinky));
    }
    if (rightHand._thumb._valid)
    {
        memcpy(&_rightHand._thumb, &rightHand._thumb, sizeof(_rightHand._thumb));
    }
    if (rightHand._index._valid)
    {
        memcpy(&_rightHand._index, &rightHand._index, sizeof(_rightHand._index));
    }
    if (rightHand._middle._valid)
    {
        memcpy(&_rightHand._middle, &rightHand._middle, sizeof(_rightHand._middle));
    }
    if (rightHand._ring._valid)
    {
        memcpy(&_rightHand._ring, &rightHand._ring, sizeof(_rightHand._ring));
    }
    if (rightHand._pinky._valid)
    {
        memcpy(&_rightHand._pinky, &rightHand._pinky, sizeof(_rightHand._pinky));
    }
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
 //TBD
            
            _shaderProgram = aProg;
            ODL_P1("_shaderProgram <- ", _shaderProgram); //####
            _shaderProgram->use();
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

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
