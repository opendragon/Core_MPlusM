//--------------------------------------------------------------------------------------------------
//
//  File:       m+mGraphicsFrame.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the drawing region of the primary window of the Leap Motion
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
//  Created:    2016-05-29
//
//--------------------------------------------------------------------------------------------------

#include "m+mGraphicsFrame.hpp"
#include "m+mGraphicsPanel.hpp"
#include "m+mLeapDisplayApplication.hpp"

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The class definition for the drawing region of the primary window of the %Leap Motion
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

/*! @brief The initial height of the displayed region. */
static const int kInitialPanelHeight = 512;

/*! @brief The initial width of the displayed region. */
static const int kInitialPanelWidth = 512;

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

GraphicsFrame::GraphicsFrame(ContentPanel * theContainer,
                             const int      startingWidth,
                             const int      startingHeight) :
    inherited(), _graphicsPanel(NULL), _container(theContainer)
{
    ODL_ENTER(); //####
    ODL_P1("theContainer = ", theContainer); //####
    ODL_LL2("startingWidth = ", startingWidth, "startingHeight = ", startingHeight); //####
    LeapDisplayApplication * ourApp = LeapDisplayApplication::getApp();
    
    setBounds(0, 0, (0 < startingWidth) ? startingWidth : kInitialPanelWidth,
              (0 < startingHeight) ? startingHeight : kInitialPanelHeight);
    _graphicsPanel = new GraphicsPanel(theContainer, getWidth(), getHeight());
    setVisible(true);
    if (ourApp)
    {
        ourApp->setGraphicsPanel(_graphicsPanel);
    }
    addAndMakeVisible(_graphicsPanel);
    ODL_EXIT_P(this); //####
} // GraphicsFrame::GraphicsFrame

GraphicsFrame::~GraphicsFrame(void)
{
    ODL_OBJENTER(); //####
    ODL_OBJEXIT(); //####
} // GraphicsFrame::~GraphicsFrame

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void
GraphicsFrame::resized(void)
{
    ODL_OBJENTER(); //####
    if (NULL != _graphicsPanel)
    {
        int                  offset = LookAndFeel::getDefaultLookAndFeel().getDefaultMenuBarHeight();
        juce::Rectangle<int> area(getLocalBounds());
        
        _graphicsPanel->setBounds(area);
        _graphicsPanel->setTopLeftPosition(0, offset);
    }
    ODL_OBJEXIT(); //####
} // GraphicsFrame::resized

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
