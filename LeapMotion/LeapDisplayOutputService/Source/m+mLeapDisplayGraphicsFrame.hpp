//--------------------------------------------------------------------------------------------------
//
//  File:       m+mLeapDisplayGraphicsFrame.hpp
//
//  Project:    m+m
//
//  Contains:   The class declaration for the drawing region of the primary window of the Leap
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
//  Created:    2016-05-29
//
//--------------------------------------------------------------------------------------------------

#if (! defined(mpmLeapDisplayGraphicsFrame_HPP_))
# define mpmLeapDisplayGraphicsFrame_HPP_ /* Header guard */

# include "m+mLeapDisplayDataTypes.hpp"

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 
 @brief The class declaration for the drawing region of the primary window of the %Leap Motion
 display output service application. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace LeapDisplay
{
    class ContentPanel;
    class GraphicsPanel;
    
    /*! @brief The drawing region of the main window of the application. */
    class GraphicsFrame : public Component
    {
    public :
    
    protected :
    
    private :
        
        /*! @brief The class that this class is derived from. */
        typedef Component inherited;
        
    public :
        
        /*! @brief The constructor.
         @param[in] container The container in which the panel is embedded.
         @param[in] startingWidth The initial width to use, or zero to use a default width.
         @param[in] startingHeight The initial height to use, or zero to uase a default height. */
        explicit
        GraphicsFrame(ContentPanel * container,
                      const int      startingWidth = 0,
                      const int      startingHeight = 0);
        
        /*! @brief The destructor. */
        virtual
        ~GraphicsFrame(void);
        
    protected :
    
    private :
        
        /*! @brief Called when the component size has been changed. */
        virtual void
        resized(void);
        
    public :
    
    protected :
    
    private :
        
        /*! @brief The graphics region. */
        ScopedPointer<GraphicsPanel> _graphicsPanel;

        /*! @brief The container in which the panel is embedded. */
        ContentPanel * _container;
        
        JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR(GraphicsFrame)
        
    }; // GraphicsFrame
    
} // LeapDisplay

#endif // ! defined(mpmLeapDisplayGraphicsFrame_HPP_)
